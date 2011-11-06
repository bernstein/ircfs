{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding ((.), id)
import Control.Category
import qualified Data.Lens.Common as L
import Control.Applicative
import qualified Data.ByteString.Char8 as B
import Foreign.C.Error
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO
import System.FilePath (splitFileName,takeBaseName)
import System.Environment (withArgs)
import System.Locale (defaultTimeLocale)
import qualified Data.Time as T
import qualified Data.Time.Format as T
import qualified System.Fuse as F
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (foldM_, foldM ,mapM_)
import Data.Maybe (maybe)
import qualified Control.Concurrent.Chan as C
import qualified Control.Concurrent as C
import Data.Attoparsec as A
import Data.Attoparsec.Enumerator as A
import qualified Network.Socket.Enumerator as E
import qualified Data.Enumerator as E hiding (drop)
import qualified Data.Enumerator.List as EL
import Control.Monad.State (get, put, modify)
import Data.Monoid
import qualified Data.IntMap as IM

import qualified Network.Socket as N hiding (recv)
import qualified Network.Socket.ByteString  as N (recv, sendAll)

import qualified Network.IRC.Message as I
import Network.IRC.Enumerator
import Ircfs.Ctl as I
import Ircfs.Types
import Ircfs.Filesystem
import qualified Ircfs.CmdLine as O
import System.Fuse.Request

--newtype IrcOut = IrcOut { unIrcOut :: C.Chan I.Message }
newtype IrcOut = IrcOut { unIrcOut :: C.Chan B.ByteString }
newtype IrcIn  = IrcIn { unIrcIn :: C.Chan I.Message }

-- process :: Enumeratee Request I.Message IO a

--process :: IrcfsState -> Request -> IO (Maybe B.ByteString, IrcfsState)
--process :: IrcOut -> IrcfsState -> Request -> IO IrcfsState
process :: IrcOut -> Request -> Ircfs ()
-- process file system requests
process ircoutc (ReqRep t m) = processTmsg ircoutc t >>= (io . C.putMVar m)
process ircoutc (Req t) = processTmsg ircoutc t >> return ()
process _ _ = return ()

-- /event -- new x #channel, new y user, del x #channel
--

-- ircoutc is actually not needed as an argument
-- process incoming irc messages

-- | Process incommint filesystem requests.
processTmsg :: IrcOut -> Tmsg -> Ircfs Rmsg
-- process Tread
processTmsg _ (Tread "/nick" byteCount offset) = do
  return . Rread . B.take (fromIntegral byteCount) 
        . B.drop (fromIntegral offset) . (`B.append` "\n") . nick
        . connection =<< get
processTmsg _ (Tread "/pong" byteCount offset) = do
  return . Rread . B.take (fromIntegral byteCount)
        . B.drop (fromIntegral offset) . pongFile . connection =<< get
processTmsg _ (Tread "/raw" byteCount offset) = do
  return . Rread . B.take (fromIntegral byteCount) 
        . B.drop (fromIntegral offset) . rawFile . connection =<< get
processTmsg _ (Tread "/event" byteCount offset) =
  return . Rread . B.take (fromIntegral byteCount) 
        . B.drop (fromIntegral offset) . eventFile . connection =<< get
processTmsg _ (Tread "/0/name" byteCount offset) = do
  return . Rread . B.take (fromIntegral byteCount)
        . B.drop (fromIntegral offset) . (`B.append` "\n"). addr
        . connection =<< get
processTmsg _ (Tread {}) = return Rerror

-- process Treaddir
processTmsg _ (Treaddir "/") = do
  ks <- (IM.keys . targets . connection) <$> get
  let ds = [(".", defaultDirStat) ,
            ("..", defaultDirStat), 
            ("0",defaultDirStat) ] ++ rootDir ++ subDirs
      rootDir = zip (map showFilepath rootDirFiles) (map fileStat rootDirFiles)
      subDirs = map (\x -> (show x,defaultDirStat)) ks
  return . Rreaddir $ ds
processTmsg _ (Treaddir _) = do
  -- let m = fromFilePath p
  let ds = [(".", defaultDirStat), ("..",defaultDirStat)] ++ subDir
      subDir = zip (map showFilepath subDirFiles) (map fileStat subDirFiles)
  return (Rreaddir ds)

-- process Twrite
processTmsg ircoutc (Twrite "/ctl" s offset) = do
  -- Todo if a msg is longer than 512 then split it into chunks
  maybe (return Rerror)
        (\c -> processTmsg ircoutc (Twrite "/raw" (I.toByteString.toMessage $c) 0)) (A.maybeResult $ A.parse I.parseCtl s)
  return . Rwrite . fromIntegral . B.length $ s
processTmsg _ (Twrite "/event" s offset) = do
  modify $ L.modL (eventLens.connectionLens) (`B.append` s)
  return . Rwrite . fromIntegral . B.length $ s
processTmsg _ (Twrite "/nick" s offset) = do
  modify $ L.setL (nickLens.connectionLens) s
  return . Rwrite . fromIntegral . B.length $ s
processTmsg _ (Twrite "/pong" s offset) = do
  modify $ L.modL (pongLens.connectionLens) (`B.append` s)
  return . Rwrite . fromIntegral . B.length $ s
processTmsg ircoutc (Twrite "/raw" s offset) = do
  appendRaw (">>>" `B.append` s)
  -- XXX writes to /raw are special,
  -- this part is the only reason processTmsg neds ircoutc
  io . C.writeChan (unIrcOut ircoutc) $ s
  return . Rwrite . fromIntegral . B.length $ s
processTmsg ircoutc (Twrite "/ircin" s offset) = do
  appendRaw ("<<<" `B.append` s `B.append` "\n")
  let m = A.maybeResult $ A.feed (A.parse I.message s) "\n"
  maybe (return ()) (processIrc ircoutc) m
  return . Rwrite . fromIntegral . B.length $ s
processTmsg _ (Twrite {}) = return Rerror

-- process Topen
processTmsg _ (Topen p _ _) = return Ropen

-- process Tstat
processTmsg _ (Tstat p) = maybe Rerror Rstat . (`stat` p) <$> get

-- | Process incommint irc messages.
processIrc :: IrcOut -> I.Message -> Ircfs ()
processIrc ircoutc (I.Message _ I.PING ps) = do
    stamp <- timeStamp
    let cmd = "pong " `B.append` head ps `B.append` "\n"
        off = fromIntegral . B.length $ cmd
        log = stamp `B.append` " " `B.append` cmd
    processTmsg ircoutc (Twrite "/ctl" cmd off)
    appendPong log
processIrc _ (I.Message (Just (I.PrefixNick n _ _)) I.NICK (new:_)) = do
    yourNick <- (nick.connection) <$> get
    if n == yourNick
      then do
        appendEvent $ "your nick changed to " `B.append` new `B.append` "\n"
        writeNick new
      else
        appendEvent $ n `B.append` " nick changed to " `B.append` new `B.append` "\n"
processIrc _ (I.Message _ I.ERROR ps) = return ()
processIrc _ (I.Message p I.JOIN (c:ps)) = do
  k <- nextDirName
  modify $ L.setL (targetLens k.connectionLens) (Just (Target k TChannel c [] mempty))
  modify $ L.setL (targetMapLens' c.connectionLens) (Just k)
  appendEvent . B.pack $ "new " ++ show k ++ " " ++ show c ++ "\n"
processIrc _ (I.Message p I.PART (c:ps)) = do
  m <- L.getL (targetMapLens' c.connectionLens) <$> get
  maybe (return ()) (\k -> do
      modify (L.setL (targetLens k.connectionLens) Nothing)
      modify (L.setL (targetMapLens' c.connectionLens) Nothing)
      freeDirName k
      appendEvent (B.pack ("del " ++ show k ++ " " ++ show c ++ "\n"))
    ) m
processIrc _ m = return ()

appendRaw :: B.ByteString -> Ircfs ()
appendRaw s = modify $ L.modL (rawLens.connectionLens) (`B.append` s)
appendEvent :: B.ByteString -> Ircfs ()
appendEvent s = modify $ L.modL (eventLens.connectionLens) (`B.append` s)
writeNick :: B.ByteString -> Ircfs ()
writeNick = modify . L.modL (nickLens.connectionLens) . const
appendPong :: B.ByteString -> Ircfs ()
appendPong s = modify $ L.modL (pongLens.connectionLens) (`B.append` s)

nextDirName :: Ircfs Int
nextDirName = do
  k <- (head . nextDirNames . connection) <$> get
  modify $ L.modL (nextDirNamesLens . connectionLens) tail
  return k

freeDirName :: Int -> Ircfs ()
freeDirName = modify . L.modL (nextDirNamesLens . connectionLens) . (:)

chanToIter2 :: C.Chan a -> E.Iteratee a IO ()
chanToIter2 chan = go
  where
    go = EL.head >>= maybe go (\x -> liftIO (C.writeChan chan x) >> go)

-- writeraw fsReq s = fuseRequest_ fsReq $ Twrite "/raw" s (B.length s)
-- writemsg

-- listens on the sockets, writes received messages to ircinc
ircReaderOld :: C.Chan Request -> N.Socket -> IrcIn -> IO ()
ircReaderOld fsReq sock ircinc = do
  _ <- E.run $ E.enumSocket 1024 sock E.$$ messages E.=$ chanToIter2 (unIrcIn ircinc)
  return ()

ircReader :: C.Chan Request -> N.Socket -> IO ()
ircReader fsReq socket =
  E.run_ $ E.enumSocket 1024 socket E.$$ irclines E.=$ iterFuseWriteFs_ fsReq

ircWriter :: N.Socket -> IrcOut -> IO ()
ircWriter s ircoutc = do
    ms <- C.getChanContents (unIrcOut ircoutc)
    --mapM_ (N.sendAll s . I.toByteString) ms
    mapM_ (N.sendAll s) ms

fsInit :: C.Chan Request -> O.Config -> IO ()
fsInit fsReq cfg = do
  inc <- C.newChan :: IO (C.Chan Request)
  ircoutc <- IrcOut <$> C.newChan
  s <- getSocket (O.addr cfg) (read (O.port cfg))

  let st = IrcfsState 
            { connection = 
                      Connection
                      { addr = B.pack . O.addr $ cfg
                      , nick = B.pack . O.nick $ cfg
                      , targets = mempty
                      , sock = s
                      , eventFile = ""
                      , pongFile = ""
                      , rawFile = ""
                      , nextDirNames = [1..100]
                      , targetMap = mempty
                      }
            -- , fsreq = fsReq 
            }

  _ <- C.forkIO $ ircReader fsReq s

  let nickMsg = B.pack $ "NICK " ++ O.nick cfg ++ "\r\n" 
  N.sendAll s nickMsg
  let userMsg = B.pack $ "USER none 0 * :" ++ O.nick cfg ++"\r\n"
  N.sendAll s userMsg
  _ <- C.forkIO $ ircWriter s ircoutc

  rs <- C.getChanContents fsReq
  _ <- C.forkIO $ runIrcfs st (mapM_ (process ircoutc) rs) >> return ()
  return ()

fsDestroy :: IO ()
fsDestroy = do
  return ()

main :: IO ()
main = N.withSocketsDo $ do
  args <- O.cmdLine
  fsReq <- C.newChan
  let ops = F.defaultFuseOps {
              F.fuseInit          = fsInit fsReq args
            , F.fuseDestroy       = fsDestroy
            , F.fuseGetFileStat   = fsStat fsReq
            , F.fuseReadDirectory = fsReadDir fsReq
            , F.fuseOpenDirectory = fsOpenDirectory
            , F.fuseOpen          = fsOpen fsReq
            , F.fuseRead          = fsRead fsReq
            , F.fuseWrite         = fsWrite fsReq
            , F.fuseSetFileSize   = fsTruncate
            }
  withArgs [O.mtpt args] $ F.fuseMain ops F.defaultExceptionHandler

fsOpenDirectory :: FilePath -> IO Errno
fsOpenDirectory  = const (return eOK)

fsTruncate :: FilePath -> FileOffset -> IO Errno
fsTruncate p _ = if takeBaseName p == "ctl" then return eOK else return eACCES

timeStamp :: MonadIO m => m B.ByteString
timeStamp = do
  now <- liftIO $ T.getCurrentTime
  return . B.pack $ (T.formatTime defaultTimeLocale "%H:%M" now)

foldMany :: Monad m => ([a] -> E.Iteratee a m b) -> E.Iteratee a m ()
foldMany f = E.continue step where
	step E.EOF = E.yield () E.EOF
	step (E.Chunks []) = E.continue step
	step (E.Chunks xs) = f xs >> E.continue step

iterFuseWriteFs_ :: MonadIO m => C.Chan Request -> E.Iteratee B.ByteString m ()
iterFuseWriteFs_ fsReq = foldMany (\xs -> liftIO $ writeMany_ fsReq xs)

writeMany_ :: C.Chan Request -> [B.ByteString] -> IO (C.Chan Request)
writeMany_ fsReq xs = foldM write_ fsReq xs
  where write_ c x = let off = fromIntegral (B.length x)
                     in  fuseRequest_ c (Twrite "/ircin" x off) >> return c

