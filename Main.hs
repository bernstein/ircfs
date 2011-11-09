{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
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
import Control.Monad (foldM_, foldM, mapM_, when)
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
import qualified System.Fuse.Request as F

--newtype IrcOut = IrcOut { unIrcOut :: C.Chan I.Message }
newtype IrcOut = IrcOut { unIrcOut :: C.Chan B.ByteString }
newtype IrcIn  = IrcIn { unIrcIn :: C.Chan I.Message }

-- process :: Enumeratee F.Request I.Message IO a

--process :: IrcfsState -> F.Request -> IO (Maybe B.ByteString, IrcfsState)
--process :: IrcOut -> IrcfsState -> F.Request -> IO IrcfsState
process :: IrcOut -> F.Request -> Ircfs ()
process o (F.ReqRep t m) = processTmsg o t >>= (io . C.putMVar m)
process o (F.Req t)      = processTmsg o t >> return ()

-- | Process incoming filesystem requests.
processTmsg :: IrcOut -> F.Tmsg -> Ircfs F.Rmsg
-- process Tread
processTmsg _ (F.Tread p bc off) = do
  st <- get
  maybe (return F.Rerror) (return . F.Rread) (readF st p bc off)
processTmsg _ (F.Tread {}) = return F.Rerror

-- process Treaddir
processTmsg _ (F.Treaddir "/") = do
  ks <- (IM.keys . targets . connection) <$> get
  let ds = [(".", F.defaultDirStat) ,
            ("..", F.defaultDirStat), 
            ("0",F.defaultDirStat) ] ++ rootDir ++ subDirs
      rootDir = map (showFilepath &&& fileStat) rootDirFiles
      subDirs = map (\x -> (show x,F.defaultDirStat)) ks
  return . F.Rreaddir $ ds
processTmsg _ (F.Treaddir _) = do
  -- let m = fromFilePath p
  let ds = [(".", F.defaultDirStat), ("..",F.defaultDirStat)] ++ subDir
      subDir = map (showFilepath &&& fileStat) subDirFiles
  return (F.Rreaddir ds)

-- process Twrite, usually appends to files
processTmsg ircoutc (F.Twrite "/ctl" s offset) = do
  -- Todo if a msg is longer than 512 then split it into chunks
  maybe 
    (return F.Rerror)
    (\c -> processTmsg ircoutc (F.Twrite "/raw" (I.toByteString.toMessage $c) 0))
    (A.maybeResult $ A.parse I.parseCtl s)
  return . F.Rwrite . fromIntegral . B.length $ s
processTmsg _ (F.Twrite "/event" s _) = do
  appendEvent s
  return . F.Rwrite . fromIntegral . B.length $ s
processTmsg _ (F.Twrite "/nick" s _) = do
  writeNick s
  return . F.Rwrite . fromIntegral . B.length $ s
processTmsg _ (F.Twrite "/pong" s _) = do
  appendPong s
  return . F.Rwrite . fromIntegral . B.length $ s
processTmsg ircoutc (F.Twrite "/raw" s _) = do
  appendRaw (">>>" `B.append` s)
  io . C.writeChan (unIrcOut ircoutc) $ s
  return . F.Rwrite . fromIntegral . B.length $ s
processTmsg ircoutc (F.Twrite "/ircin" s _) = do
  appendRaw ("<<<" `B.append` s `B.append` "\n")
  let m = A.maybeResult $ A.feed (A.parse I.message s) "\n"
  maybe (return ()) (processIrc ircoutc) m
  return . F.Rwrite . fromIntegral . B.length $ s
processTmsg _ (F.Twrite {}) = return F.Rerror

-- process Topen
processTmsg _ F.Topen {} = return F.Ropen

-- process Tstat
processTmsg _ (F.Tstat p) = maybe F.Rerror F.Rstat . (`stat` p) <$> get

-- | Process incommint irc messages.
processIrc :: IrcOut -> I.Message -> Ircfs ()
processIrc ircoutc (I.Message _ I.PING ps) = do
    stamp <- timeStamp
    let cmd = "pong " `B.append` head ps `B.append` "\n"
        off = fromIntegral . B.length $ cmd
        log = stamp `B.append` " " `B.append` cmd
    _ <- processTmsg ircoutc (F.Twrite "/ctl" cmd off)
    appendPong log
processIrc _ (I.Message (Just (I.PrefixNick n _ _)) I.NICK (new:_)) = do
  yourNick <- (nick.connection) <$> get
  if n == yourNick
    then do
      appendEvent $ "your nick changed to " `B.append` new `B.append` "\n"
      writeNick new
    else
      appendEvent $ n `B.append` " changed nick to " 
                      `B.append` new 
                      `B.append` "\n"
processIrc _ (I.Message (Just (I.PrefixNick n _ _)) I.JOIN (c:_)) = do
  yourNick <- (nick.connection) <$> get
  when (n == yourNick) $ do
    k <- nextDirName
    modify $ L.setL (targetLens k.connectionLens) 
                    (Just (Target k TChannel c mempty mempty))
    modify $ L.setL (targetMapLens' c.connectionLens) (Just k)
    let s = B.pack $ "new " ++ show k ++ " "
    appendEvent $ s `B.append` c `B.append` "\n"
processIrc _ (I.Message (Just (I.PrefixNick n _ _)) I.PART (c:_)) = do
  yourNick <- (nick.connection) <$> get
  when (n == yourNick) $ do
    m <- L.getL (targetMapLens' c.connectionLens) <$> get
    maybe (return ()) (\k -> do
        modify (L.setL (targetLens k.connectionLens) Nothing)
        modify (L.setL (targetMapLens' c.connectionLens) Nothing)
        freeDirName k
        let s = B.pack $ "del " ++ show k ++ " "
        appendEvent (s `B.append` c `B.append` "\n")
      ) m
processIrc _ (I.Message (Just (I.PrefixNick n _ _)) I.PRIVMSG (c:cs)) = do
  stamp <- timeStamp
  tm <- L.getL (targetMapLens' c.connectionLens) <$> get
  maybe (return ()) (\k -> do
      modify $ L.modL (targetLens k.connectionLens) 
                      (fmap (L.modL textLens (\t -> t `B.append` stamp `B.append` " < " `B.append` n `B.append` "> " `B.append` B.unwords cs `B.append` "\n")))
      ) tm
  return ()
processIrc _ m@(I.Message _ I.PRIVMSG _) = do
  stamp <- timeStamp
  tm <- L.getL (targetMapLens' c.connectionLens) <$> get
  let s = I.toByteString m
  appendEvent $ stamp `B.append` " error: processIrc PRIVMSG " `B.append` s `B.append` "\n"
  return ()
processIrc _ m@(I.Message _ I.ERROR _) =
  appendEvent ("error " `B.append` I.toByteString m `B.append` "\n")
processIrc _ _ = return ()

nextDirName :: Ircfs Int
nextDirName = do
  k <- (head . nextDirNames . connection) <$> get
  modify $ L.modL (nextDirNamesLens . connectionLens) tail
  return k

freeDirName :: Int -> Ircfs ()
freeDirName = modify . L.modL (nextDirNamesLens . connectionLens) . (:)

chanToIter2 :: C.Chan a -> E.Iteratee a IO ()
chanToIter2 c = go
  where
    go = EL.head >>= maybe go (\x -> liftIO (C.writeChan c x) >> go)

-- | Listens on the sockets, writes received messages to ircinc
ircReader :: C.Chan F.Request -> N.Socket -> IO ()
ircReader fsReq socket =
  E.run_ $ E.enumSocket 1024 socket E.$$ irclines E.=$ iterFuseWriteFs_ fsReq

ircWriter :: N.Socket -> IrcOut -> IO ()
ircWriter s out = mapM_ (N.sendAll s) =<< C.getChanContents (unIrcOut out) 

-- | Initialize the filesystem.
fsInit :: C.Chan F.Request -> O.Config -> IO ()
fsInit fsReq cfg = do
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
            }

  _ <- C.forkIO $ ircReader fsReq s

  let nickMsg = B.pack $ "NICK " ++ O.nick cfg ++ "\r\n" 
  N.sendAll s nickMsg
  let userMsg = B.pack $ "USER none 0 * :" ++ O.nick cfg ++"\r\n"
  N.sendAll s userMsg
  ircoutc <- IrcOut <$> C.newChan
  _ <- C.forkIO $ ircWriter s ircoutc

  rs <- C.getChanContents fsReq
  _ <- C.forkIO $ runIrcfs st (mapM_ (process ircoutc) rs) >> return ()
  return ()

fsDestroy :: IO ()
fsDestroy = return ()

main :: IO ()
main = N.withSocketsDo $ do
  args <- O.cmdLine
  fsReq <- C.newChan
  let ops = F.defaultFuseOps {
              F.fuseInit          = fsInit fsReq args
            , F.fuseDestroy       = fsDestroy
            , F.fuseGetFileStat   = F.fsStat fsReq
            , F.fuseReadDirectory = F.fsReadDir fsReq
            , F.fuseOpenDirectory = fsOpenDirectory
            , F.fuseOpen          = F.fsOpen fsReq
            , F.fuseRead          = F.fsRead fsReq
            , F.fuseWrite         = F.fsWrite fsReq
            , F.fuseSetFileSize   = fsTruncate
            }
  withArgs [O.mtpt args] $ F.fuseMain ops F.defaultExceptionHandler

fsOpenDirectory :: FilePath -> IO Errno
fsOpenDirectory  = const (return eOK)

fsTruncate :: FilePath -> FileOffset -> IO Errno
fsTruncate p _ = if takeBaseName p == "ctl" then return eOK else return eACCES

timeStamp :: MonadIO m => m B.ByteString
timeStamp = do
  now <- liftIO T.getCurrentTime
  return . B.pack $ T.formatTime defaultTimeLocale "%H:%M" now

foldMany :: Monad m => ([a] -> E.Iteratee a m b) -> E.Iteratee a m ()
foldMany f = E.continue step where
	step E.EOF = E.yield () E.EOF
	step (E.Chunks []) = E.continue step
	step (E.Chunks xs) = f xs >> E.continue step

iterFuseWriteFs_ :: MonadIO m => C.Chan F.Request -> E.Iteratee B.ByteString m ()
iterFuseWriteFs_ fsReq = foldMany (liftIO . writeMany_ fsReq)

writeMany_ :: C.Chan F.Request -> [B.ByteString] -> IO (C.Chan F.Request)
writeMany_ fsReq xs = foldM write_ fsReq xs
  where write_ c x = let off = fromIntegral (B.length x)
                     in  F.fuseRequest_ c (F.Twrite "/ircin" x off) >> return c

