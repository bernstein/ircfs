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
import Control.Monad (foldM_,mapM_)
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

import qualified Network.Socket as N hiding (recv)
import qualified Network.Socket.ByteString  as N (recv, sendAll)

import qualified Network.IRC.Message as I
import Network.IRC.Enumerator
import Ircfs.Ctl as I
import Ircfs.Types
import Ircfs.Filesystem
import qualified Ircfs.CmdLine as O
import System.Fuse.Request

data In  = Fs  Request
         | Cmd CtlCommand
         | Irc I.Message
         | Shutdown
         -- deriving (Eq)

--newtype IrcOut = IrcOut { unIrcOut :: C.Chan I.Message }
newtype IrcOut = IrcOut { unIrcOut :: C.Chan B.ByteString }
newtype IrcIn  = IrcIn { unIrcIn :: C.Chan I.Message }

-- process :: Enumeratee In I.Message IO a

--process :: IrcfsState -> In -> IO (Maybe B.ByteString, IrcfsState)
--process :: IrcOut -> IrcfsState -> In -> IO IrcfsState
process :: IrcOut -> In -> Ircfs ()
-- process file system requests
process ircoutc (Fs (ReqRep t m)) = processTmsg ircoutc t >>= (io . C.putMVar m)
process ircoutc (Fs (Req t)) = processTmsg ircoutc t >> return ()

-- /event -- new x #channel, new y user, del x #channel
--

--doIRC :: MonadIO m => IrcOut -> N.Socket -> I.Message -> m ()
-- ircoutc is actually not needed as an argument
-- TODO just write a PONG statement as System.Fuse.Request to Qrootctl
--doIRC fsReq ircoutc (I.Message p I.NOTICE ps) = undefined
--doIRC fsReq ircoutc (I.Message p I.PRIVMSG ps) = undefined
--doIRC fsReq ircoutc (I.Message p I.NICK ps) = undefined
--doIRC fsReq ircoutc (I.Message p I.MODE ps) = undefined
--doIRC fsReq ircoutc (I.Message p I.QUIT ps) = undefined
--doIRC fsReq ircoutc (I.Message p I.ERROR ps) = undefined
--doIRC fsReq ircoutc (I.Message p I.SQUIT ps) = undefined
--doIRC fsReq ircoutc (I.Message p I.JOIN ps) = undefined
--doIRC fsReq ircoutc (I.Message p I.PART ps) = undefined
--doIRC fsReq ircoutc (I.Message p I.KICK ps) = undefined
--doIRC fsReq ircoutc (I.Message p I.TOPIC ps) = undefined
--doIRC fsReq ircoutc (I.Message p I.INVITE ps) = undefined
-- process incoming irc messages
process ircoutc (Irc m@(I.Message _ I.PING ps)) = do
    stamp <- timeStamp
    let cmd = "pong " `B.append` head ps `B.append` "\n"
        off = fromIntegral . B.length $ cmd
        log = stamp `B.append` " " `B.append` cmd
        s = I.toByteString m
        off2 = fromIntegral . B.length $ s
    processTmsg ircoutc (Twrite "/ctl" cmd off)
    processTmsg ircoutc (Twrite "/pong" log off)
    writeRaw s off2
process ircoutc (Irc m@(I.Message (Just (I.PrefixNick n _ _)) I.NICK (new:_))) = do
    st <- get
    if n == nick (connection st)
      then do
        let log = "your nick changed\n"
            off = fromIntegral . B.length $ log
        processTmsg ircoutc (Twrite "/event" log off) >> return ()
        processTmsg ircoutc (Twrite "/nick" new (fromIntegral . B.length $ new))
        return ()
      else do
        let log = "someones nick changed\n"
            off = fromIntegral . B.length $ log
        processTmsg ircoutc (Twrite "/event" log off)
        return ()
    let s = I.toByteString m
        off2 = fromIntegral . B.length $ s
    writeRaw s off2

process ircoutc (Irc m@(I.Message _ I.ERROR ps)) =
    let s = I.toByteString m
        off = fromIntegral . B.length $ s
    in writeRaw s off
process ircoutc (Irc m) =
    let s = I.toByteString m
        off = fromIntegral . B.length $ s
    in writeRaw s off
process _ _ = return ()

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
  modify $ L.modL (rawLens.connectionLens) (`B.append` s)
  -- XXX writes to /raw are special,
  -- this part is the only reason processTmsg neds ircoutc
  io . C.writeChan (unIrcOut ircoutc) $ s
  return . Rwrite . fromIntegral . B.length $ s
processTmsg _ (Twrite {}) = return Rerror

-- process Topen
processTmsg _ (Topen p _ _) = return Ropen

-- process Tstat
processTmsg _ (Tstat p) = maybe Rerror Rstat . (`stat` p) <$> get

writeRaw :: B.ByteString -> Int -> Ircfs ()
writeRaw s off = modify $ L.modL (rawLens.connectionLens) (`B.append` s)

chanToIter2 :: C.Chan a -> E.Iteratee a IO ()
chanToIter2 chan = go
  where
    go = EL.head >>= maybe go (\x -> liftIO (C.writeChan chan x) >> go)

-- writeraw fsReq s = fuseRequest_ fsReq $ Twrite "/raw" s (B.length s)
-- writemsg

-- listens on the sockets, writes received messages to ircinc
ircReader :: N.Socket -> IrcIn -> IO ()
ircReader sock ircinc = do
  x <- E.run $ E.enumSocket 1024 sock E.$$ messages E.=$ chanToIter2 (unIrcIn ircinc)
  return ()

ircWriter :: N.Socket -> IrcOut -> IO ()
ircWriter s ircoutc = do
    ms <- C.getChanContents (unIrcOut ircoutc)
    --mapM_ (N.sendAll s . I.toByteString) ms
    mapM_ (N.sendAll s) ms

fsInit :: C.Chan Request -> O.Config -> IO ()
fsInit fsReq cfg = do
  inc <- C.newChan :: IO (C.Chan In)
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
                      }
            -- , fsreq = fsReq 
            }

  ircinc <- IrcIn <$> C.newChan
  C.forkIO $ ircReader s ircinc

  let nickMsg = B.pack $ "NICK " ++ O.nick cfg ++ "\r\n" 
  N.sendAll s nickMsg
  let userMsg = B.pack $ "USER none 0 * :" ++ O.nick cfg ++"\r\n"
  N.sendAll s userMsg
  C.forkIO $ ircWriter s ircoutc

  ms <- C.getChanContents (unIrcIn ircinc)
  C.forkIO $ foldM_ (\c m -> C.writeChan c (Irc m) >> return c) inc ms

  rs <- C.getChanContents fsReq
  C.forkIO $ foldM_ (\c r -> C.writeChan c (Fs r) >> return c) inc rs

  is <- C.getChanContents inc
  C.forkIO $ runIrcfs st (mapM_ (process ircoutc) is) >> return ()
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
            , F.fuseReadDirectory = fsReadDir
            , F.fuseOpenDirectory = fsOpenDirectory
            , F.fuseOpen          = fsOpen fsReq
            , F.fuseRead          = fsRead fsReq
            , F.fuseWrite         = fsWrite fsReq
            , F.fuseSetFileSize   = fsTruncate
            }
  withArgs [O.mtpt args] $ F.fuseMain ops F.defaultExceptionHandler

fsReadDir :: FilePath -> IO (Either Errno [(FilePath, F.FileStat)])
fsReadDir "/" = return . Right $
                    [(".", defaultDirStat) ,("..", defaultDirStat), ("0",defaultDirStat) ] ++ rootDir
  where
    rootDir = zip (map showFilepath rootDirFiles) (map fileStat rootDirFiles)
fsReadDir _ = return . Right $
                    [(".", defaultDirStat), ("..",defaultDirStat)] ++ subDir
  where
    subDir = zip (map showFilepath subDirFiles) (map fileStat subDirFiles)
--fsReadDir _ = return (Left (eNOENT))

fsOpenDirectory :: FilePath -> IO Errno
fsOpenDirectory  = const (return eOK)

fsTruncate :: FilePath -> FileOffset -> IO Errno
fsTruncate p _ = if takeBaseName p == "ctl" then return eOK else return eACCES

timeStamp :: MonadIO m => m B.ByteString
timeStamp = do
  now <- liftIO $ T.getCurrentTime
  return . B.pack $ (T.formatTime defaultTimeLocale "%H:%M" now)

