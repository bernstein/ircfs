{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import qualified Data.ByteString.Char8 as B
import Foreign.C.Error
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO
import System.FilePath (splitFileName,takeBaseName)
import System.Environment (withArgs)
import qualified System.Fuse as F
import Control.Monad.IO.Class (liftIO)
import Control.Monad (foldM_)
import Data.Maybe (maybe)
import qualified Control.Concurrent.Chan as C
import qualified Control.Concurrent as C
import Data.Attoparsec as A
import Data.Attoparsec.Enumerator as A
import qualified Network.Socket.Enumerator as E
import qualified Data.Enumerator as E hiding (drop)
import qualified Data.Enumerator.List as EL
import Control.Monad.State (get, put)

import qualified Network.Socket as N hiding (recv)
import qualified Network.Socket.ByteString  as N (recv, sendAll)

import qualified Network.IRC.Message as I
import Ircfs.Ctl as I
import Ircfs.Types
import qualified Ircfs.CmdLine as O
import System.Fuse.Request

helloString = "it works!!!!!!!"

newtype IrcOut = IrcOut { unIrcOut :: C.Chan I.Message }
newtype IrcIn = IrcIn { unIrcIn :: C.Chan I.Message }

processFuseRequests :: IrcOut -> IrcfsState -> Request -> IO IrcfsState
processFuseRequests ircoutc st (ReqRep t m) = 
    fmap snd $ runIrcfs st $ processTmsg ircoutc t >>= (io . C.putMVar m)
processFuseRequests ircoutc st (Req t) = 
    fmap snd $ runIrcfs st $ processTmsg ircoutc t

processTmsg :: IrcOut -> Tmsg -> Ircfs Rmsg
-- process Tread
processTmsg _ (Tread "/nick" byteCount offset) = do
  st <- get
  let r = Rread
        . B.take (fromIntegral byteCount) 
        . B.drop (fromIntegral offset)
        . nick 
        . connection 
        $ st
  return r
processTmsg _ (Tread "/pong" byteCount offset) = do
  st <- get
  let r = Rread
        . B.take (fromIntegral byteCount) 
        . B.drop (fromIntegral offset)
        . pongFile 
        . connection 
        $ st
  return r
processTmsg _ (Tread "/raw" byteCount offset) = do
  st <- get
  let r = Rread
        . B.take (fromIntegral byteCount) 
        . B.drop (fromIntegral offset)
        . rawFile 
        . connection 
        $ st
  return r
processTmsg _ (Tread "/event" byteCount offset) = do
  st <- get
  let r = Rread
        . B.take (fromIntegral byteCount) 
        . B.drop (fromIntegral offset)
        . eventFile 
        . connection 
        $ st
  return r
processTmsg _ (Tread {}) = return Rerror

-- process Twrite
processTmsg ircoutc (Twrite "/ctl" s offset) = do
  st <- get
  let m = case A.parse I.parseCtl s of
         A.Fail {} -> Nothing
         A.Partial {} -> Nothing
         A.Done _ (I.Unknown {}) -> Nothing
         A.Done _ c -> Just (toMessage c)
  -- Todo if a msg is longer than 512 then split it into chunks
  maybe (return ()) (io . C.writeChan (unIrcOut ircoutc)) m
  let r = Rwrite . fromIntegral . B.length $ s
  return r
processTmsg ircoutc (Twrite "/event" s offset) = do
  st <- get
  -- XXX
  let c = connection st
  let c' = c { eventFile = eventFile c `B.append` s `B.append` "\n" }
  let st' = st {connection = c' }
  put st'
  let r = Rwrite . fromIntegral . B.length $ s
  return r
processTmsg ircoutc (Twrite "/pong" s offset) = do
  st <- get
  -- XXX
  let c = connection st
  let c' = c { pongFile = pongFile c `B.append` s `B.append` "\n" }
  let st' = st {connection = c' }
  put st'
  let r = Rwrite . fromIntegral . B.length $ s
  return r
processTmsg ircoutc (Twrite "/raw" s offset) = do
  st <- get
  -- XXX
  let c = connection st
  let c' = c { rawFile = rawFile c `B.append` s }
  let st' = st {connection = c' }
  put st'
  let r = Rwrite . fromIntegral . B.length $ s
  return r
processTmsg ircoutc (Twrite {}) = return Rerror

-- process Topen
processTmsg _ (Topen p _ _) = return Ropen
  -- | takeBaseName p == "ctl"  = return Rerror
  -- | otherwise   = return Ropen

-- process Tstat
processTmsg _ (Tstat p) = do
  st <- get
  let m = maybe Rerror Rstat (stat st p)
  return m

stat :: IrcfsState -> FilePath -> Maybe F.FileStat
stat (IrcfsState NotConnected) _ = Nothing
stat (IrcfsState con) "/nick" = 
  let s = (fileStat Qnick) { F.statFileSize = size }
      size = fromIntegral . B.length . nick $ con
  in  Just s
stat (IrcfsState con) "/event" = 
  let size = fromIntegral . B.length . eventFile $ con
  in  Just $ (fileStat Qevent) { F.statFileSize = size }
stat (IrcfsState con) "/raw" = 
  let size = fromIntegral . B.length . rawFile $ con
  in  Just $ (fileStat Qevent) { F.statFileSize = size }
stat (IrcfsState con) "/pong" = 
  let size = fromIntegral . B.length . pongFile $ con
  in  Just $ (fileStat Qevent) { F.statFileSize = size }
stat (IrcfsState con) p = fileStat <$> fromFilePath p

chanToIter2 :: C.Chan a -> E.Iteratee a IO ()
chanToIter2 chan = go
  where
    go = EL.head >>= maybe go (\x -> liftIO (C.writeChan chan x) >> go)

iterMessage :: Monad m => E.Iteratee B.ByteString m I.Message
iterMessage = A.iterParser I.message

messages :: Monad m => E.Enumeratee B.ByteString I.Message m a
messages = E.sequence iterMessage

--doIRC :: MonadIO m => IrcOut -> N.Socket -> I.Message -> m ()
-- ircoutc is actually not needed as an argument
-- TODO just write a PONG statement as System.Fuse.Request to Qrootctl
doIRC fsReq ircoutc (I.Message _ I.PING ps) = do
  let msg = (I.Message Nothing I.PONG ps)
      s = I.toByteString msg
      off = fromIntegral . B.length $ s
  liftIO $ C.writeChan (unIrcOut ircoutc) msg
  fuseRequest_ fsReq (Twrite "/pong" s off) 
doIRC fsReq ircoutc m = do
  let s = I.toByteString m
      off = fromIntegral . B.length $ s
  fuseRequest_ fsReq (Twrite "/raw" s off) 

-- listens on the sockets, writes received messages to ircinc
ircReader :: N.Socket -> IrcIn -> IO ()
ircReader sock ircinc = do
  x <- E.run $ E.enumSocket 1024 sock E.$$ messages E.=$ chanToIter2 (unIrcIn ircinc)
  return ()

ircWriter :: N.Socket -> IrcOut -> IO ()
ircWriter sock ircoutc = do
    ms <- C.getChanContents (unIrcOut ircoutc)
    mapM_ (N.sendAll sock . I.toByteString) ms

fsInit :: C.Chan Request -> O.Config -> IO ()
fsInit fsReq cfg = do
  ircoutc <- IrcOut <$> C.newChan
  addrinfos <- N.getAddrInfo Nothing (Just (O.addr cfg)) (Just (O.port cfg))
  let serveraddr = head addrinfos
  sock <- N.socket (N.addrFamily serveraddr) N.Stream N.defaultProtocol
  N.connect sock (N.addrAddress serveraddr)

  let st = IrcfsState 
            { connection = 
                      Connection
                      { addr = O.addr cfg
                      , nick = B.pack . O.nick $ cfg
                      , targets = []
                      , sock = sock
                      , eventFile = ""
                      , pongFile = ""
                      , rawFile = ""
                      }
            -- , fsreq = fsReq 
            }

  ircinc <- IrcIn <$> C.newChan
  C.forkIO $ ircReader sock ircinc

  let nickMsg = B.pack $ "NICK " ++ O.nick cfg ++ "\r\n" 
  N.sendAll sock nickMsg 
  let userMsg = B.pack $ "USER none 0 * :" ++ O.nick cfg ++"\r\n"
  N.sendAll sock userMsg
  C.forkIO $ ircWriter sock ircoutc

  ms <- C.getChanContents (unIrcIn ircinc)
  C.forkIO $ mapM_ (doIRC fsReq ircoutc) ms

  rs <- C.getChanContents fsReq
  C.forkIO $ foldM_ (processFuseRequests ircoutc) st rs
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

-- fuseInit :: FuseOperations fh -> IO ()
-- fuseOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno fh)
-- On success, returns Right of a filehandle-like value that will be passed to 
-- future file operations; on failure, returns Left of the appropriate Errno.
--
-- No creation, exclusive access or truncating flags will be passed. This 
-- should check that the operation is permitted for the given flags. 
--
-- fuseRead :: FilePath -> fh -> ByteCount -> FileOffset -> 
--                 IO (Either Errno ByteString)
--
-- fuseWrite :: FilePath -> fh -> ByteString -> FileOffset -> IO (Either Errno ByteCount)
-- fuseGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
-- implements statfs(2)
-- fuseOpenDirectory :: FilePath -> IO Errno
-- This method should check if the open operation is permitted for this directory. 
-- fuseReadDirectory :: FilePath -> IO (Either Errno [(FilePath, F.FileStat)])
-- fuseAccess :: FilePath -> Int -> IO Errno
-- fuseGetFileStat :: FilePath -> IO (Either Errno F.FileStat)
--   lstat
-- 

fsReadDir :: FilePath -> IO (Either Errno [(FilePath, F.FileStat)])
fsReadDir p@"/" = return . Right $ 
                    [(".", defaultDirStat) ,("..", defaultDirStat), ("0",defaultDirStat) ] ++ rootDir
  where
    rootDir = zip (map showFilepath rootDirFiles) (map fileStat rootDirFiles)
fsReadDir p = return . Right $ 
                    [(".", defaultDirStat), ("..",defaultDirStat)] ++ subDir
  where
    subDir = zip (map showFilepath subDirFiles) (map fileStat subDirFiles)
--fsReadDir _ = return (Left (eNOENT))

fsOpenDirectory :: FilePath -> IO Errno
fsOpenDirectory  "/" = return eOK
fsOpenDirectory  p = return eOK

{-
fsOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno IrcfsFH)
fsOpen p m f = 
  | p == "/ctl" = return (Right IrcfsFH)
  | p == "/nick" = return (Right IrcfsFH)
  | otherwise = return (Left eNOENT)
-}

fsTruncate :: FilePath -> FileOffset -> IO Errno
fsTruncate p off = if takeBaseName p == "ctl" then return eOK else return eACCES

showFile :: Qreq -> B.ByteString
showFile Qroot    = "/"
showFile Qrootctl = "ctl"
showFile Qevent   = "event"
showFile Qraw     = "raw"
showFile Qnick    = "nick"
showFile Qpong    = "pong"
showFile Qdir     = ""
showFile Qctl     = "ctl"
showFile Qname    = "name"
showFile Qusers   = "users"
showFile Qdata    = "data"

showFilepath :: Qreq -> FilePath
showFilepath = B.unpack . showFile

filemode :: Qreq -> FileMode
filemode Qroot    = 0o555 -- DIR
filemode Qrootctl = 0o222
filemode Qevent   = 0o444
filemode Qraw     = 0o666
filemode Qnick    = 0o444
filemode Qpong    = 0o444
filemode Qdir     = 0o555 -- DIR
filemode Qctl     = 0o222
filemode Qname    = 0o444
filemode Qusers   = 0o444
filemode Qdata    = 0o666

fileStat :: Qreq -> F.FileStat
fileStat Qroot    = defaultDirStat  { F.statFileMode = filemode Qroot }
fileStat Qrootctl = defaultFileStat { F.statFileMode = filemode Qrootctl }
fileStat Qevent   = defaultFileStat { F.statFileMode = filemode Qevent }
fileStat Qraw     = defaultFileStat { F.statFileMode = filemode Qraw }
fileStat Qnick    = defaultFileStat { F.statFileMode = filemode Qnick }
fileStat Qpong    = defaultFileStat { F.statFileMode = filemode Qpong }
fileStat Qdir     = defaultDirStat  { F.statFileMode = filemode Qroot }
fileStat Qctl     = defaultFileStat { F.statFileMode = filemode Qrootctl }
fileStat Qname    = defaultFileStat { F.statFileMode = filemode Qname }
fileStat Qusers   = defaultFileStat { F.statFileMode = filemode Qusers }
fileStat Qdata    = defaultFileStat { F.statFileMode = filemode Qdata }

toQreq :: String -> Maybe Qreq
toQreq "ctl"   = Just Qctl
toQreq "event" = Just Qevent
toQreq "nick"  = Just Qnick
toQreq "raw"   = Just Qraw
toQreq "pong"  = Just Qpong
toQreq "data"  = Just Qdata
toQreq "name"  = Just Qname
toQreq "users" = Just Qusers
toQreq _       = Nothing

fromFilePath :: FilePath -> Maybe Qreq
fromFilePath "/"    = Just Qroot
fromFilePath "/ctl" = Just Qrootctl
fromFilePath "/0"   = Just Qdir
fromFilePath p      = toQreq . snd . splitFileName $ p

rootDirFiles :: [Qreq]
rootDirFiles = [Qrootctl, Qevent, Qnick, Qraw, Qpong]

subDirFiles :: [Qreq]
subDirFiles = [Qctl, Qdata, Qname, Qusers]


files :: [Qreq]
files = [ Qroot, Qrootctl, Qevent, Qraw, Qnick, Qpong, Qdir, Qctl, Qname
        , Qusers, Qdata
        ]

--connect :: -> IO Connection
--
