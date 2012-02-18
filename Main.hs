{-# LANGUAGE OverloadedStrings, BangPatterns #-}
-- -----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Andreas-Christoph Bernstein 2011
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  andreas.bernstein@googlemail.com
-- Stability   :  unstable
-- Portability :  not portable
--
--------------------------------------------------------------------------------
module Main
where

import Prelude hiding ((.), id)
import Control.Category
import Control.Applicative
import Data.Traversable (sequenceA)
import qualified Data.ByteString.Char8 as B
import Foreign.C.Error
import Foreign.C.Types (CTime)
import qualified Data.Time as T
import qualified Data.Time.Clock.POSIX as T
import System.Posix.Types
import qualified System.Posix.User as S
import System.FilePath (takeBaseName)
import System.Environment (withArgs)
import qualified System.Fuse as F
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (foldM, when)
import qualified Control.Concurrent.Chan as C
import qualified Control.Concurrent as C
import qualified Control.Exception as E
import qualified Network.Socket.ByteString.Lazy as NL (sendAll,getContents)
import           Data.Monoid
import           Data.Either
import           Data.IORef
import           Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Lens.Common as L

import qualified Network.Socket as N
import qualified Network.Socket.ByteString  as N (sendAll)
import qualified Data.ByteString.Lazy as BL hiding (elemIndex,head)
import qualified Data.ByteString.Lazy.Char8 as BL hiding (map)
import qualified Data.Attoparsec.Lazy as AL

import Ircfs.Types
import Ircfs.Process
import Ircfs.Inode
import Ircfs.Filesystem
import Ircfs.Ctl as I
import Ircfs.Misc
import qualified Ircfs.CmdLine as O
import Data.Attoparsec as A
import qualified Network.IRC.Message as I

-- | Initialize file system.
fsInit :: IORef Fs -> O.Config -> IO ()
fsInit ref cfg = do
  let work r = either (err r) (sequence_ . sequenceA [ircin r, pong r, onError r])
      f s t = event t (mconcat ["could not parse: ",B.pack s,"\n"])
      err r s = atomicModifyIORef_ r . f s =<< now
  thr <- C.forkIO (mapM_ (work ref) =<< C.getChanContents =<< (inChan <$> readIORef ref))
  doConnect ref (B.pack (O.addr cfg)) (B.pack (O.nick cfg))

main :: IO ()
main = N.withSocketsDo $ do
  args <- O.cmdLine
  ref <- newIORef =<< newFS (B.pack . O.addr $ args) (B.pack . O.nick $ args)
  ircoutc <- IrcOut <$> C.newChan
  let ops = F.defaultFuseOps {
              F.fuseInit          = fsInit ref args
            , F.fuseDestroy       = disconnect ref
            , F.fuseGetFileStat   = fsStat ref
            , F.fuseReadDirectory = fsReadDir ref
            , F.fuseOpenDirectory = fsOpenDirectory ref
            , F.fuseOpen          = fsOpen ref
            , F.fuseRead          = fsRead ref
            , F.fuseWrite         = fsWrite ref
            , F.fuseSetFileSize   = fsTruncate
            }
-- XXX
  --withArgs [O.mtpt args, "-d"] $ F.fuseMain ops F.defaultExceptionHandler
  withArgs [O.mtpt args] $ F.fuseMain ops F.defaultExceptionHandler

fsStat :: IORef Fs -> FilePath -> IO (Either Errno F.FileStat)
fsStat ref p
 = do
    st <- readIORef ref
    let ms = stat st =<< parsePath p
    return $ maybe (Left F.eNOENT) Right ms

fsOpenDirectory :: IORef Fs -> FilePath -> IO Errno
fsOpenDirectory ref p = do
  fs <- readIORef ref
  return . fromMaybe eNOENT . fmap (f fs) . parsePath $ p
  where
    f fs p = if exists p fs && isDir p then eOK else eNOENT

fsTruncate :: FilePath -> FileOffset -> IO Errno
fsTruncate p _ = if takeBaseName p == "ctl" then return eOK else return eACCES

newFS :: B.ByteString -> FileData -> IO Fs
newFS a n = do
  uid <- fromIntegral <$> S.getEffectiveUserID
  gid <- fromIntegral <$> S.getEffectiveGroupID
  name <- S.getEffectiveUserName
  time <- now
  tz <- T.getCurrentTimeZone
  inc <- C.newChan
  let st = Fs 
          { addr = a
          , targets = mempty
          , targetMap = mempty
          , userID = uid
          , groupID = gid
          , effectiveUserName = name
          , inodes = mempty
          , start = time
          , timeZone = tz
          , inChan = inc
          , connection = Disconnected
          }
      emptyNode = setTimes time . chmod 0o440 $ mkInode (defaultFileStat st)
      rwNode = chmod 0o660 emptyNode
      wNode = chmod 0o220 emptyNode
      nickNode = L.setL iDataL n emptyNode
      dirNode = mkInode (defaultDirStat st)
      insert = 
          L.setL (inodeL Qevent) (Just emptyNode)
        . L.setL (inodeL Qpong) (Just emptyNode)
        . L.setL (inodeL Qraw) (Just rwNode)
        . L.setL (inodeL Qnick) (Just nickNode)
        . L.setL (inodeL Qrootctl) (Just wNode)
        . L.setL (inodeL Qroot) (Just dirNode)
        . insertChannel a time
  return (insert st)

fsReadDir :: IORef Fs -> FilePath
          -> IO (Either Errno [(FilePath, F.FileStat)])
fsReadDir ref p = do
  st <- readIORef ref
  let ds = readDir' st <$> parsePath p
  return $ maybe (Left F.eNOENT) Right ds

fsOpen :: IORef Fs -> FilePath -> F.OpenMode -> F.OpenFileFlags 
        -> IO (Either Errno FH)
fsOpen ref p _ _ = do
  st <- readIORef ref
  return $ maybe (Left F.eACCES)
          (\p' -> if exists p' st then Right FH else Left F.eACCES)
          (parsePath p)

exists :: Qreq -> Fs -> Bool
exists p = M.member p . inodes

isDir :: Qreq -> Bool
isDir Qroot = True
isDir (Qdir {}) = True
isDir _ = False

fsRead :: IORef Fs -> FilePath -> FH -> ByteCount ->
            FileOffset -> IO (Either Errno B.ByteString)
fsRead ref p _ bc off = do
  st <- readIORef ref
  let s = readF st p bc off
  return $ maybe (Left F.eNOENT) Right s

fsRelease :: IORef Fs -> FilePath -> FH -> IO ()
fsRelease _ _ _ = return ()

-- write to ctj
-- Todo if a msg is longer than 512 then split it into chunks
    -- XXX TODO: process ctl
    -- mR >>= processCtl
-- XXX
fsWrite :: IORef Fs -> FilePath -> FH -> B.ByteString 
        -> FileOffset -> IO (Either Errno ByteCount)
fsWrite ref "/ctl" fh s off = do
  let mR = A.maybeResult $ A.parse I.parseCtl s
  case mR of
    Just I.Disconnect -> disconnect ref 
    Just (I.Connect s n) -> doConnect ref s n
    _ -> maybe (return ()) (rawSend ref . I.encode) (toMessage =<< mR)
  return (Right (fromIntegral (B.length s)))
fsWrite ref "/raw" _ s _ = do
  rawSend ref s
  return (Right (fromIntegral (B.length s)))
fsWrite ref p fh s off = do
  st <- readIORef ref
  stamp <- timeStamp
  time <- now
-- XXX
  let x = write st stamp s <$> parsePath p
  case x of
-- XXX
    Just (st', []) -> atomicModifyIORef_ ref (const st')
    Just (st', msg:_) -> do
      atomicModifyIORef_ ref (const st')
      rawSend ref (I.encode msg)
    _ -> return ()
  return . Right . fromIntegral . B.length $ s
fsWrite _ _ _ _ _ = return (Left F.eNOENT)

ircin :: IORef Fs -> I.Message -> IO ()
ircin ref m = atomicModifyIORef_ ref . f =<< now
  where f t = processIrc t m . touch Qraw t 
          . append Qraw ("<<<" `mappend` I.encode m)

pong :: IORef Fs -> I.Message -> IO ()
pong ref (I.Message _ I.PING (I.Params _ (Just p))) =
  let time t st = B.pack $ stamp' (timeZone st) t
      text t st = mconcat [time t st," pong ",p,"\n"]
      msg = I.encode $ I.Message Nothing I.PONG (I.Params [p] Nothing)
      f t st = touch Qpong t . append Qpong (text t st) $ st
  in do
    rawSend ref msg
    atomicModifyIORef_ ref . f =<< now
pong _ _ = return ()

onError :: IORef Fs -> I.Message -> IO ()
onError ref (I.Message _ I.ERROR _) = disconnect ref
onError _ _ = return ()

disconnect :: IORef Fs -> IO ()
disconnect ref = do
  print "disconnect"
  rawSend ref "QUIT\r\n"
  con <- connection <$> readIORef ref
  killCon con

{-
  st <- readIORef ref
  when (Disconnected /= connection st) $ do
    -- close socket
    atomicModifyIORef_ ref (\s ->
        let f = L.setL connectionL Disconnected
              . removeAllChannels
              . rm Qraw . rm Qnick . rm Qpong
            removeAllChannels =
                id
              . L.setL targetMapLens mempty
              . L.setL targetsLens mempty
        in  (f s))
-}

doConnect :: IORef Fs -> B.ByteString -> B.ByteString -> IO ()
doConnect ref server nick = do
  time <- now
  out <- C.newChan
  !doIt <- atomicModifyIORef ref
     (\s ->
        let f s = if Disconnected == connection s then (con s,True) else (s,False)
            con =
                  L.setL addrLens server
                . touch (Qname 0) time
                . L.setL (dataL (Qname 0)) (Just server)
                . touch Qnick time
                . L.setL (dataL Qnick) (Just nick)
                . L.setL connectionL Connecting
                . event time (mconcat ["connecting ",server,"\n"])
        in f s)
  if doIt then do
      st <- readIORef ref
      let port = 6667
          server = addr st
      x <- E.catch (Just <$> getSocket (B.unpack server) port) (\e -> do
                  let
                    err = "error while opening socket: "
                          ++ show (e :: E.IOException)
                          ++ "\n"
                    f t = event t (B.pack err) . L.setL connectionL Disconnected
                  Nothing <$ (atomicModifyIORef_ ref . f =<< now)
                  )
      maybe
        (return ())
        (\s -> do
          thr <- C.forkIO (connect ref s out)
          let f t = event t "connecting seems to be successful\n"
                . L.setL connectionL (Connected thr out)
                -- XXX debug
                . event t ("debug: threadID: " `mappend` B.pack (show thr) `mappend` "\n")
          atomicModifyIORef_ ref (f time))
        x
     else atomicModifyIORef_ ref
              (event time "already Connected or Connecting\n")

connect :: IORef Fs -> N.Socket -> C.Chan B.ByteString -> IO ()
connect ref s out = do
  st <- readIORef ref
  withSocket st s out
  `E.finally`
      let f t = event t "disconnect\n" . L.setL connectionL Disconnected
      in  now >>= atomicModifyIORef_ ref . f >> N.sClose s

readerFun :: C.Chan (Either String I.Message) -> [BL.ByteString] -> IO ()
readerFun inc = mapM_ (C.writeChan inc . AL.eitherResult . AL.parse I.message)

isPingMessage :: I.Message -> Bool
isPingMessage (I.Message _ I.PING _) = True
isPingMessage _ = False

withSocket ::  Fs -> N.Socket -> C.Chan B.ByteString -> IO ()
withSocket st s toSend = do
  xs <- C.getChanContents toSend
  let inc = inChan st
      pass = ""
      user = B.pack (effectiveUserName st)
      nickName = fromMaybe "ircfs" (L.getL (dataL Qnick) st)
      knock = sayHello pass nickName user
      writer sock = mapM_ (N.sendAll sock) xs
  N.sendAll s knock
  ircs <- ircLines <$> NL.getContents s
  reader <- C.forkIO (readerFun inc ircs)
  writer s `E.finally` C.killThread reader

killCon :: Connection -> IO ()
killCon Disconnected     = return ()
killCon Connecting       = return ()
killCon (Connected t _)  = C.killThread t

ircLines :: BL.ByteString -> [BL.ByteString]
ircLines y = h : if BL.null t then [] else ircLines t
     where (h,t) = breakAfterCRLF y

sayHello :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
sayHello pass nick user =
  (if not (B.null pass) then "PASS " `mappend` pass `mappend` "\r\n" else mempty)
             `mappend` "NICK " `mappend` nick `mappend` "\r\nUSER "
             `mappend` user `mappend` " 0 * :" `mappend` nick `mappend` "\r\n"

rawSend :: IORef Fs -> B.ByteString -> IO ()
rawSend ref x = do
  let msg t x = touch Qraw t . append Qraw (">>>" `mappend` x)
      maybeSend t s =
        if isConnected (connection s)
        then (msg t x s, Just (out (connection s)))
        else (s,Nothing)
  !m <- atomicModifyIORef ref . maybeSend =<< now
  maybe (return ()) (`C.writeChan` x) m

isConnected :: Connection -> Bool
isConnected (Connected {})  = True
isConnected Disconnected    = False
isConnected Connecting      = False
