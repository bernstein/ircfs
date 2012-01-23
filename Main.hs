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
import qualified Data.ByteString.Char8 as B
import Foreign.C.Error
import Foreign.C.Types (CTime)
import qualified Data.Time as T
import qualified Data.Time.Clock.POSIX as T
import Data.IORef
import System.Posix.Types
import qualified System.Posix.User as S
import System.FilePath (takeBaseName)
import System.Environment (withArgs)
import qualified System.Fuse as F
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (foldM, when)
import qualified Control.Concurrent.Chan as C
import qualified Control.Concurrent as C
import qualified Network.Socket.Enumerator as E
import qualified Data.Enumerator as E hiding (drop)
import qualified Data.Enumerator.List as EL
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Lens.Common as L

import qualified Network.Socket as N
import qualified Network.Socket.ByteString  as N (sendAll)

import Network.IRC.Enumerator
import Ircfs.Types
import Ircfs.Process
import Ircfs.Inode
import Ircfs.Filesystem
import Ircfs.Ctl as I
import Ircfs.Misc
import qualified Ircfs.CmdLine as O
import qualified System.Fuse.Request as F
import Data.Attoparsec as A
import qualified Network.IRC.Message as I

-- addr
-- XXX
-- | Listens on the socket, writes received messages to fsReq
ircReader :: IrcOut -> IORef IrcfsState -> C.Chan F.Request -> N.Socket -> IO ()
ircReader ircoutc ref fsReq socket =
  E.run_ $ E.enumSocket 1024 socket E.$$ irclines E.=$ iterFuseWriteFs_ ircoutc ref fsReq

foldMany :: Monad m => ([a] -> E.Iteratee a m b) -> E.Iteratee a m ()
foldMany f = E.continue step where
	step E.EOF = E.yield () E.EOF
	step (E.Chunks []) = E.continue step
	step (E.Chunks xs) = f xs >> E.continue step

iterFuseWriteFs_ :: MonadIO m => IrcOut -> IORef IrcfsState -> C.Chan F.Request -> E.Iteratee B.ByteString m ()
iterFuseWriteFs_ ircoutc ref fsReq = foldMany (liftIO . writeMany_ ircoutc ref fsReq)

writeMany_ :: IrcOut -> IORef IrcfsState -> C.Chan F.Request -> [B.ByteString] -> IO (C.Chan F.Request)
writeMany_ ircoutc ref fsReq xs = foldM write_ fsReq xs
  where write_ c x = ircin ircoutc ref x >> return c
-- fsWrite ircoutc ref "/ctl" fh s off = do

ircWriter :: N.Socket -> IrcOut -> IO ()
ircWriter s out = mapM_ (N.sendAll s) =<< C.getChanContents (unIrcOut out) 

-- | Initialize file system.
fsInit :: IrcOut -> IORef IrcfsState -> C.Chan F.Request -> O.Config -> IO ()
fsInit ircoutc ref fsReq cfg = do
  s <- getSocket (O.addr cfg) (read (O.port cfg))
  -- userEntry <- S.getUserEntryForID (fromIntegral uid)
  readTh <- C.forkIO $ ircReader ircoutc ref fsReq s
  !_ <- atomicModifyIORef ref (\st ->
      let f = L.setL statusL Connected
            . L.modL threadsL (readTh:)
      in (f st, ()))

-- XXX
  let nickMsg = B.pack $ "NICK " ++ O.nick cfg ++ "\r\n" 
  N.sendAll s nickMsg
-- XXX
  st <- readIORef ref
  let userMsg = B.pack $ "USER " ++ (effectiveUserName st) ++ " 0 * :"
                                ++ O.nick cfg ++"\r\n"
  N.sendAll s userMsg
  writeTh <- C.forkIO (ircWriter s ircoutc)
  !_ <- atomicModifyIORef ref (\st ->
      let f = L.setL statusL Connected
            . L.modL threadsL (writeTh:)
      in (f st, ()))
  return ()

fsDestroy :: IORef IrcfsState -> IO ()
fsDestroy = disconnect

main :: IO ()
main = N.withSocketsDo $ do
  args <- O.cmdLine
  fsReq <- C.newChan
  ref <- newIORef =<< newFS (B.pack . O.addr $ args) (B.pack . O.nick $ args)
  ircoutc <- IrcOut <$> C.newChan
  let ops = F.defaultFuseOps {
              F.fuseInit          = fsInit ircoutc ref fsReq args
            , F.fuseDestroy       = fsDestroy ref
            , F.fuseGetFileStat   = fsStat ref
            , F.fuseReadDirectory = fsReadDir ref
            , F.fuseOpenDirectory = fsOpenDirectory
            , F.fuseOpen          = fsOpen ref
            , F.fuseRead          = fsRead ref
            , F.fuseWrite         = fsWrite ircoutc ref
            , F.fuseSetFileSize   = fsTruncate
            }
-- XXX
  withArgs [O.mtpt args, "-d"] $ F.fuseMain ops F.defaultExceptionHandler

fsStat :: IORef IrcfsState -> FilePath -> IO (Either Errno F.FileStat)
fsStat ref p
 = do
    st <- readIORef ref
    let ms = stat st =<< parsePath p
    return $ maybe (Left F.eNOENT) Right ms

fsOpenDirectory :: FilePath -> IO Errno
fsOpenDirectory  = const (return eOK)

fsTruncate :: FilePath -> FileOffset -> IO Errno
fsTruncate p _ = if takeBaseName p == "ctl" then return eOK else return eACCES

newFS :: B.ByteString -> FileData -> IO IrcfsState
newFS a n = do
  uid <- fromIntegral <$> S.getEffectiveUserID
  gid <- fromIntegral <$> S.getEffectiveGroupID
  name <- S.getEffectiveUserName
  time <- now
  tz <- T.getCurrentTimeZone
  let st = IrcfsState 
          { addr = a
          , targets = mempty
          , targetMap = mempty
          , userID = uid
          , groupID = gid
          , effectiveUserName = name
          , inodes = mempty
          , start = time
          , timeZone = tz
          , status = Disconnected
          , threads = []
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

-- touch :: Time -> Inode -> Inode

fsReadDir :: IORef IrcfsState -> FilePath
          -> IO (Either Errno [(FilePath, F.FileStat)])
fsReadDir ref p = do
  st <- readIORef ref
  let ds = readDir' st <$> parsePath p
  return $ maybe (Left F.eNOENT) Right ds

fsOpen :: IORef IrcfsState -> FilePath -> F.OpenMode -> F.OpenFileFlags 
        -> IO (Either Errno FH)
fsOpen ref p _ _ = do
  st <- readIORef ref
  return $ maybe (Left F.eACCES)
          (\p' -> if exists p' st then Right FH else Left F.eACCES)
          (parsePath p)

exists :: Qreq -> IrcfsState -> Bool
exists p = M.member p . inodes

fsRead :: IORef IrcfsState -> FilePath -> FH -> ByteCount ->
            FileOffset -> IO (Either Errno B.ByteString)
fsRead ref p _ bc off = do
  st <- readIORef ref
  let s = readF st p bc off
  return $ maybe (Left F.eNOENT) Right s

fsRelease :: IORef IrcfsState -> FilePath -> FH -> IO ()
fsRelease _ _ _ = return ()


raw ircoutc ref s = do
  time <- now
  !_ <- atomicModifyIORef ref (\st ->
      let f = touch Qraw time . append Qraw (">>>" `mappend` s)
      in (f st, ()))
  C.writeChan (unIrcOut ircoutc) s

-- write to ctj
-- Todo if a msg is longer than 512 then split it into chunks
    -- XXX TODO: process ctl
    -- mR >>= processCtl
-- XXX
fsWrite :: IrcOut -> IORef IrcfsState -> FilePath -> FH -> B.ByteString 
        -> FileOffset -> IO (Either Errno ByteCount)
fsWrite ircoutc ref "/ctl" fh s off = do
  let mR = A.maybeResult $ A.parse I.parseCtl s
  maybe (return ()) (raw ircoutc ref . I.encode . toMessage) mR
  return (Right (fromIntegral (B.length s)))
fsWrite ircoutc ref "/raw" _ s _ = do
  raw ircoutc ref s
  return (Right (fromIntegral (B.length s)))
fsWrite ircoutc ref p fh s off = do
  st <- readIORef ref
  stamp <- timeStamp
  time <- now
-- XXX
  let x = write st stamp s <$> parsePath p
  case x of
-- XXX
    Just (st', []) -> do
      !_ <- atomicModifyIORef ref (\_ -> (st',()))
      return ()
    Just (st', msg:_) -> do
      !_ <- atomicModifyIORef ref (\_ -> (st',()))
      raw ircoutc ref (I.encode msg)
    _ -> return ()
  return . Right . fromIntegral . B.length $ s
fsWrite _ _ _ _ _ _ = return (Left F.eNOENT)

ircin :: IrcOut -> IORef IrcfsState -> B.ByteString -> IO ()
ircin ircoutc ref s = do
  --tz <- T.getCurrentTimeZone
  time <- now
-- XXX
  !_ <- atomicModifyIORef ref (\st ->
       let f = touch Qraw time . append Qraw ("<<<" `mappend` s `mappend` "\n")
      in (f st, ()))
  let m = A.maybeResult $ A.feed (A.parse I.message s) "\n"
-- XXX
  st <- readIORef ref
  (ms',st') <- runIrcfs st $ maybe (return []) (processIrc time) m
  !_ <- atomicModifyIORef ref (\_ -> (st',()))
  -- das ist nur die pong message
  mapM_ (\c -> fsWrite ircoutc ref "/raw" FH (I.encode c) 0) ms'

disconnect :: IORef IrcfsState -> IO ()
disconnect ref = do
  print "disconnect"
  st <- readIORef ref
  when (Disconnected /= status st) $ do
    mapM_ C.killThread (threads st)
    atomicModifyIORef ref (\s ->
        let f = L.setL statusL Disconnected
              . L.setL threadsL []
              . removeAllChannels
              . rm Qraw . rm Qnick . rm Qpong
            removeAllChannels =
                id
              . L.setL targetMapLens mempty
              . L.setL targetsLens mempty
        in  (f s,()))
    return ()

connect ref addr nick pass = do
  return ()
