{-# LANGUAGE OverloadedStrings, BangPatterns #-}
-- -----------------------------------------------------------------------------
-- |
-- Module      :  Ircfs.Connect
-- Copyright   :  (c) Andreas-Christoph Bernstein 2011
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  andreas.bernstein@googlemail.com
-- Stability   :  unstable
-- Portability :  not portable
--
--------------------------------------------------------------------------------
module Ircfs.Connect
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

connectingPossible :: IORef Fs -> IO Bool
connectingPossible ref = do
  time <- now
  !ok <- atomicModifyIORef ref
     (\s ->
        let f s = if Disconnected == connection s then (con s,True) else (s,False)
            con = L.setL connectionL Connecting . event time "connecting\n"
        in f s)
  return ok

connect :: IORef Fs -> B.ByteString -> B.ByteString -> IO ()
connect ref server nick = do
  !ok <- connectingPossible ref
  time <- now
  if ok then do
      let port = 6667
      E.catch (do
                sock <- getSocket (B.unpack server) port 
                out <- C.newChan
                let clean s =
                        -- set nick
                          touch Qnick time
                        . L.setL (dataL Qnick) (Just nick)
                        -- create control channel
                        . insertChannel server time
                        -- remove all channels
                        . L.setL targetMapLens mempty
                        . L.setL targetsLens mempty
                atomicModifyIORef_ ref (clean time)

                thr <- C.forkIO (runCon ref sock out)
                let f t = 
                        event t "connecting seems to be successful\n"
                      . L.setL connectionL (Connected thr out)
                      . L.setL addrLens server

                atomicModifyIORef_ ref (f time))
              (\e -> do
                let
                  err = "error while opening socket: "
                        ++ show (e :: E.IOException)
                        ++ "\n"
                  f t = event t (B.pack err) 
                      . L.setL connectionL Disconnected
                atomicModifyIORef_ ref (f time)
              )
     else atomicModifyIORef_ ref
              (event time "already Connected or Connecting\n")

runCon :: IORef Fs -> N.Socket -> C.Chan B.ByteString -> IO ()
runCon ref s out = do
  st <- readIORef ref
  withSocket st s out
  `E.finally`
      let f t = event t "disconnect\n" . L.setL connectionL Disconnected
      in  now >>= atomicModifyIORef_ ref . f >> N.sClose s

readerFun :: C.Chan (Either String I.Message) -> [BL.ByteString] -> IO ()
readerFun inc = mapM_ (C.writeChan inc . AL.eitherResult . AL.parse I.message)

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

ircLines :: BL.ByteString -> [BL.ByteString]
ircLines = split "\r\n"

sayHello :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
sayHello pass nick user =
  (if not (B.null pass) then "PASS " `mappend` pass `mappend` "\r\n" else mempty)
             `mappend` "NICK " `mappend` nick `mappend` "\r\nUSER "
             `mappend` user `mappend` " 0 * :" `mappend` nick `mappend` "\r\n"

killCon :: Connection -> IO ()
killCon Disconnected     = return ()
killCon Connecting       = return ()
killCon (Connected t _)  = C.killThread t

isConnected :: Connection -> Bool
isConnected (Connected {})  = True
isConnected Disconnected    = False
isConnected Connecting      = False
