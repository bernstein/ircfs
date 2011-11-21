{-# LANGUAGE OverloadedStrings #-}
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
module Main where

import Prelude hiding ((.), id)
import Control.Category
import Control.Applicative
import qualified Data.ByteString.Char8 as B
import Foreign.C.Error
import System.Posix.Types
import qualified System.Posix.User as S
import System.FilePath (takeBaseName)
import System.Environment (withArgs)
import qualified System.Fuse as F
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (foldM)
import qualified Control.Concurrent.Chan as C
import qualified Control.Concurrent as C
import qualified Network.Socket.Enumerator as E
import qualified Data.Enumerator as E hiding (drop)
import qualified Data.Enumerator.List as EL
import Data.Monoid

import qualified Network.Socket as N
import qualified Network.Socket.ByteString  as N (sendAll)

import Network.IRC.Enumerator
import Ircfs.Types
import Ircfs.Process
import qualified Ircfs.CmdLine as O
import qualified System.Fuse.Request as F

chanToIter2 :: C.Chan a -> E.Iteratee a IO ()
chanToIter2 c = go
  where
    go = EL.head >>= maybe go (\x -> liftIO (C.writeChan c x) >> go)

-- | Listens on the socket, writes received messages to fsReq
ircReader :: C.Chan F.Request -> N.Socket -> IO ()
ircReader fsReq socket =
  E.run_ $ E.enumSocket 1024 socket E.$$ irclines E.=$ iterFuseWriteFs_ fsReq

ircWriter :: N.Socket -> IrcOut -> IO ()
ircWriter s out = mapM_ (N.sendAll s) =<< C.getChanContents (unIrcOut out) 

-- | Initialize file system.
fsInit :: C.Chan F.Request -> O.Config -> IO ()
fsInit fsReq cfg = do
  s <- getSocket (O.addr cfg) (read (O.port cfg))
  uid <- fromIntegral <$> S.getEffectiveUserID
  gid <- fromIntegral <$> S.getEffectiveGroupID
  -- userEntry <- S.getUserEntryForID (fromIntegral uid)
  name <- S.getEffectiveUserName
  _ <- C.forkIO $ ircReader fsReq s

  let nickMsg = B.pack $ "NICK " ++ O.nick cfg ++ "\r\n" 
  N.sendAll s nickMsg
  let userMsg = B.pack $ "USER "++name++" 0 * :" ++ O.nick cfg ++"\r\n"
  N.sendAll s userMsg
  ircoutc <- IrcOut <$> C.newChan
  _ <- C.forkIO $ ircWriter s ircoutc

  rs <- C.getChanContents fsReq
  let st = IrcfsState 
            { addr = B.pack . O.addr $ cfg
            , nick = B.pack . O.nick $ cfg
            , targets = mempty
            , eventFile = ""
            , pongFile = ""
            , rawFile = ""
            , nextDirNames = [1..100]
            , targetMap = mempty
            , userID = uid
            , groupID = gid
            }

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

