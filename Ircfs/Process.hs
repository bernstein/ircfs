{-# LANGUAGE OverloadedStrings #-}
-- -----------------------------------------------------------------------------
-- |
-- Module      :  Ircfs.Process
-- Copyright   :  (c) Andreas-Christoph Bernstein 2011
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  andreas.bernstein@googlemail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- Process incoming file system messages.
--------------------------------------------------------------------------------
module Ircfs.Process
  ( 
    process
  ) where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Applicative
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.State (get, modify)
import qualified Control.Concurrent.Chan as C
import qualified Control.Concurrent as C
import qualified Data.Lens.Common as L
import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap as IM
import Data.Monoid
import Data.Maybe (maybeToList)
import Data.Attoparsec as A
import qualified Data.Time as T
import System.Locale (defaultTimeLocale)

import qualified System.Fuse.Request as F

import qualified Network.IRC.Message as I
import Ircfs.Ctl as I
import Ircfs.Types
import Ircfs.Filesystem

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
processTmsg ircoutc (F.Twrite "/ctl" s _) = do
  -- Todo if a msg is longer than 512 then split it into chunks
  _ <- maybe 
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
-- example: PING :irc.funet.fi ; Ping message sent by server
processIrc ircoutc (I.Message _ I.PING (I.Params _ (Just p))) = do
    stamp <- timeStamp
    let cmd = "pong " `B.append` p `B.append` "\n"
        off = fromIntegral . B.length $ cmd
        s = stamp `B.append` " " `B.append` cmd
    _ <- processTmsg ircoutc (F.Twrite "/ctl" cmd off)
    appendPong s
processIrc _ (I.Message (Just (I.PrefixNick n _ _)) I.NICK (I.Params [] (Just new))) = do
  yourNick <- (nick.connection) <$> get
  if n == yourNick
    then do
      appendEvent $ "your nick changed to " `B.append` new `B.append` "\n"
      writeNick new
    else
      appendEvent $ n `B.append` " changed nick to " 
                      `B.append` new 
                      `B.append` "\n"
processIrc _ (I.Message (Just (I.PrefixNick n _ _)) I.NICK (I.Params (new:_) _)) = do
  yourNick <- (nick.connection) <$> get
  if n == yourNick
    then do
      appendEvent $ "your nick changed to " `B.append` new `B.append` "\n"
      writeNick new
    else
      appendEvent $ n `B.append` " changed nick to " 
                      `B.append` new 
                      `B.append` "\n"
processIrc _ (I.Message (Just (I.PrefixNick n _ _)) I.JOIN (I.Params (c:_) _)) = do
  yourNick <- (nick.connection) <$> get
  when (n == yourNick) $ do
    k <- nextDirName
    modify $ L.setL (targetLens k.connectionLens) 
                    (Just (Target k TChannel c mempty mempty))
    modify $ L.setL (targetMapLens' c.connectionLens) (Just k)
    let s = B.pack $ "new " ++ show k ++ " "
    appendEvent $ s `B.append` c `B.append` "\n"
processIrc _ (I.Message (Just (I.PrefixNick n _ _)) I.PART (I.Params (c:_) _)) = do
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
processIrc _ (I.Message (Just (I.PrefixNick n _ _)) I.PRIVMSG (I.Params (c:cs) t)) = do
  stamp <- timeStamp
  tm <- L.getL (targetMapLens' c.connectionLens) <$> get
  let ts = maybeToList t
  maybe (return ()) (\k -> do
      modify $ L.modL (targetLens k.connectionLens) 
                      (fmap (L.modL textLens (\s -> s `B.append` stamp `B.append` " < " `B.append` n `B.append` "> " `B.append` B.unwords (cs++ts) `B.append` "\n")))
      ) tm
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

timeStamp :: MonadIO m => m B.ByteString
timeStamp = do
  now <- liftIO T.getCurrentTime
  return . B.pack $ T.formatTime defaultTimeLocale "%H:%M" now

