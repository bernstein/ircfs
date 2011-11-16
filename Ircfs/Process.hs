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
import Control.Applicative
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.State (get, modify)
import qualified Control.Concurrent.Chan as C
import qualified Control.Concurrent as C
import qualified Data.Lens.Common as L
import qualified Data.ByteString.Char8 as B
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

-- process :: Enumeratee F.Request I.Message IO a
--process :: IrcfsState -> F.Request -> IO (Maybe B.ByteString, IrcfsState)
--process :: IrcOut -> IrcfsState -> F.Request -> IO IrcfsState
process :: IrcOut -> F.Request -> Ircfs ()
process o (F.ReqRep t m) = processTmsg o t >>= (io . C.putMVar m)
process o (F.Req t)      = processTmsg o t >> return ()

-- | Process incoming filesystem requests.
processTmsg :: IrcOut -> F.Tmsg -> Ircfs F.Rmsg
processTmsg _ (F.Tread p bc off) = do
  st <- get
  maybe (return F.Rerror) (return . F.Rread) (readF st p bc off)
processTmsg _ F.Topen {} = return F.Ropen
processTmsg _ (F.Tstat p) = maybe F.Rerror F.Rstat . (`stat` p) <$> get
processTmsg _ (F.Treaddir p) = F.Rreaddir . (`readDir` p) <$> get
-- process Twrite, usually appends to files
processTmsg ircoutc (F.Twrite "/ctl" s _) = do
  -- Todo if a msg is longer than 512 then split it into chunks
  let mR = A.maybeResult $ A.parse I.parseCtl s
      -- XXX TODO: process ctl
      -- mR >>= processCtl
  _ <- maybe 
    (return F.Rerror)
    (\c -> processTmsg ircoutc (F.Twrite "/raw" (I.encode.toMessage $c) 0))
    mR
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
  appendRaw (">>>" `mappend` s)
  io . C.writeChan (unIrcOut ircoutc) $ s
  return . F.Rwrite . fromIntegral . B.length $ s
processTmsg ircoutc (F.Twrite "/ircin" s _) = do
  appendRaw ("<<<" `mappend` s `mappend` "\n")
  let m = A.maybeResult $ A.feed (A.parse I.message s) "\n"
  ms' <- maybe (return []) processIrc m
  mapM_ (\c -> processTmsg ircoutc (F.Twrite "/raw" (I.encode c) 0)) ms'
  return . F.Rwrite . fromIntegral . B.length $ s
processTmsg _ (F.Twrite {}) = return F.Rerror

-- | Process incomming irc messages.
processIrc :: I.Message -> Ircfs [I.Message]
-- example: PING :irc.funet.fi ; Ping message sent by server
processIrc (I.Message _ I.PING (I.Params _ (Just p))) = do
  stamp <- timeStamp
  let s = mconcat [stamp," pong ",p,"\n"]
      msg = I.Message Nothing I.PONG (I.Params [p] Nothing)
  appendPong s
  return [msg]
processIrc (I.Message (Just (I.PrefixNick n _ _)) I.NICK (I.Params [] (Just new))) = do
  yourNick <- nick <$> get
  if n == yourNick
    then do
      appendEvent . mconcat $ ["You're now known as ",new,"\n"]
      writeNick new
    else
      appendEvent . mconcat $ [n," changed nick to ",new,"\n"]
  return []
processIrc (I.Message (Just (I.PrefixNick n _ _)) I.NICK (I.Params (new:_) _)) = do
  yourNick <- nick <$> get
  if n == yourNick
    then do
      appendEvent (mconcat ["You're now known as ",new,"\n"])
      writeNick new
    else
      appendEvent . mconcat $ [n," changed nick to ",new,"\n"]
  return []
processIrc (I.Message (Just (I.PrefixNick n _ _)) I.JOIN (I.Params (c:_) _)) = do
  yourNick <- nick <$> get
  when (n == yourNick) $ do
    k <- nextDirName
    modify $ L.setL (targetLens k) 
                    (Just (Target k TChannel c mempty mempty))
    modify $ L.setL (targetMapLens' c) (Just k)
    let s = B.pack $ "new " ++ show k ++ " "
    appendEvent $ s `mappend` c `mappend` "\n"
  return []
processIrc (I.Message (Just (I.PrefixNick n _ _)) I.PART (I.Params (c:_) _)) = do
  yourNick <- nick <$> get
  when (n == yourNick) $ do
    m <- L.getL (targetMapLens' c) <$> get
    maybe (return ()) (\k -> do
        modify (L.setL (targetLens k) Nothing)
        modify (L.setL (targetMapLens' c) Nothing)
        freeDirName k
        let s = B.pack $ "del " ++ show k ++ " "
        appendEvent (mconcat [s,c,"\n"])
      ) m
  return []
processIrc (I.Message Nothing I.PRIVMSG (I.Params (c:cs) t)) = do
  stamp <- timeStamp
  tm <- L.getL (targetMapLens' c) <$> get
  n <- nick <$> get
  let ts = maybeToList t

  maybe (return ()) (\k -> do
      modify $ L.modL (targetLens k)
                      (fmap (L.modL textLens (\s -> mconcat [s,stamp," < ",n,"> ",B.unwords (cs++ts),"\n"])))
      ) tm
  return []
processIrc (I.Message (Just (I.PrefixNick n _ _)) I.PRIVMSG (I.Params (c:cs) t)) = do
  stamp <- timeStamp
  tm <- L.getL (targetMapLens' c) <$> get
  let ts = maybeToList t

  maybe (return ()) (\k -> do
      modify $ L.modL (targetLens k) 
                      (fmap (L.modL textLens (\s -> mconcat [s,stamp," < ",n,"> ",B.unwords (cs++ts),"\n"])))
      ) tm
  return []
processIrc m@(I.Message _ I.ERROR _) = do
  appendEvent (mconcat ["error ",I.encode m,"\n"])
  return []
processIrc _ = return []

nextDirName :: Ircfs Int
nextDirName = do
  k <- (head . nextDirNames) <$> get
  modify $ L.modL nextDirNamesLens tail
  return k

freeDirName :: Int -> Ircfs ()
freeDirName = modify . L.modL nextDirNamesLens . (:)

timeStamp :: MonadIO m => m B.ByteString
timeStamp = do
  now <- liftIO T.getCurrentTime
  return . B.pack $ T.formatTime defaultTimeLocale "%H:%M" now

