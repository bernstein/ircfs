{-# LANGUAGE OverloadedStrings #-}
-- -----------------------------------------------------------------------------
-- |
-- Module      :  Ircfs.Filesystem
-- Copyright   :  (c) Andreas-Christoph Bernstein 2011
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  andreas.bernstein@googlemail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- Ircfs filesystem
--
--------------------------------------------------------------------------------
module Ircfs.Filesystem
  (
  
    readF
  , showFilepath
  , fileStat
  , stat
  , rootDirFiles
  , subDirFiles
  , readDir

  , appendRaw
  , appendEvent
  , appendPong
  , writeNick
  ) where

import           Prelude hiding ((.), id, read)
import qualified Prelude as P
import           Control.Arrow
import           Control.Applicative
import           Control.Category
import           Control.Monad.State (modify)
import qualified Data.Lens.Common as L
import           Data.Monoid
import           Data.Char (isNumber)
import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap as IM

import qualified System.Fuse as F
import qualified System.Fuse.Request as F
import qualified System.Posix.Types as S
import           System.FilePath

import           Ircfs.Types

-- file to Qreq
fileToQreq :: Int -> String -> Maybe Qreq
fileToQreq _ "event" = Just Qevent
fileToQreq _ "nick"  = Just Qnick
fileToQreq _ "raw"   = Just Qraw
fileToQreq _ "pong"  = Just Qpong
fileToQreq n "ctl"   = Just (Qctl n)
fileToQreq n "data"  = Just (Qdata n)
fileToQreq n "name"  = Just (Qname n)
fileToQreq n "users" = Just (Qusers n)
fileToQreq _ _       = Nothing

fromFilePath :: FilePath -> Maybe Qreq
fromFilePath p
  | p == ""       = Nothing
  | p == "/"      = Just Qroot
  | p == "/ctl"   = Just Qrootctl
  | p == "/event" = Just Qevent
  | p == "/raw"   = Just Qraw
  | p == "/nick"  = Just Qnick
  | p == "/pong"  = Just Qpong
  | p == "/0"     = Just (Qdir 0)
  | 2 == length (splitDirectories p) =
                 let ds = splitDirectories p
                     ok = all isNumber x
                     x = last ds
                 in if ok then (Just . Qdir . P.read) x else Nothing
  | 3 == length (splitDirectories p) =
                 let ds = splitDirectories p
                     ok = all isNumber x
                     x = head (tail ds)
                 in if ok then fileToQreq (P.read x) (last ds) else Nothing
  | otherwise = Nothing

rootDirFiles :: [Qreq]
rootDirFiles = [Qrootctl, Qevent, Qnick, Qraw, Qpong]

subDirFiles :: [Qreq]
subDirFiles = [Qctl 0, Qdata 0, Qname 0, Qusers 0]

{-
files :: [Qreq]
files = [ Qroot, Qrootctl, Qevent, Qraw, Qnick, Qpong, Qdir 0, Qctl 0, Qname 0
        , Qusers 0, Qdata 0
        ]
-}

-- XXX change to full FilePath ???
showFile :: Qreq -> B.ByteString
showFile Qroot      = "/"
showFile Qrootctl   = "ctl"
showFile Qevent     = "event"
showFile Qraw       = "raw"
showFile Qnick      = "nick"
showFile Qpong      = "pong"
showFile (Qdir _)   = ""
showFile (Qctl _)   = "ctl"
showFile (Qname _)  = "name"
showFile (Qusers _) = "users"
showFile (Qdata _)  = "data"

-- XXX
showFilepath :: Qreq -> FilePath
showFilepath = B.unpack . showFile

filemode :: Qreq -> S.FileMode
filemode Qroot    = 0o555 -- DIR
filemode Qrootctl = 0o222
filemode Qevent   = 0o444
filemode Qraw     = 0o666
filemode Qnick    = 0o444
filemode Qpong    = 0o444
filemode Qdir  {} = 0o555 -- DIR
filemode Qctl  {} = 0o222
filemode Qname {} = 0o444
filemode Qusers{} = 0o444
filemode Qdata {} = 0o666

fileStat :: Qreq -> F.FileStat
fileStat Qroot   = F.defaultDirStat  { F.statFileMode = filemode Qroot }
fileStat Qdir {} = F.defaultDirStat  { F.statFileMode = filemode Qroot }
fileStat Qctl {} = F.defaultFileStat { F.statFileMode = filemode Qrootctl }
fileStat q       = F.defaultFileStat { F.statFileMode = filemode q }

stat :: IrcfsState -> FilePath -> Maybe F.FileStat
stat st p = maybePlus1 <$> m <*> x
  where m = fromFilePath p
        x = stat' st =<< m
        -- add 1 for newline charakter, XXX find a better way
        maybePlus1 Qnick s = L.modL statFileSizeL (+1) s
        maybePlus1 Qname {} s = L.modL statFileSizeL (+1) s
        maybePlus1 _ s = s

stat' :: IrcfsState -> Qreq -> Maybe F.FileStat
stat' f Qroot = Just $ F.defaultDirStat 
                            { F.statFileMode = filemode Qroot 
                            , F.statFileOwner = fromIntegral $ userID f
                            , F.statFileGroup = fromIntegral $ groupID f
                            }
stat' f Qdir {} = Just $ F.defaultDirStat 
                            { F.statFileMode = filemode Qroot 
                            , F.statFileOwner = fromIntegral $ userID f
                            , F.statFileGroup = fromIntegral $ groupID f
                            }
stat' f Qctl {} = Just $ F.defaultFileStat 
                            { F.statFileMode = filemode Qrootctl 
                            , F.statFileOwner = fromIntegral $ userID f
                            , F.statFileGroup = fromIntegral $ groupID f
                            }
stat' f q =
  let mn = fromIntegral . B.length <$> read' f q
      s = F.defaultFileStat { F.statFileMode = filemode q 
                            , F.statFileOwner = fromIntegral $ userID f
                            , F.statFileGroup = fromIntegral $ groupID f
                            }
  in  (\n -> L.setL statFileSizeL n s) <$> mn

statFileSizeL :: L.Lens F.FileStat S.FileOffset
statFileSizeL = L.lens F.statFileSize (\x s -> s { F.statFileSize = x })

readF :: IrcfsState -> FilePath -> S.ByteCount -> S.FileOffset -> Maybe B.ByteString
readF s p bc off = cut <$> (read' s =<< fromFilePath p)
  where cut = B.take (fromIntegral bc) . B.drop (fromIntegral off)

read' :: IrcfsState -> Qreq -> Maybe B.ByteString
read' _ Qroot        = Nothing
read' _ Qrootctl     = Just mempty
read' con Qevent     = Just $ eventFile con
read' con Qraw       = Just $ rawFile con
read' con Qnick      = Just $ mappend (nick con) "\n"
read' con Qpong      = Just $ pongFile con
read' _ Qdir {}      = Nothing
read' _ Qctl {}    = Just mempty
read' con (Qname 0)  = Just . (`mappend` "\n") . addr $ con
read' con (Qname k)  = 
  ((`mappend` "\n") . targetName) <$> L.getL (targetLens k) con
read' _ (Qusers 0)   = Just mempty
read' con (Qusers k) = users <$> L.getL (targetLens k) con
read' _ (Qdata 0)    = Just mempty
read' con (Qdata k)  = text <$> L.getL (targetLens k) con

readDir' :: IrcfsState -> Qreq -> [(FilePath, F.FileStat)]
readDir' st Qroot = 
  let ks = IM.keys (targets st)
      rootDir = map (showFilepath &&& fileStat) rootDirFiles
      subDirs = map (\x -> (show x,F.defaultDirStat)) ks
  in  [(".", F.defaultDirStat), ("..", F.defaultDirStat), 
            ("0",F.defaultDirStat) ] ++ rootDir ++ subDirs
readDir' _ Qdir {} = 
  let subDir = map (showFilepath &&& fileStat) subDirFiles
  in [(".", F.defaultDirStat), ("..",F.defaultDirStat)] ++ subDir
readDir' _ _ = []

readDir :: IrcfsState -> FilePath -> [(FilePath, F.FileStat)]
readDir st p = maybe [] (readDir' st) (fromFilePath p)

{-
- uses readHelper
statHelper :: Qreq -> Connection -> Int
statHelper (Qname k) con = targetName con
-}

-- might return empty string, if filepath is unknown 
-- readHelper :: Qreq -> (Connection -> B.ByteString)
-- readHelper (Qname k) con =

appendRaw :: B.ByteString -> Ircfs ()
appendRaw s = modify $ L.modL rawLens (`mappend` s)

appendEvent :: B.ByteString -> Ircfs ()
appendEvent s = modify $ L.modL eventLens (`mappend` s)

writeNick :: B.ByteString -> Ircfs ()
writeNick = modify . L.modL nickLens . const

appendPong :: B.ByteString -> Ircfs ()
appendPong s = modify $ L.modL pongLens (`mappend` s)

--write :: IrcfsState -> Qreq -> B.ByteString -> S.FileOffset -> (IrcfsState, Either Errno S.ByteCount)
--write :: IrcfsState -> Qreq -> B.ByteString -> S.FileOffset -> (IrcfsState, Maybe S.ByteCount)
--write st Qrootctl _ off = (st,Just off)
--write st Qevent s off =
--  let st' = L.modL eventLens (`mappend` s) st
--  in (st', Just off)
--
