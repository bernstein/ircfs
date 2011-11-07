{-# LANGUAGE OverloadedStrings #-}
module Ircfs.Filesystem
where

import Control.Applicative
import Prelude hiding ((.), id, read)
import qualified Prelude as P
import Control.Category
import qualified Data.Lens.Common as L
import qualified System.Fuse as F
import qualified System.Fuse.Request as F
import qualified System.Posix.Types as S
import System.FilePath
import Data.Monoid
import Data.Char (isNumber)
import qualified Data.ByteString.Char8 as B
import Ircfs.Types

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

files :: [Qreq]
files = [ Qroot, Qrootctl, Qevent, Qraw, Qnick, Qpong, Qdir 0, Qctl 0, Qname 0
        , Qusers 0, Qdata 0
        ]

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
stat (IrcfsState NotConnected) _ = Nothing
stat (IrcfsState con) "/nick" =
  let size = fromIntegral . B.length . nick $ con
  in  Just $ (fileStat Qnick) { F.statFileSize = 1+size }
stat (IrcfsState con) "/event" =
  let size = fromIntegral . B.length . eventFile $ con
  in  Just $ (fileStat Qevent) { F.statFileSize = size }
stat (IrcfsState con) "/raw" =
  let size = fromIntegral . B.length . rawFile $ con
  in  Just $ (fileStat Qraw) { F.statFileSize = size }
stat (IrcfsState con) "/pong" =
  let size = fromIntegral . B.length . pongFile $ con
  in  Just $ (fileStat Qpong) { F.statFileSize = size }
stat (IrcfsState con) "/0/name" =
  let size = fromIntegral . B.length . addr $ con
  in  Just $ (fileStat (Qname 0)) { F.statFileSize = 1+size }
stat (IrcfsState con) p = fileStat <$> fromFilePath p

readF :: IrcfsState -> FilePath -> S.ByteCount -> S.FileOffset -> Maybe B.ByteString
readF s@(IrcfsState con) p bc off = cut <$> (read' s =<< fromFilePath p)
  where cut = B.take (fromIntegral bc) . B.drop (fromIntegral off)

read' :: IrcfsState -> Qreq -> Maybe B.ByteString
read' (IrcfsState con) Qroot      = Nothing
read' (IrcfsState con) Qrootctl   = Just mempty
read' (IrcfsState con) Qevent     = Just $ eventFile con
read' (IrcfsState con) Qraw       = Just $ rawFile con
read' (IrcfsState con) Qnick      = Just $ B.append (nick con) "\n"
read' (IrcfsState con) Qpong      = Just $ pongFile con
read' (IrcfsState con) Qdir {}    = Nothing
read' (IrcfsState con) Qctl {}    = Just mempty
read' (IrcfsState con) (Qname 0)  = Just . (`B.append` "\n") . addr $ con
read' (IrcfsState con) (Qname k)  = 
  ((`B.append` "\n") . targetName) <$> L.getL (targetLens k) con
read' (IrcfsState con) (Qusers 0) = Just mempty
read' (IrcfsState con) (Qusers k) = users <$> L.getL (targetLens k) con
read' (IrcfsState con) (Qdata 0)  = Just mempty
read' (IrcfsState con) (Qdata k)  = text <$> L.getL (targetLens k) con

{-
- uses readHelper
statHelper :: Qreq -> Connection -> Int
statHelper (Qname k) con = targetName con
-}

-- might return empty string, if filepath is unknown 
-- readHelper :: Qreq -> (Connection -> B.ByteString)
-- readHelper (Qname k) con =

