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
  , parsePath
  , fileStat
  , stat
  , rootDirFiles
  , subDirFiles
  , readDir
  , write

  , append
  , substitute
  , touch
  , insertChannel
  , removeChannel
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
import           Data.Maybe (maybeToList, fromMaybe)
import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap as IM
import qualified Data.Map as M
import           Foreign.C.Types (CTime)

import qualified System.Fuse as F
import qualified System.Fuse.Request as F
import qualified System.Posix.Types as S
import           System.FilePath

import           Ircfs.Types
import           Ircfs.Misc
import           Ircfs.Inode
import qualified Network.IRC.Message as I

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

parsePath :: FilePath -> Maybe Qreq
parsePath p
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

{-
stat :: IrcfsState -> FilePath -> Maybe F.FileStat
stat st p = maybePlus1 <$> m <*> x
  where m = parsePath p
        x = stat' st =<< m
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
-}

readF :: IrcfsState -> FilePath -> S.ByteCount -> S.FileOffset -> Maybe B.ByteString
readF s p bc off = cut <$> (read' s =<< parsePath p)
  where cut = B.take (fromIntegral bc) . B.drop (fromIntegral off)

read' :: IrcfsState -> Qreq -> Maybe B.ByteString
read' _ Qroot        = Nothing
read' con Qrootctl   = L.getL (dataL Qrootctl) con
read' con Qevent     = L.getL (dataL Qevent) con
read' con Qpong      = L.getL (dataL Qpong) con
read' con Qraw       = L.getL (dataL Qraw) con
read' con Qnick      = L.getL (dataL Qnick) con
read' con (Qname k)  = L.getL (dataL (Qname k)) con
read' con (Qusers k) = L.getL (dataL (Qusers k)) con
read' con (Qdata k)  = L.getL (dataL (Qdata k)) con
read' _ Qctl {}      = Just mempty
read' _ Qdir {}      = Nothing
read' _ _ = Nothing

readDir' :: IrcfsState -> Qreq -> [(FilePath, F.FileStat)]
readDir' st Qroot = 
  let ks = IM.keys (targets st)
      rootDir = map (showFilepath &&& fileStat) rootDirFiles
      subDirs = map (\x -> (show x,F.defaultDirStat)) ks
  in  [(".", F.defaultDirStat), ("..", F.defaultDirStat)] 
      ++ rootDir ++ subDirs
readDir' _ Qdir {} = 
  let subDir = map (showFilepath &&& fileStat) subDirFiles
  in [(".", F.defaultDirStat), ("..",F.defaultDirStat)] ++ subDir
readDir' _ _ = []

readDir :: IrcfsState -> FilePath -> [(FilePath, F.FileStat)]
readDir st p = maybe [] (readDir' st) (parsePath p)

append :: Qreq -> B.ByteString -> Endomorphism IrcfsState
append p s = L.modL (dataL p) (`mappend` Just s)

substitute :: Qreq -> B.ByteString -> Endomorphism IrcfsState
substitute p s = L.setL (dataL p) (Just s)

touch :: Qreq -> CTime -> Endomorphism IrcfsState
touch p t = L.modL (inodeL p) (fmap (setTimes t))

type Timestamp = B.ByteString
write :: IrcfsState -> Timestamp -> B.ByteString -> Qreq -> (IrcfsState, [I.Message])
write st _ _ Qrootctl = (st, mempty)
write st t xs Qevent = (append Qevent xs st,[])
write st t xs Qnick = (substitute Qnick xs st,[])
write st t xs Qpong = (append Qpong xs st,[])
write st stamp xs p@(Qdata k) = 
  let targets = maybeToList . L.getL (dataL (Qname k)) $ st
      line = mconcat [stamp, " < ",me,"> ", xs]
      me = fromMaybe mempty (L.getL nickLens st) -- nick st
      n = fromIntegral (B.length line)
      msg = privmsg targets xs
  in  (append p line st, [msg])
write st _ _ _ = (st, mempty)

privmsg :: [B.ByteString] -> B.ByteString -> I.Message
privmsg targets x = I.Message Nothing I.PRIVMSG (I.Params targets (Just x))

insertChannel :: B.ByteString -> CTime -> Endomorphism IrcfsState
insertChannel name time st = 
  let 
      k = head (nextDirNames st)
      target = Target k TChannel
      s = B.pack $ "new " ++ show k ++ " "

      emptyNode = setTimes time . chmod 0o440
        $ mkInode (defaultFileStat st)
      rwNode = chmod 0o660 emptyNode
      wNode = chmod 0o220 emptyNode
      nameNode = L.setL iDataL name  emptyNode
      dirNode = mkInode (defaultDirStat st)

      insert = 
              L.setL (targetLens k) (Just target)
            . L.setL (targetMapLens' name) (Just k)

            . L.setL (inodeL (Qname k)) (Just nameNode)
            . L.setL (inodeL (Qusers k)) (Just emptyNode)
            . L.setL (inodeL (Qdata k)) (Just rwNode)
            . L.setL (inodeL (Qctl k)) (Just wNode)
            . L.setL (inodeL (Qdir k)) (Just dirNode)

            . append Qevent (s `mappend` name `mappend` "\n")
            . L.modL nextDirNamesLens tail
  in  insert st

removeChannel :: B.ByteString -> CTime -> Endomorphism IrcfsState
removeChannel name time st =
  let
      str k = B.pack $ "del " ++ show k ++ " "
      text k = (mconcat [str k,name,"\n"])
      del k = rmdir (Qdir k)
            . L.setL (targetMapLens' name) Nothing
            . rm (Qname k) . rm (Qusers k) . rm (Qdata k) . rm (Qctl k)
            . append Qevent (text k)
            . L.modL nextDirNamesLens (k:)
  in  maybe st (\k -> del k st) 
                (L.getL (targetMapLens' name) st)

-- writeF :: FilePath -> S.ByteCount -> B.ByteString -> Ircfs [I.Message]

-- (B.ByteString -> Maybe a, a -> B.ByteString)

rm :: Qreq -> Endomorphism IrcfsState
rm q = L.setL (inodeL q) Nothing

rmdir :: Qreq -> Endomorphism IrcfsState
rmdir (Qdir k) =  L.setL (targetLens k) Nothing . rm (Qdir k)
rmdir _ = id

rmdir' :: Qreq -> Endomorphism IrcfsState
rmdir' (Qdir k) = rmdir (Qdir k) . rm (Qname k) . rm (Qusers k) 
                . rm (Qdata k) . rm (Qctl k)
rmdir' _ = id

stat :: IrcfsState -> Qreq -> Maybe F.FileStat
--stat st RootCtl = Just $ defaultFileStat st
stat st p = statFromInode <$> M.lookup p (inodes st)

statFromInode (Inode st d) = 
  if F.statEntryType st == F.Directory then st 
  else st { F.statFileSize = fromIntegral (B.length d) }

