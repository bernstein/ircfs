{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import qualified Data.ByteString.Char8 as B
import Foreign.C.Error
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO
import System.FilePath (splitFileName)
import System.Environment (withArgs)
import qualified System.Fuse as F
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (maybe)
import qualified Control.Concurrent.Chan as C
import qualified Control.Concurrent as C
import qualified Network.IRC.Message as I
import Data.Attoparsec as A
import Data.Attoparsec.Enumerator as A
import qualified Network.Socket.Enumerator as E
import qualified Data.Enumerator as E hiding (drop)
import qualified Data.Enumerator.List as EL

import qualified Network.Socket as N hiding (recv)
import qualified Network.Socket.ByteString  as N (recv, sendAll)

import Ircfs.Ctl as I
import Ircfs.Types

--type FileInfoHandler = FilePath -> IO (Maybe FsObject)
data IrcfsFileHandle = IrcfsFileHandle
--data Eircfs = Ebadctl | 

msgToByteString :: I.Message -> B.ByteString
msgToByteString (I.Message Nothing (I.CmdNumericReply _) ps) = 
                                                        error "msgToByteString"
msgToByteString (I.Message Nothing (I.CmdString _) ps) = error "msgToByteString"
msgToByteString (I.Message Nothing cmd ps) = 
  (B.pack (show cmd)) `B.append` " " `B.append` (B.unwords ps) `B.append` "\r\n"

chanToIter2 :: C.Chan a -> E.Iteratee a IO ()
chanToIter2 chan = go
  where
    go = EL.head >>= maybe go (\x -> liftIO (C.writeChan chan x) >> go)

iterMessage :: Monad m => E.Iteratee B.ByteString m I.Message
iterMessage = A.iterParser I.message

messages :: Monad m => E.Enumeratee B.ByteString I.Message m a
messages = E.sequence iterMessage

doIRC sock (I.Message _ I.PING ps) = do
              liftIO $ N.sendAll sock $ "PONG " `B.append` (B.intercalate "," ps) `B.append`  "\r\n"
              liftIO $ putStrLn "send PONG"

ircReader :: N.Socket -> C.Chan I.Message -> String -> String -> IO ()
ircReader sock ircinc nick pass = do
  --N.sendAll sock $ B.pack $ "NICK " ++ (nick cfg) ++ "\r\n"
  --N.sendAll sock $ B.pack $ "USER none 0 * :" ++ (nick cfg)++"\r\n"
  x <- E.run $ E.enumSocket 1024 sock E.$$ messages E.=$ chanToIter2 ircinc --printChunks True
  return ()

ircWriter :: N.Socket -> C.Chan B.ByteString -> IO ()
ircWriter sock ircoutc = do
    ms <- C.getChanContents ircoutc
    mapM_ f ms
    return ()
  where
    f s = do
       print s
       case A.parse I.parseCtl s of
         A.Fail {} -> return ()
         A.Partial {} -> return ()
         A.Done _ (I.Unknown {}) -> return ()
         A.Done _ c -> do
           let m = msgToByteString (toMessage c)
           N.sendAll sock m

fsInit :: Config -> C.Chan B.ByteString -> IO ()
fsInit cfg ircoutc = do
  addrinfos <- N.getAddrInfo Nothing (Just (addr cfg)) (Just (port cfg))
  let serveraddr = head addrinfos
  sock <- N.socket (N.addrFamily serveraddr) N.Stream N.defaultProtocol
  N.connect sock (N.addrAddress serveraddr)

  ircinc <- C.newChan
  ms <- C.getChanContents ircinc
  C.forkIO (ircReader sock ircinc (nick cfg) (secret cfg))

  N.sendAll sock $ B.pack $ "NICK " ++ (nick cfg) ++ "\r\n"
  N.sendAll sock $ B.pack $ "USER none 0 * :" ++ (nick cfg)++"\r\n"
  C.forkIO $ ircWriter sock ircoutc

  C.forkIO (mapM_ (doIRC sock) ms)
  return ()

fsDestroy :: IO ()
fsDestroy = do
  return ()

main :: IO ()
main = N.withSocketsDo $ do

  args <- cmdLine

  ircoutc <- C.newChan
  --ircerrc <- C.newChan

  let ops = F.defaultFuseOps {
              F.fuseInit = fsInit args ircoutc
            , F.fuseDestroy = fsDestroy
            , F.fuseGetFileStat = fsFileStat
            , F.fuseReadDirectory = fsReadDir
            , F.fuseOpenDirectory = fsOpenDirectory
            , F.fuseOpen = fsOpen
            , F.fuseWrite = fsWrite ircoutc
            , F.fuseSetFileSize = fsTruncate
            }
  withArgs [mtpt args] $ F.fuseMain ops F.defaultExceptionHandler

data Target = Target

ctl :: C.Chan B.ByteString -> B.ByteString -> Target -> IO ()
ctl ircoutc s t = do
  C.writeChan ircoutc s
  -- writemsg --> 
  --   writeraw --> 
  --     ircoutc
  --     writefile rawFile
 
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
                    [(".", defaultDirStat) ,("..", defaultDirStat) ] ++ rootDir
  where
    rootDir = zip (map showFilepath rootDirFiles) (map fileStat rootDirFiles)
fsReadDir p = return . Right $ 
                    [(".", defaultDirStat), ("..",defaultDirStat)] ++ subDir
  where
    subDir = zip (map showFilepath files) (map fileStat files)
--fsReadDir _ = return (Left (eNOENT))

fsOpenDirectory :: FilePath -> IO Errno
fsOpenDirectory  "/" = return eOK
fsOpenDirectory  p = return eOK

fsOpen :: FilePath -> OpenMode -> OpenFileFlags -> 
              IO (Either Errno IrcfsFileHandle)
fsOpen p m f 
  | p == "/ctl" = return (Right IrcfsFileHandle)
  | otherwise = return (Left eNOENT)

fsWrite :: C.Chan B.ByteString -> FilePath -> IrcfsFileHandle -> 
          B.ByteString -> FileOffset -> IO (Either Errno ByteCount)
fsWrite c p h s off
  | p == "/ctl" = do
                  ctl c s Target
                  return . Right . fromIntegral . B.length $ s
  | otherwise = return (Left eNOENT)

fsTruncate :: FilePath -> FileOffset -> IO Errno
fsTruncate p off
  | p == "/ctl" = return eOK
  | otherwise = return eACCES

fsFileStat :: FilePath -> IO (Either Errno F.FileStat)
fsFileStat p = return $ maybe (Left eNOENT) (Right. fileStat) (fromFilePath p)

data Qreq = Qroot
          | Qrootctl
          | Qevent
          | Qraw
          | Qnick
          | Qpong
          | Qdir
          | Qctl
          | Qname
          | Qusers
          | Qdata
  deriving (Show,Eq,Ord)

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

modeFile :: Qreq -> FileMode
modeFile Qroot    = 0o555 -- DIR
modeFile Qrootctl = 0o222
modeFile Qevent   = 0o444
modeFile Qraw     = 0o666
modeFile Qnick    = 0o444
modeFile Qpong    = 0o444
modeFile Qdir     = 0o555 -- DIR
modeFile Qctl     = 0o222
modeFile Qname    = 0o444
modeFile Qusers   = 0o444
modeFile Qdata    = 0o666

fileStat :: Qreq -> F.FileStat
fileStat Qroot    = defaultDirStat  { F.statFileMode = modeFile Qroot }
fileStat Qrootctl = defaultFileStat { F.statFileMode = modeFile Qrootctl }
fileStat Qevent   = defaultFileStat { F.statFileMode = modeFile Qevent }
fileStat Qraw     = defaultFileStat { F.statFileMode = modeFile Qraw }
fileStat Qnick    = defaultFileStat { F.statFileMode = modeFile Qnick }
fileStat Qpong    = defaultFileStat { F.statFileMode = modeFile Qpong }
fileStat Qdir     = defaultDirStat  { F.statFileMode = modeFile Qroot }
fileStat Qctl     = defaultFileStat { F.statFileMode = modeFile Qrootctl }
fileStat Qname    = defaultFileStat { F.statFileMode = modeFile Qname }
fileStat Qusers   = defaultFileStat { F.statFileMode = modeFile Qusers }
fileStat Qdata    = defaultFileStat { F.statFileMode = modeFile Qdata }

toQreq :: String -> Maybe Qreq
toQreq "ctl" = Just Qctl
toQreq "event" = Just Qevent
toQreq "nick" = Just Qnick
toQreq "raw" = Just Qraw
toQreq "pong" = Just Qpong
toQreq "data" = Just Qdata
toQreq "name" = Just Qname
toQreq "users" = Just Qusers
toQreq _        = Nothing

fromFilePath :: FilePath -> Maybe Qreq
fromFilePath "/" = Just Qroot
fromFilePath "/ctl" = Just Qrootctl
fromFilePath p = toQreq f
    where (d,f) = splitFileName p

rootDirFiles :: [Qreq]
rootDirFiles = [Qctl, Qname, Qdata, Qusers]

files :: [Qreq]
files = [ Qroot, Qrootctl, Qevent, Qraw, Qnick, Qpong, Qdir, Qctl, Qname
        , Qusers, Qdata
        ]

defaultFileStat = F.FileStat 
                { F.statEntryType = F.RegularFile
                , F.statFileMode = 0o222
                , F.statLinkCount = 1
                , F.statFileOwner = 0
                , F.statFileGroup = 0
                , F.statSpecialDeviceID = 0
                , F.statFileSize = 0
                , F.statBlocks = 1
                , F.statAccessTime = 0
                , F.statModificationTime = 0
                , F.statStatusChangeTime = 0
                }
defaultDirStat = defaultFileStat
               { F.statEntryType = F.Directory
               , F.statFileMode = 0o555
               , F.statLinkCount = 2
               , F.statFileSize = 4096
               }

