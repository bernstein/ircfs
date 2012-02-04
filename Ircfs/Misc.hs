{-# LANGUAGE OverloadedStrings, BangPatterns #-}
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
module Ircfs.Misc
--  ( 
--  , timeStamp
--  , now
--  ) where
where

import Prelude hiding ((.), id)
import Control.Category
import Foreign.C.Types (CTime)
import Control.Applicative
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.Time as T
import qualified Data.Time.Clock.POSIX as T
import System.Locale (defaultTimeLocale)
import qualified Data.ByteString.Lazy.Char8 as BL hiding (map)
import qualified Data.ByteString.Lazy as BL hiding (elemIndex,head)
import qualified Data.ByteString.Char8 as B
import           Data.List (partition)
import           Data.Monoid
import           Data.IORef
import qualified Data.Time as T
import System.Locale (defaultTimeLocale)
import Foreign.C.Types (CTime)

type Endomorphism a = a -> a

now :: IO CTime
now = fromIntegral . truncate <$> T.getPOSIXTime
-- T.posixSecondsToUTCTime . realToFrac <$> now

toUTCTime :: CTime -> T.UTCTime
toUTCTime = T.posixSecondsToUTCTime . realToFrac

stamp :: T.UTCTime -> String
stamp = T.formatTime defaultTimeLocale "%H:%M"

stamp' :: T.TimeZone -> CTime -> String
stamp' z = T.formatTime defaultTimeLocale "%H:%M" . T.utcToLocalTime z . toUTCTime

minfree :: [Int] -> Int
minfree xs = minfrom 0 (length xs,xs)

minfrom a (n,xs)
  | n == 0 = a
  | m == b - a = minfrom b (n-m, vs)
  | otherwise = minfrom a (m, us)
    where (us,vs) = partition (<b) xs
          b = a + 1 + n `div` 2
          m = length us

breakAfter :: BL.ByteString -> BL.ByteString -> (BL.ByteString, BL.ByteString)
breakAfter str bs | BL.null str = (bs,mempty)
breakAfter str bs = find 0 bs
  where find n xs =
          case BL.elemIndex c xs of
            Nothing -> (bs,mempty)
            Just i ->
                    let (_,rest) = BL.splitAt (n+i) bs
                    in if str `BL.isPrefixOf` rest then BL.splitAt (n+i+2) bs
                       else find (n+i+1) rest
        c = BL.head str

split :: BL.ByteString -> BL.ByteString -> [BL.ByteString]
split e bs = xs : if BL.null ys then [] else split e ys
     where (xs,ys) = breakAfter e bs

breakAfterCRLF :: BL.ByteString -> (BL.ByteString, BL.ByteString)
breakAfterCRLF bs = find 0 bs
  where find n xs =
          case BL.elemIndex '\r' xs of
            Nothing -> (bs,mempty)
            Just i ->
                    let (_,rest) = BL.splitAt (n+i) bs
                    in if ("\r\n") `BL.isPrefixOf` rest then BL.splitAt (n+i+2) bs
                       else find (n+i+1) rest

timeStamp :: MonadIO m => m B.ByteString
timeStamp = do
  now <- liftIO T.getCurrentTime
  return . B.pack $ T.formatTime defaultTimeLocale "%H:%M" now

timeStamp' :: CTime -> B.ByteString
timeStamp' = B.pack . stamp . toUTCTime

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ref f = do
  !_ <- atomicModifyIORef ref (\st -> (f st,()))
  return ()
