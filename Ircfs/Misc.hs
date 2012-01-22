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
--  , now
--  ) where
where

import Prelude hiding ((.), id)
import Control.Category
import Foreign.C.Types (CTime)
import Control.Applicative
import qualified Data.Time as T
import qualified Data.Time.Clock.POSIX as T
import System.Locale (defaultTimeLocale)
import Data.List (partition)

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
