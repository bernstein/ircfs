module Filesystem.IRC.Target
where

data Target = Target
  { targetId :: Int
  , name :: String
  , lname :: String
  , ischan :: Int
  --, logfd :: Handle
  , tdata :: [Fidfile]
  , users :: [Fidfile]
  --, hist :: [String]
  , histlength :: Int
  , histfirst :: Int
  , histbegin :: Int
  , histend :: Int
  , joined :: [String]
  , newjoined :: [String]
  , eof :: Int
  , opens :: Int
  , remove :: Int
  , prevaway :: String
  , mtime :: Int
  }

data Fid = Fid Int

data Fidfile = Fidfile 
  { fid :: Fid
  , histoff :: Int
  , histo :: Int
  --, reads :: [Tmsg]
  , a :: [String]
  , singlebuf :: Int
  }
