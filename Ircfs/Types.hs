{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
-- -----------------------------------------------------------------------------
-- |
-- Module      :  Ircfs.Types
-- Copyright   :  (c) Andreas-Christoph Bernstein 2011
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  andreas.bernstein@googlemail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- Used Types
--
--------------------------------------------------------------------------------

module Ircfs.Types
where

import System.Console.CmdArgs -- (Data, Typeable)

showVersion = "0.0.1"

data Config = Config 
            { nick :: String
            , fromhost :: String
            , addr :: String
            , port :: String
            , secret :: String
            , logpath :: FilePath
            , mtpt :: FilePath
            } deriving (Show, Data, Typeable)

conf = Config
  { nick = def &= args &= typ "NICKNAME"
  , fromhost = def &= help "fromhost" &= typ "FROMHOST"
  , addr = "irc.freenode.net" &= help "server" &= typ "SERVER"
  , port = "6667" &= help "port" &= typ "PORT"
  , secret = def &= help "port number" &= typ "PASSWORD"
  , logpath = def &= help "Path to logfile" &= typFile
  , mtpt = "/mnt/irc" &= help "Mount point" &= typDir
  } &= summary ("ircfs v" ++ showVersion ++ ", (C) Andreas-C. Bernstein 2011\n") &= program "ircfs"

cmdLine :: IO Config
cmdLine = cmdArgs conf
