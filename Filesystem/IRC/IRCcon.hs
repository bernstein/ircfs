{-# LANGUAGE OverloadedStrings #-}
module Filesystem.IRC.IRCcon
where

import Network.IRC.Message
import qualified Data.ByteString.Char8 as B

data Irccon = Irccon {
  -- fd :: FD
  -- , b :: Iobuf
    addr :: String
  , nick :: String
  , lnick :: String
  , server :: String
  }
  deriving (Show, Eq, Ord)

data Replies =
--  5.  Replies
--  5.1  Command responses
--     Numerics in the range from 001 to 099 are used for client-server
--     connections only and should never travel between servers.  Replies
--     generated in the response to commands are found in the range from 200
--     to 399.
    Unknown
  deriving (Show, Eq, Ord)

{-
pack :: ClientMsg -> B.ByteString
pack (Pass p)        = B.pack $ "PASS " ++ show p ++ "\r\n"
pack (Nick s)        = B.pack $ "NICK " ++ show s ++ "\r\n"
pack (User u m r)    = B.pack $ "USER " ++ show u ++ " " ++ show m ++ " * :" ++ show r ++ "\r\n"
--pack (User u m r)    = B.pack $ "USER none 0 * :" ++ show r ++ "\r\n"
pack (Mode n ms)     = let modes = foldr (\m s -> show m ++ show " " ++ s) [] ms
                       in  B.pack $ "MODE " ++ show n ++ " " ++ modes ++ "\r\n"
pack (Quit m)        = B.pack $ "QUIT :" ++ show m ++ "\r\n"
pack (Join c)        = B.pack $ "JOIN " ++ show c ++ "\r\n"
pack (Part m)        = B.pack $ "PART " ++ show m ++ "\r\n"
pack (Names m)       = B.pack $ "NAMES " ++ show m ++ "\r\n"
pack (Privmsg t m)   = B.pack $ "PRIVMSG " ++ show t ++ " :" ++ show m ++ "\r\n"
pack (Notice t m)    = B.pack $ "NOTICE " ++ show t ++ " :" ++ show m ++ "\r\n"
pack (Motd Nothing)  = B.pack $ "MOTD " ++ "\r\n"
pack (Motd (Just t)) = B.pack $ "MOTD " ++ show t ++ "\r\n"
pack (Time Nothing)  = B.pack $ "TIME " ++ "\r\n"
pack (Time (Just t)) = B.pack $ "TIME " ++ show t ++ "\r\n"
pack (Who s)         = B.pack $ "WHO " ++ show s ++ "\r\n"
pack (Ping s)        = B.pack $ "PING " ++ show s ++ "\r\n"
pack x               = error $ "IRCcon.pack: no match for " ++ show x
-}

-- parse :: B.ByteString -> ClientMsg
-- checkParse :: B.ByteString -> B.ByteString
-- writeMsg :: Irccon -> ClientMsg -> IO String
--
-- newIrccon :: String -> String -> String -> String -> IO Irccon
-- newIrccon addr nick name pass = do
--   c = Irccon fd b addr nick (lowercase nick) ""
--   if not (null pass) then writeMsg c (Pass pass)
--     else return ()
--   writeMsg c (Nick nick)
--   writeMsg c (User none 0 name)
--   return c

-- readMsg :: Irccon -> IO Message
-- readMsg c = undefined
--
-- parse :: B.ByteString -> RecvMessage
--
