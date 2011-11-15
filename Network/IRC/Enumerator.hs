{-# LANGUAGE OverloadedStrings #-}
module Network.IRC.Enumerator
where

import qualified Network.Socket as N hiding (recv)
import qualified Data.Enumerator as E hiding (drop)
import qualified Data.Enumerator.Binary as EB
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.Enumerator as A

import qualified Network.IRC.Message as I

getSocket :: String -> Int -> IO N.Socket
getSocket host port = do
  addrinfos <- N.getAddrInfo Nothing (Just host) (Just $ show port)
  let serveraddr = head addrinfos
  sock <- N.socket (N.addrFamily serveraddr) N.Stream N.defaultProtocol
  N.connect sock (N.addrAddress serveraddr)
  return sock

-- XXX E.catchError
iterMessage :: Monad m => E.Iteratee B.ByteString m I.Message
iterMessage = A.iterParser I.message

messages :: Monad m => E.Enumeratee B.ByteString I.Message m a
messages = E.sequence iterMessage

irclines :: Monad m => E.Enumeratee B.ByteString B.ByteString m a
irclines = EB.splitWhen (== 10) -- '\n' == chr 10

{-
catchParser :: Monad m => String -> Iteratee a m b -> Iteratee a m b
catchParser s i = catchError i (const $ throwError $ IrcParserException s)
data IrcException = StatusCodeException Int L.ByteString
                  | InvalidUrlException String String
                  | TooManyRedirects
                  | IrcParserException String
    deriving (Show, Typeable)
instance Exception IrcException
-}


