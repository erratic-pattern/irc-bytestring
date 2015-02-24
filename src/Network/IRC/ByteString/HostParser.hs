{-# LANGUAGE OverloadedStrings #-}
-- |Efficient domain name parsing based on RFC112 (https://tools.ietf.org/html/rfc1123)
module Network.IRC.ByteString.HostParser (host) where

import Data.ByteString.Char8 as BS
import Data.Attoparsec.Char8 as Char8
import Data.Char
import Control.Applicative

import Network.IRC.ByteString.Utils

data HostParserState =
      AfterDot
    | AfterDash
    | MidLabel

-- |Efficient domain name parsing based on RFC112 (https://tools.ietf.org/html/rfc1123)
host :: Parser ByteString
host = host' AfterDot
    where
        host' AfterDot = satisfy isAlphaNum <:> host' MidLabel
        host' AfterDash = do
            maybeChar <- peekChar
            case maybeChar of
                Nothing -> fail "End of input reached after '-' in hostname"
                Just '-' -> anyChar <:> host' AfterDash
                Just c
                    | isAlphaNum c -> anyChar <:> host' MidLabel
                    | otherwise -> fail $ "Invalid character '" ++ [c] ++ "' after '-' in hostname" 
        host' MidLabel = do
            maybeChar <- peekChar
            case maybeChar of
                Nothing -> pure ""
                Just '.' -> anyChar <:> host' AfterDot
                Just '-' -> anyChar <:> host' AfterDash
                Just c
                    | isAlphaNum c -> anyChar <:> host' MidLabel
                    | otherwise -> pure ""