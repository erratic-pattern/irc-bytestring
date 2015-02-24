module Network.IRC.ByteString.Config where

import Data.ByteString.Char8
import Data.Attoparsec.Char8 as Char8

data IRCParserConfig = IRCParserConfig 
    { nickParser :: Parser ByteString
    , userParser :: Parser ByteString
    , hostParser :: Parser ByteString
    , paramParser :: Parser ByteString
    , isIRCSpace :: Char -> Bool
    }
