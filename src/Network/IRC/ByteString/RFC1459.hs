{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Network.IRC.ByteString.RFC1459 (
    rfc1459Conf
    -- |parsers
    , channel, nick, user, host
    -- |predicates
    , Network.IRC.ByteString.RFC1459.isIRCSpace
) where

import Data.Attoparsec.Char8 as Char8
import Data.Char

import Network.IRC.ByteString.Utils
import Network.IRC.ByteString.HostParser
import Network.IRC.ByteString.Config
import Prelude hiding (takeWhile)


-- |Only the octet 0x20 is defined as a valid space.
-- > <SPACE>    ::= ' ' { ' ' } 
isIRCSpace = (== ' ')

-- | > <nonwhite>   ::= <any 8bit code except SPACE (0x20), NUL (0x0), CR (0xd), and LF (0xa)>
isNonWhite c = c /= ' ' && c /= '\r' && c /= '\n' && c /= '\0'

-- |Tests if a character is a valid channel prefix.
-- '#' and '$' are the only valid channel prefix characters
isChanPrefix c = c == '#' || c  == '$'

-- |Tests if a character is valid in a channel string.
-- > <chstring>   ::= <any 8bit code except SPACE, BELL, NUL, CR, LF and comma (',')>
isChanChar c = isNonWhite c && c /= '\x007' && c /= ','

-- |Tests if a character is valid as the first character in a nick string.
isFirstNickChar = isAlphaNum

-- |Tests if a character is valid as a non-first character in a nick string.
isNickChar c = isAlphaNum c || isSpecial c

-- |Tests if a character is special.
-- > <special>    ::= '-' | '[' | ']' | '\' | '`' | '^' | '{' | '}'
isSpecial c = c == '-' || c == '[' || c == ']' || c == '\\' || c == '`'
              || c == '^' || c == '{' || c == '}' || c == '_'

-- |Tests if a character is valid in a user string              
isUserChar c = isNonWhite c && c /= '@'
       

-- | RFC1459 channel name.
--
-- @
--     <channel>    ::= ('#' | '&') <chstring>
--     <chstring>   ::= <any 8bit code except SPACE, BELL, NUL, CR, LF and comma (',')>
-- @
channel = prefix <:> name
    where prefix = satisfy isChanPrefix <?> "RFC1459 channel prefix"
          name = Char8.takeWhile isChanChar   <?> "RFC1459 channel name"

          


-- |RFC1459 nick name
--
-- @
--     <nick>       ::= <letter> { <letter> | <number> | <special> }
--     <letter>     ::= 'a' ... 'z' | 'A' ... 'Z'
--     <number>     ::= '0' ... '9'
--     <special>    ::= '-' | '[' | ']' | '\' | '`' | '^' | '{' | '}'
-- @
nick = satisfy isFirstNickChar <:> Char8.takeWhile isNickChar 
       <?> "RFC1459 nick"

user = takeWhile1 isUserChar 
       <?> "RFC1459 username" 

param = satisfy (\c -> isNonWhite c && c /= ':')
                <:> Char8.takeWhile isNonWhite      
       
rfc1459Conf = IRCParserConfig
    { nickParser = nick
    , userParser = user
    , hostParser = host
    , paramParser = param
    , maxParams = Nothing
    , Network.IRC.ByteString.Config.isIRCSpace = Network.IRC.ByteString.RFC1459.isIRCSpace
    }