{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- |Parsing based on RFC2812.
--
-- Note the following deviations from the spec:
-- * The nickname length restriction to 9 characters is not followed
-- * The restriction of command syntax to 15 parameters is not followed
--
-- Please yell at the maintainer if you would like one of these things added.
module Network.IRC.ByteString.RFC2812 (
    rfc2812Conf
    -- |parsers
    , channelId, channel, nick, user, host, param
    -- |predicates
    , Network.IRC.ByteString.RFC2812.isIRCSpace, isChanPrefix, isChanChar, isTargetMaskPrefix, isFirstNickChar, isNickChar, isSpecial, isUserChar
) where

import Data.Attoparsec.Char8 as Char8
import Data.Char
import Data.Maybe
import Control.Applicative

import Network.IRC.ByteString.Utils
import Network.IRC.ByteString.HostParser
import Network.IRC.ByteString.Config
import qualified Network.IRC.ByteString.RFC1459 as RFC1459
import Prelude hiding (takeWhile)


-- |Only the octet 0x20 is defined as a valid space.
-- > SPACE     =  %x20        ; space character 
isIRCSpace = (== ' ')

-- |Tests if a character is a valid channel prefix.
-- '#' and '&' are the only valid channel prefix characters
isChanPrefix c = c == '#' || c  == '+' || c == '!' || c == '&'

-- |Tests is a character is a valid target mask prefix
-- '#' and '$' are the only valid characters
isTargetMaskPrefix c = c == '#' || c == '$'

{- |Tests if a character is valid in a channel string.
   @
       chanstring =  %x01-07 / %x08-09 / %x0B-0C / %x0E-1F / %x21-2B
       chanstring =/ %x2D-39 / %x3B-FF
                       ; any octet except NUL, BELL, CR, LF, " ", "," and ":"
   @
-}
isChanChar c = RFC1459.isChanChar c && c /= ':'

-- |Tests if a character is valid as the first character in a nick string.
isFirstNickChar c = isAlpha c || isSpecial c 

-- |Tests if a character is valid as a non-first character in a nick string.
isNickChar c = isAlphaNum c || isSpecial c || c == '-'

{- |Tests if a character is special.
   @
       special    =  %x5B-60 / %x7B-7D
                        ; "[", "]", "\", "`", "_", "^", "{", "|", "}"
   @
-}
isSpecial c = c == '`' || c == '[' || c == ']' || c == '\\' || c == '`'
          || c == '^' || c == '{' || c == '}' || c == '_' || c == '|'

{- |Tests if a character is valid in a user string
   @
       user       =  1*( %x01-09 / %x0B-0C / %x0E-1F / %x21-3F / %x41-FF )
                       ; any octet except NUL, CR, LF, " " and "@"
   @
-}   
isUserChar = RFC1459.isUserChar
   
{- | RFC2812 channel name.

   @
        channel    =  ( "#" / "+" / ( "!" channelid ) / "&" ) chanstring
                      [ ":" chanstring ]
        chanstring =  %x01-07 / %x08-09 / %x0B-0C / %x0E-1F / %x21-2B
        chanstring =/ %x2D-39 / %x3B-FF
                      ; any octet except NUL, BELL, CR, LF, " ", "," and ":"
                      
        channelid  = 5( %x41-5A / digit )   ; 5( A-Z / 0-9 )
   @
-}
channel = (char '!' <:> channelId <++> name 
          <|> prefix <:> name)
          <++> (fromMaybe "" <$> optional (char ':' <:> name))
    where 
        prefix = satisfy isChanPrefix <?> "channel prefix"
        name = Char8.takeWhile isChanChar   <?> "channel name"

-- | RFC2812 5-digit channel ID
--
-- > channelid  = 5( %x41-5A / digit )   ; 5( A-Z / 0-9 )
channelId = d <:> d <:> d <:> d <:> d <:> pure "" <?> "channel ID"
            where d = satisfy isAlphaNum          


{- |RFC2812 nick name (note: length restriction is ignored)

  @
       nickname   =  ( letter / special ) *8( letter / digit / special / "-" )
       letter     =  %x41-5A / %x61-7A       ; A-Z / a-z
       digit      =  %x30-39                 ; 0-9'
       special    =  %x5B-60 / %x7B-7D
                        ; "[", "]", "\", "`", "_", "^", "{", "|", "}"
  @
-}
nick = satisfy isFirstNickChar <:> Char8.takeWhile isNickChar 
       <?> "nick"

{- |RFC2812 user name (see 'isNonWhite')
   @
       user       =  1*( %x01-09 / %x0B-0C / %x0E-1F / %x21-3F / %x41-FF )
                       ; any octet except NUL, CR, LF, " " and "@"
   @
-}
user = RFC1459.user

{- |Parses a single command parameter according to RFC2812

   @
       middle     =  nospcrlfcl *( ":" / nospcrlfcl )
       nospcrlfcl =  %x01-09 / %x0B-0C / %x0E-1F / %x21-39 / %x3B-FF
                       ; any octet except NUL, CR, LF, " " and ":"

   @
-}
param = RFC1459.param

-- |Parse options for adhering to RFC2812
rfc2812Conf = IRCParserConfig
    { nickParser = nick
    , userParser = user
    , hostParser = host
    , paramParser = param
    , Network.IRC.ByteString.Config.isIRCSpace = Network.IRC.ByteString.RFC2812.isIRCSpace
    }