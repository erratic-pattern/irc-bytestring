module Network.IRC.ByteString (
     -- |Configuration
     IRCParserConfig, rfc1459Conf, rfc2812Conf
     -- |IRC message types
   , ServerName, IRCMsg(..), UserInfo(..)
     -- |Conversion functions
   , toIRCMsg, fromIRCMsg
) where

import Network.IRC.ByteString.Parser
import Network.IRC.ByteString.Config
import Network.IRC.ByteString.RFC1459
import Network.IRC.ByteString.RFC2812