{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Network.IRC.ByteString.Parser
       ( -- |Parser configuration
         IRCParserConfig
         -- |IRC message types
       , ServerName, IRCMsg(..), UserInfo(..)
         -- |Conversion functions
       , toIRCMsg, fromIRCMsg
         -- |Attoparsec parser
       , ircLine
       ) where
import Data.Attoparsec.Char8 as Char8
import qualified Data.Attoparsec.ByteString as Word8
import Data.ByteString.Char8 as BS
  (cons, append, intercalate, ByteString, concat)
import Data.Maybe (fromMaybe)
import Data.Char (isNumber, isAlpha)
import Control.Applicative
import Prelude hiding (takeWhile)

import Network.IRC.ByteString.Config
import Network.IRC.ByteString.Utils

--Types--
type ServerName = ByteString

data IRCMsg = IRCMsg { msgPrefix  :: Maybe (Either UserInfo ServerName)
                     , msgCmd     :: ByteString
                     , msgParams  :: [ByteString]
                     , msgTrail   :: Maybe ByteString
                     }
            deriving (Show, Eq, Read)
            
            
data UserInfo = UserInfo { userNick  :: ByteString
                         , userName  :: Maybe ByteString
                         , userHost  :: Maybe ByteString
                         }
              deriving (Eq, Show, Read)

fromIRCMsg :: IRCMsg -> ByteString
fromIRCMsg msg = BS.concat $ [prefix', command', params', trail', "\r\n"]
  where prefix' = case msgPrefix msg of
          Nothing -> ""
          Just (Right serv) -> ':' `cons` serv `append` " "
          Just (Left info)  -> ':' `cons` userNick info
                               `append` maybeUser
                               `append` maybeHost
                               `append` " "
            where 
              maybeUser  = maybe ""  ('!' `cons`) (userName info)
              maybeHost  = maybe "" ('@' `cons`) (userHost info)
      
        command' = msgCmd msg            
        
        paramList = msgParams msg
        params'
          | Prelude.null paramList = ""
          | otherwise = ' ' `cons` intercalate " " (msgParams msg)
      
        trail' = case msgTrail msg of
            Nothing -> ""
            Just t -> " :" `append` t
          
toIRCMsg :: IRCParserConfig -> ByteString -> Result IRCMsg
toIRCMsg = parse . ircLine 
          

-- |zero or more spaces as defined by 'isSpace'
ircSpaces c = skipWhile (isIRCSpace c) <?> "optional space"

-- |one or more spaces as defined by 'isSpace'
ircSpaces1 c = satisfy (isIRCSpace c) 
               >> skipWhile (isIRCSpace c) 
               <?> "required space"

               
ircLine conf = IRCMsg <$> optional (prefix conf) <*> command <*> params conf <*> trail conf <* skipSpace
          <?> "IRC line" 
          
prefix conf = char ':' 
              *> eitherP (userInfo <* end) (serverName <* end)
              <?> "prefix"
  where
    end = ircSpaces1 conf <|> endOfInput
    nick = nickParser conf <?> "nick"
    user = userParser conf <?> "user"
    host = hostParser conf <?> "host"
    serverName = host <?> "server name"
    userInfo = UserInfo <$> nick
                        <*> optional (char '!' >> user)
                        <*> optional (char '@' >> host)
                        <?> "user info"
                        
command = (alpha <|> numeric) <?> "command name"        
  where
    alpha = takeWhile1 isAlpha
    numeric = n <:> n <:> n <:> pure ""
        where n = satisfy isNumber


params conf = fromMaybe [] <$> optional 
              (ircSpaces1 conf >> paramParser conf `sepBy` ircSpaces1 conf) 
              <?> "params list"

trail conf = ircSpaces conf
             >> optional (char ':' >> Word8.takeWhile (not . isEndOfLine))
             <?> "message body"
 


          