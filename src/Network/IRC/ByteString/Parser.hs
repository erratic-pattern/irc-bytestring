{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Network.IRC.ByteString.Parser
       ( -- |IRC message types
         ServerName, IRCMsg(..), UserInfo(..)
         -- |Conversion functions
       , toIRCMsg, fromIRCMsg, ircMsg
         -- |Attoparsec parser
       , ircLine
       ) where
import Control.Applicative
import Data.Attoparsec.Char8 as Char8
import qualified Data.Attoparsec.ByteString as Word8
import qualified Data.Attoparsec.Text as T
import Data.ByteString.Char8 as BS
  (cons, append, intercalate, ByteString, null, concat)
import Data.Maybe (fromMaybe)
import Data.Char (isAlphaNum, isNumber, isAlpha)
import Prelude hiding (takeWhile)

--Types--
type ServerName = ByteString

data IRCMsg = IRCMsg { msgPrefix  :: Maybe (Either UserInfo ServerName)
                     , msgCmd     :: ByteString
                     , msgParams  :: [ByteString]
                     , msgTrail   :: ByteString
                     }
            deriving (Show, Eq, Read)


data UserInfo = UserInfo { userNick  :: ByteString
                         , userName  :: Maybe ByteString
                         , userHost  :: Maybe ByteString
                         }
              deriving (Eq, Show, Read)


toIRCMsg :: ByteString -> Result IRCMsg
toIRCMsg = parse ircLine

fromIRCMsg :: IRCMsg -> ByteString
fromIRCMsg msg = BS.concat $ [prefix', command', params', trail, "\r\n"]
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
      
        t = msgTrail msg
        trail
          | BS.null t = ""
          | otherwise = " :" `append` msgTrail msg


ircMsg :: ByteString -> [ByteString] -> ByteString -> IRCMsg
ircMsg = IRCMsg Nothing


--Parsers--
spaces = skipWhile isSpaceNonLine          <?> "optional whitespace"
spaces1 = satisfy isSpaceNonLine >> spaces <?> "required whitespace"

isSpaceNonLine = (&&) <$> isSpace <*> not . T.isEndOfLine
isNonWhite c = c /= ' ' && c /= '\r' && c /= '\n' && c /= '\0'

isChanPrefix c = c == '#' || c  == '$'
isChanChar c = isNonWhite c && c /= '\x007' && c /= ','

chan = cons 
       <$> (satisfy isChanPrefix  <?> "channel prefix")
       <*> (takeWhile1 isChanChar <?> "channel name")

isNickChar c = isAlphaNum c || isSpecial c

isSpecial c = c == '-' || c == '[' || c == ']' || c == '\\' || c == '`'
              || c == '^' || c == '{' || c == '}' || c == '_'

nick = BS.cons <$> satisfy isAlpha
               <*> takeWhile isNickChar 
               <?> "nick"

isUserChar c = isNonWhite c && c /= '@'

user = takeWhile1 isUserChar <?> "username"

isHostChar c = isAlphaNum c || c == '.' || c == '-'

host = cons
       <$> satisfy isAlpha
       <*> takeWhile1 isHostChar 
       <?> "hostname"

prefix = char ':' *> eitherP (userInfo <* end) (serverName <* end)
         <?> "prefix"
  where
    end = spaces1 <|> endOfInput
    serverName = host
    userInfo = UserInfo <$> nick 
                        <*> optional (char '!' >> user)
                        <*> optional (char '@' >> host)

command = alpha <|> numeric        
  where
    alpha = takeWhile1 isAlpha
    numeric = 
      cons 
      <$> satisfy isNumber
      <*> (cons
           <$> satisfy isNumber
           <*> (cons 
                <$> satisfy isNumber
                <*> pure "")
           )
      <?> "command name"

params = fromMaybe [] <$> optional 
         (spaces1 >> param `sepBy` spaces1) <?> "params list"
  where param = cons
                <$> satisfy (\c -> isNonWhite c && c /= ':')
                <*> Char8.takeWhile isNonWhite

mess = spaces >> fromMaybe "" <$>
       optional (char ':' >> Word8.takeWhile (not . isEndOfLine))
       <?> "message body"

ircLine = IRCMsg <$> optional prefix <*> command <*> params <*> mess
          <?> "IRC line"
