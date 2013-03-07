{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Network.IRC.ByteString.Conduit
       ( -- |IRC sources and sinks
         IRCSource, IRCSink, IRCNode
       , sourceIRC, sinkIRC
         -- |IRC conduits
       , ircParseInput, ircSerializeOutput
         -- |IRC clients
        ,IRCClientSettings, runIRCClient
        , main
        ) where
import Network.IRC.ByteString.Parser
import Network.Socket
import Data.Conduit as C
import Data.Conduit.List as CL
import Data.Conduit.Network
import Data.Conduit.Attoparsec
import Data.ByteString.Char8 as BS (ByteString, append, putStr, unwords, cons)

import Control.Monad.IO.Class
import Control.Monad.Trans.Control

import Data.Function
import Data.Word

type IRCSource m = Source m IRCMsg
type IRCSink   m = Sink IRCMsg m ()

type IRCNode m = IRCSource m -> IRCSink m -> m ()


data IRCClientSettings = IRCClientSettings { ircHost :: ByteString
                                           , ircPort :: Word16
                                           , ircNick :: ByteString
                                           , ircUser :: ByteString
                                           , ircRealName :: ByteString
                                           , ircPass :: Maybe ByteString
                                           }
                       deriving (Eq, Show, Read)

data IRCServerSettings = IRCServerSettings { serverHost :: HostPreference
                                           , serverPort :: Word16
                                           }
                       deriving (Eq, Show)
--Pipes--
sourceIRC :: (MonadIO m, MonadThrow m) => Socket -> IRCSource m
sourceIRC s = sourceSocket s $= ircParseInput

sinkIRC :: MonadIO m => Socket -> IRCSink m
sinkIRC s = ircSerializeOutput =$ sinkSocket s

ircParseInput :: (MonadIO m, MonadThrow m) => Conduit ByteString m IRCMsg
ircParseInput = CL.sequence (sinkParser ircLine)

ircSerializeOutput :: MonadIO m => Conduit IRCMsg m ByteString
ircSerializeOutput = CL.map fromIRCMsg

      
printOutput :: MonadIO m => Conduit IRCMsg m IRCMsg 
printOutput = CL.mapM $ \msg -> do
                                liftIO $ print msg
                                return msg
                      
outRaw :: MonadIO m => Conduit ByteString m ByteString
outRaw = CL.mapM $ \msg -> liftIO (BS.putStr msg) >> return msg


--todo: send pass/user/nick
runIRCClient :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) => 
       IRCClientSettings -> IRCNode m -> m ()
runIRCClient s client = 
  runTCPClient (clientSettings (fromIntegral (ircPort s)) (ircHost s))
    $ \appData -> 
      let src = appSource appData
          snk = appSink appData
          inp = src $= ircParseInput
          out = (connect >> ircSerializeOutput) =$ snk  
      in client inp out
  where
    connect = do
      case ircPass s of
        Just pass -> yield $ "PASS " `append` pass
        Nothing   -> return ()
      yield $ BS.unwords ["USER", ircUser s, "* *", ':' `cons` ircRealName s]
      yield $ "NICK " `append` ircNick s

main = do
  runIRCClient IRCClientSettings {ircHost = "irc.rizon.net"
                                 , ircPort = 6667
                                 , ircNick = "ero"
                                 , ircUser = "bot"
                                 , ircRealName = "."
                                 , ircPass = Nothing 
                                 }
    $ \src snk -> ((src) $$ (test =$ snk) :: IO ())
    where
      test = do
        fix $ \p -> do
          msg <- await
          case msg of
            Just (IRCMsg {msgCmd = "376"}) -> return ()
            Nothing -> return ()
            other -> p
        yield $ msg "JOIN" ["#miuchan"] ""
        yield $ msg "PRIVMSG" ["#miuchan"] "Hello, World!"
        yield $ msg "QUIT" [] ""
        where
          msg cmd params trail = IRCMsg Nothing cmd params trail
        

