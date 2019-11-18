{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Chrome.Target where

import           Control.Retry
import           Data.Aeson
import           Data.Maybe

import           Network.HTTP.Client        (defaultManagerSettings, httpLbs,
                                             newManager, parseRequest,
                                             responseBody)
import           Network.URL

import           Chrome.Target.DebuggingURL

data Target = Target { _targetId            :: String
                     , _targetTitle         :: String
                     , _targetWSDebuggerURL :: DebuggingURL
                     } deriving Show

instance FromJSON Target where
  parseJSON = withObject "target" $ \o -> Target
                <$> o .: "id"
                <*> o .: "title"
                <*> o .: "webSocketDebuggerUrl"

newtype BrowserTarget = BrowserTarget { webSocketDebuggerUrl :: DebuggingURL } deriving Show

instance FromJSON BrowserTarget where
  parseJSON = withObject "browserTarget" $ \o -> BrowserTarget
                <$> o .: "webSocketDebuggerUrl"

fetchBrowserTarget :: String -> IO (Maybe Target)
fetchBrowserTarget url = do
  req <- parseRequest $ url ++ "/json/version"
  manager <- newManager defaultManagerSettings
  res <- recoverAll (constantDelay 50000 <> limitRetries 100) (const (httpLbs req manager))
  let brw = decode . responseBody $ res :: Maybe BrowserTarget
  return $ Target "" "Browser" . webSocketDebuggerUrl <$> brw

fetchTargets :: String -> IO (Maybe [Target])
fetchTargets url = do
  putStrLn "Trying to talk to Chrome"
  req <- parseRequest $ url ++ "/json"
  manager <- newManager defaultManagerSettings
  res <- recoverAll (constantDelay 50000 <> limitRetries 100) (const (httpLbs req manager))
  return . decode . responseBody $ res

type WSTargetSettings = (String, Integer, String)

wsClientFromTarget :: Target -> Maybe WSTargetSettings
wsClientFromTarget Target{..} = let h = debuggingURLHost _targetWSDebuggerURL
                                    domain = host <$> h
                                    port' = fromMaybe 80 . port <$> h
                                    in
                                        (,,) <$> domain <*> port' <*> pure (debuggingURLPath _targetWSDebuggerURL)
