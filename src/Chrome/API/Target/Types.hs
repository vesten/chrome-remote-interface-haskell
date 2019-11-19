{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Chrome.API.Target.Types
  ( TargetId(..)
  , SessionId(..)
  , BrowserContextId(..)
  , TargetInfo(..)
  , AttachParams(..)
  , CreateParams(..)
  , DeleteParams(..)
  , ReceivedMessageEvent(..)
  , TargetCrashedEvent(..)
  , AutoAttachParams(..)
  , MessageParams(..)
  ) where

import           Data.Aeson
import           Data.Aeson.TH

newtype TargetId = TargetId String
                  deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''TargetId)

newtype SessionId = SessionId String
                  deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''SessionId)

newtype BrowserContextId = BrowserContextId String
                  deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''BrowserContextId)

data TargetInfo
  = TargetInfo
    { targetId      :: TargetId
    , targetType     :: String
    , title :: String
    , url :: String
    , attached :: Bool
    , openerId :: Maybe TargetId
    , browserContextId :: Maybe BrowserContextId
    } deriving Show

instance FromJSON TargetInfo where
  parseJSON = withObject "targetInfo" $ \o -> TargetInfo
                                              <$> o .: "targetId"
                                              <*> o .: "type"
                                              <*> o .: "title"
                                              <*> o .: "url"
                                              <*> o .: "attached"
                                              <*> o .:? "openerId"
                                              <*> o .:? "browserContextId"

data AttachParams
  = AttachParams
    { targetId :: TargetId
    , flatten :: Maybe Bool
    } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''AttachParams)

data CreateParams
  = CreateParams
    { url :: String
    , width :: Maybe Int
    , height :: Maybe Int
    , browserContextId :: Maybe BrowserContextId
    , enableBeginFrameControl :: Maybe Bool
    , newWindow :: Maybe Bool
    , background :: Maybe Bool
    } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''CreateParams)

data DeleteParams
  = DeleteParams
    { targetId :: Maybe TargetId
    , sessionId :: Maybe SessionId
    } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''DeleteParams)

data ReceivedMessageEvent
  = ReceivedMessageEvent
    { sessionId :: SessionId
    , message :: String
    , targetId :: Maybe TargetId
    } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''ReceivedMessageEvent)

data TargetCrashedEvent
  = TargetCrashedEvent
    { targetId :: TargetId
    , status :: String
    , errorCode :: Int
    } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''TargetCrashedEvent)


-- Not CDP v1.3 --

data AutoAttachParams
  = AutoAttachParams
    { autoAtatch :: Bool
    , waitForDegubberOnStart :: Bool
    , flatten :: Maybe Bool
    , windowOpen :: Maybe Bool
    } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''AutoAttachParams)

data MessageParams
  = MessageParams
    { mesage :: String
    , sessionId :: Maybe SessionId
    , targetId :: Maybe TargetId
    } deriving Show

$(deriveJSON defaultOptions{ omitNothingFields = True } ''MessageParams)
