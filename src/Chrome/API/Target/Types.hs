{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Chrome.API.Target.Types where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char (toLower)
import           Data.Text (Text)

newtype TargetId =
  TargetId String
  deriving (Show)

$(deriveJSON defaultOptions ''TargetId)

newtype SessionId = SessionId { sessionId :: String} deriving (Show)

$(deriveJSON defaultOptions ''SessionId)

newtype BrowserContextId =
  BrowserContextId String
  deriving (Show)

$(deriveJSON defaultOptions {omitNothingFields = True} ''BrowserContextId)

data TargetInfo = TargetInfo
  { infoTargetId         :: TargetId
  , infoTargetType       :: String
  , infoTitle            :: String
  , infoUrl              :: String
  , infoAttached         :: Bool
  , infoOpenerId         :: Maybe TargetId
  , infoBrowserContextId :: Maybe BrowserContextId
  } deriving (Show)

instance FromJSON TargetInfo where
  parseJSON =
    withObject "targetInfo" $ \o ->
      TargetInfo <$> o .: "targetId" <*> o .: "type" <*> o .: "title" <*>
      o .: "url" <*>
      o .: "attached" <*>
      o .:? "openerId" <*>
      o .:? "browserContextId"

data TargetInfos = TargetInfos
  { infoTargetInfos         :: [TargetInfo]
  } deriving (Show)

instance FromJSON TargetInfos where
  parseJSON =
    withObject "targetInfos" $ \o ->
      TargetInfos <$> o .: "targetInfos"

data AttachParams = AttachParams
  { attachTargetId :: TargetId
  , attachFlatten  :: Maybe Bool
  } deriving (Show)

instance ToJSON AttachParams where
  toJSON (AttachParams targ fltn) =
    object ["targetId" .= targ, "flatten" .= fltn]

data CreateParams = CreateParams
  { url                     :: String
  , width                   :: Maybe Int
  , height                  :: Maybe Int
  , browserContextId        :: Maybe BrowserContextId
  , enableBeginFrameControl :: Maybe Bool
  , newWindow               :: Maybe Bool
  , background              :: Maybe Bool
  } deriving (Show)

$(deriveJSON defaultOptions {omitNothingFields = True} ''CreateParams)

data DeleteParams = DeleteParams
  { deleteTargetId  :: Maybe TargetId
  , deleteSessionId :: Maybe SessionId
  } deriving (Show)

instance ToJSON DeleteParams where
  toJSON (DeleteParams targ fltn) =
    object ["targetId" .= targ, "sessionId" .= fltn]

data ReceivedMessageEvent = ReceivedMessageEvent
  { rxSessionId :: SessionId
  , rxMessage   :: String
  , rxTargetId  :: Maybe TargetId
  } deriving (Show)

instance FromJSON ReceivedMessageEvent where
  parseJSON =
    withObject "receivedMessageEvent" $ \o ->
      ReceivedMessageEvent <$> o .: "sessionId" <*> o .: "message" <*>
      o .:? "targetId"

data TargetCrashedEvent = TargetCrashedEvent
  { crashedTargetId  :: TargetId
  , crashedStatus    :: String
  , crashedErrorCode :: Int
  } deriving (Show)

instance FromJSON TargetCrashedEvent where
  parseJSON =
    withObject "targetCrashedEvent" $ \o ->
      TargetCrashedEvent <$> o .: "targetId" <*> o .: "status" <*>
      o .: "errorCode"

-- Not CDP v1.3 --
data AutoAttachParams = AutoAttachParams
  { autoAttach             :: Bool
  , waitForDebuggerOnStart :: Bool
  , flatten                :: Maybe Bool
  , windowOpen             :: Maybe Bool
  } deriving (Show)

$(deriveJSON defaultOptions {omitNothingFields = True} ''AutoAttachParams)

data MessageParams = MessageParams
  { msgMessage   :: Text
  , msgSessionId :: Maybe SessionId
  , msgTargetId  :: Maybe TargetId
  } deriving (Show)

instance ToJSON MessageParams where
  toJSON (MessageParams msg sess targ) =
    object ["message" .= msg, "sessionId" .= sess, "targetId" .= targ]

data AttachedInfo = AttachedInfo
  { attSessionId :: SessionId
  , attTargetInfo :: TargetInfo
  , attWaitingForDebugger :: Bool
  } deriving Show

instance FromJSON AttachedInfo where
  parseJSON =
    withObject "attachedInfo" $ \o ->
      AttachedInfo <$> o .: "sessionId" <*> o .: "targetInfo" <*> o .: "waitingForDebugger"

-- $(deriveJSON defaultOptions {fieldLabelModifier = dropCamel 3} ''AttachedInfo)

dropCamel :: Int -> String -> String
dropCamel n s = uncap $ drop n s
  where
    uncap [] = []
    uncap (c:cs) = toLower c : cs
