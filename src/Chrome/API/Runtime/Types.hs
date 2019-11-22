{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Chrome.API.Runtime.Types where

import           Data.Aeson
import           Data.Aeson.TH

import           Chrome.Target.Message.TH

type UnserializableValue = String

type RemoteObjectId = String

data RemoteObject = RemoteObject
  { remoteType                :: String
  , remoteSubtype             :: Maybe String
  , remoteClassName           :: Maybe String
  , remoteValue               :: Maybe Value
  , remoteUnserializableValue :: Maybe UnserializableValue
  , remoteDescription         :: Maybe String
  , remoteObjectId            :: Maybe RemoteObjectId
  } deriving (Show)

instance FromJSON RemoteObject where
  parseJSON =
    withObject "remoteObject" $ \o ->
      RemoteObject <$> o .: "type" <*> o .:? "subtype" <*> o .:? "className" <*>
      o .:? "value" <*>
      o .:? "unserializedValue" <*>
      o .:? "description" <*>
      o .:? "objectId"

instance ToJSON RemoteObject where
  toJSON (RemoteObject type_ subtype name val uval desc obj) =
    object
      [ "remoteType" .= type_
      , "remoteSubtype" .= subtype
      , "remoteClassName" .= name
      , "remoteValue" .= val
      , "remoteUnserializableValue" .= uval
      , "remoteDescription" .= desc
      , "remoteObjectId" .= obj
      ]

type ScriptId = String

data CallFrame = CallFrame
  { frameFunctionName :: String
  , frameScriptId     :: ScriptId
  , frameUrl          :: String
  , frameLineNumber   :: Int
  , frameColumnNumber :: Int
  } deriving (Show)

instance FromJSON CallFrame where
  parseJSON =
    withObject "callFrame" $ \o ->
      CallFrame <$> o .: "functionName" <*> o .: "scriptId" <*> o .: "url" <*>
      o .: "lineNumber" <*>
      o .: "columnNumber"

instance ToJSON CallFrame where
  toJSON (CallFrame name id_ url line col) =
    object
      [ "frameFunctionName" .= name
      , "frameScriptId" .= id_
      , "frameUrl" .= url
      , "frameLineNumber" .= line
      , "frameColumnNumber" .= col
      ]

data StackTrace = StackTrace
  { description :: Maybe String
  , callFrames  :: [CallFrame]
  , parent      :: Maybe StackTrace
  } deriving (Show)

$(deriveJSON defaultOptions {omitNothingFields = True} ''StackTrace)

type ExecutionContextId = Int

data ExceptionDetails = ExceptionDetails
  { exceptionId        :: Int
  , text               :: String
  , lineNumber         :: Int
  , columnNumber       :: Int
  , scriptId           :: Maybe ScriptId
  , url                :: Maybe String
  , stackTrace         :: Maybe StackTrace
  , exception          :: Maybe RemoteObject
  , executionContextId :: Maybe ExecutionContextId
  } deriving (Show)

$(deriveJSON defaultOptions {omitNothingFields = True} ''ExceptionDetails)

data CallArgument = CallArgument
  { argValue               :: Maybe Value
  , argUnserializableValue :: Maybe UnserializableValue
  , argObjectId            :: Maybe RemoteObjectId
  } deriving (Show)

instance FromJSON CallArgument where
  parseJSON =
    withObject "callArgument" $ \o ->
      CallArgument <$> o .:? "value" <*> o .:? "unserializedValue" <*>
      o .:? "objectId"

instance ToJSON CallArgument where
  toJSON (CallArgument val unVal obj) =
    object
      [ "argValue" .= val
      , "argUnserializableValue" .= unVal
      , "argObjectId" .= obj
      ]

data EvaluateParams = EvaluateParams
  { evalExpression            :: String
  , evalObjectGroup           :: Maybe String
  , evalIncludeCommandLineAPI :: Maybe Bool
  , evalSilent                :: Maybe Bool
  , evalContextId             :: Maybe ExecutionContextId
  , evalReturnByValue         :: Maybe Bool
  , evalGeneratePreview       :: Maybe Bool
  , evalUserGesture           :: Maybe Bool
  , evalAwaitPromise          :: Maybe Bool
  } deriving (Show)

instance ToJSON EvaluateParams where
  toJSON (EvaluateParams ex grp incl silent ctx rtv prev ug wait) =
    object
      [ "evalExpression" .= ex
      , "evalObjectGroup" .= grp
      , "evalIncludeCommandLineAPI" .= incl
      , "evalSilent" .= silent
      , "evalContextId" .= ctx
      , "evalReturnByValue" .= rtv
      , "evalGeneratePreview" .= prev
      , "evalUserGesture" .= ug
      , "evalAwaitPromise" .= wait
      ]

data EvaluateResult = EvaluateResult
  { evalResult           :: RemoteObject
  , evalExceptionDetails :: Maybe ExceptionDetails
  } deriving (Show)

instance FromJSON EvaluateResult where
  parseJSON =
    withObject "evaluateResult" $ \o ->
      EvaluateResult <$> o .: "result" <*> o .: "exceptionDetails"

data AwaitPromiseParams = AwaitPromiseParams
  { promisePromiseObjectId :: RemoteObjectId
  , promiseReturnByValue   :: Maybe Bool
  , promiseGeneratePreview :: Maybe Bool
  } deriving (Show)

instance ToJSON AwaitPromiseParams where
  toJSON (AwaitPromiseParams obj rtv prev) =
    object
      [ "promisePromiseObjectId" .= obj
      , "promiseReturnByValue" .= rtv
      , "promiseGeneratePreview" .= prev
      ]

data CallFunctionOnParams = CallFunctionOnParams
  { fcnObjectId            :: RemoteObjectId
  , fcnFunctionDeclaration :: String
  , fcnArguments           :: Maybe [CallArgument]
  , fcnSilent              :: Maybe Bool
  , fcnReturnByValue       :: Maybe Bool
  , fcnGeneratePreview     :: Maybe Bool
  , fcnUserGesture         :: Maybe Bool
  , fcnAwaitPromise        :: Maybe Bool
  } deriving (Show)

instance ToJSON CallFunctionOnParams where
  toJSON (CallFunctionOnParams obj decl arg silent rbv pre ug wait) =
    object
      [ "fcnObjectId" .= obj
      , "fcnFunctionDeclaration" .= decl
      , "fcnArguments" .= arg
      , "fcnSilent" .= silent
      , "fcnReturnByValue" .= rbv
      , "fcnGeneratePreview" .= pre
      , "fcnUserGesture" .= ug
      , "fcnAwaitPromise" .= wait
      ]

data GetPropertiesParams = GetPropertiesParams
  { objectId               :: RemoteObjectId
  , ownProperties          :: Maybe Bool
  , accessorPropertiesOnly :: Maybe Bool
  , generatePreview        :: Maybe Bool
  } deriving (Show)

$(deriveJSONMsg ''GetPropertiesParams)

data PropertyDescriptor = PropertyDescriptor
  { propName         :: String
  , propValue        :: Maybe RemoteObject
  , propWriteable    :: Maybe Bool
  , propGet          :: Maybe RemoteObject
  , propSet          :: Maybe RemoteObject
  , propConfigurable :: Bool
  , propEnumerable   :: Bool
  , propWasThrown    :: Maybe Bool
  , propIsOwn        :: Maybe Bool
  , propSymbol       :: Maybe RemoteObject
  } deriving (Show)

instance FromJSON PropertyDescriptor where
  parseJSON =
    withObject "propertyDescriptor" $ \o ->
      PropertyDescriptor <$> o .: "name" <*> o .:? "value" <*> o .:? "writeable" <*>
      o .:? "get" <*>
      o .:? "set" <*>
      o .: "configurable" <*>
      o .: "enumerable" <*>
      o .:? "wasThrown" <*>
      o .:? "isOwn" <*>
      o .:? "symbol"

instance ToJSON PropertyDescriptor where
  toJSON (PropertyDescriptor pName val wrt get set cnf enum thw is sym) =
    object
      [ "propName" .= pName
      , "propValue" .= val
      , "propWriteable" .= wrt
      , "propGet" .= get
      , "propSet" .= set
      , "propConfigurable" .= cnf
      , "propEnumerable" .= enum
      , "propWasThrown" .= thw
      , "propIsOwn" .= is
      , "propSymbol" .= sym
      ]

data InternalPropertyDescriptor = InternalPropertyDescriptor
  { name  :: String
  , value :: Maybe RemoteObject
  } deriving (Show)

$(deriveJSONMsg ''InternalPropertyDescriptor)

data GetPropertiesResult = GetPropertiesResult
  { result             :: [PropertyDescriptor]
  , internalProperties :: [InternalPropertyDescriptor]
  , exceptionDetail    :: Maybe ExceptionDetails
  } deriving (Show)

$(deriveJSONMsg ''GetPropertiesResult)

data ReleaseObjectParams = ReleaseObjectParams
  { relObjectId :: RemoteObjectId
  } deriving (Show)

instance ToJSON ReleaseObjectParams where
  toJSON (ReleaseObjectParams id_) = object ["relObjectId" .= id_]

data ReleaseObjectGroupParams = ReleaseObjectGroupParams
  { objectGroup :: String
  } deriving (Show)

$(deriveJSONMsg ''ReleaseObjectGroupParams)

data CompileScriptParams = CompileScriptParams
  { scriptExpression         :: String
  , scriptSourceURL          :: String
  , scriptPersistScript      :: Bool
  , scriptExecutionContextId :: Maybe ExecutionContextId
  } deriving (Show)

instance ToJSON CompileScriptParams where
  toJSON (CompileScriptParams ex url_ persist ctx) =
    object
      [ "scriptExpression" .= ex
      , "scriptSourceURL" .= url_
      , "scriptPersistScript" .= persist
      , "scriptExecutionContextId" .= ctx
      ]

data CompileScriptResult = CompileScriptResult
  { resScriptId         :: Maybe ScriptId
  , resExceptionDetails :: Maybe ExceptionDetails
  } deriving (Show)

instance FromJSON CompileScriptResult where
  parseJSON =
    withObject "compileScriptResult" $ \o ->
      CompileScriptResult <$> o .:? "scriptId" <*> o .:? "exceptionDetails"

data RunScriptParams = RunScriptParams
  { runScriptId              :: ScriptId
  , runExecutionContextId    :: Maybe ExecutionContextId
  , runObjectGroup           :: Maybe String
  , runSilent                :: Maybe Bool
  , runIncludeCommandLineAPI :: Maybe Bool
  , runReturnByValue         :: Maybe Bool
  , runGeneratePreview       :: Maybe Bool
  , runAwaitPromise          :: Maybe Bool
  } deriving (Show)

instance FromJSON RunScriptParams where
  parseJSON =
    withObject "runScriptParams" $ \o ->
      RunScriptParams <$> o .: "scriptId" <*> o .:? "executionContextId" <*>
      o .:? "objectGroup" <*>
      o .:? "silent" <*>
      o .:? "includeCommandLineAPI" <*>
      o .:? "returnByValue" <*>
      o .:? "generatePreview" <*>
      o .:? "awaitPromise"

instance ToJSON RunScriptParams where
  toJSON (RunScriptParams id_ ctx grp silent api rtn prev wait) =
    object
      [ "runScriptId" .= id_
      , "runExecutionContextId" .= ctx
      , "runObjectGroup" .= grp
      , "runSilent" .= silent
      , "runIncludeCommandLineAPI" .= api
      , "runReturnByValue" .= rtn
      , "runGeneratePreview" .= prev
      , "runAwaitPromise" .= wait
      ]

data ExecutionContextDescription = ExecutionContextDescription
  { ctxId      :: ExecutionContextId
  , ctxOrigin  :: String
  , ctxName    :: String
  , ctxAuxData :: Maybe Value
  } deriving (Show)

instance FromJSON ExecutionContextDescription where
  parseJSON =
    withObject "executionContextDescription" $ \o ->
      ExecutionContextDescription <$> o .: "id" <*> o .: "origin" <*>
      o .: "name" <*>
      o .:? "auxData"

instance ToJSON ExecutionContextDescription where
  toJSON (ExecutionContextDescription id_ origin eName aux) =
    object
      [ "ctxId" .= id_
      , "ctxOrigin" .= origin
      , "ctxName" .= eName
      , "ctxAuxData" .= aux
      ]

data ContextCreatedEvent = ContextCreatedEvent
  { context :: ExecutionContextDescription
  } deriving (Show)

$(deriveJSONMsg ''ContextCreatedEvent)

data ContextDestroyedEvent = ContextDestroyedEvent
  { destroyedExecutionContextId :: ExecutionContextId
  } deriving (Show)

$(deriveJSONMsg ''ContextDestroyedEvent)

data ExceptionThrownEvent = ExceptionThrownEvent
  { timestamp        :: Double
  , exceptionDetails :: ExceptionDetails
  } deriving (Show)

$(deriveJSONMsg ''ExceptionThrownEvent)

data ExceptionRevokedEvent = ExceptionRevokedEvent
  { revokedReason      :: String
  , revokedExceptionId :: Int
  } deriving (Show)

instance FromJSON ExceptionRevokedEvent where
  parseJSON =
    withObject "exceptionRevokedEvent" $ \o ->
      ExceptionRevokedEvent <$> o .: "reason" <*> o .: "exceptionId"

data ConsoleAPICalledEvent = ConsoleAPICalledEvent
  { calledType               :: String
  , calledArgs               :: [RemoteObject]
  , calledExecutionContextId :: ExecutionContextId
  , calledTimestamp          :: Double
  , calledStackTrace         :: Maybe StackTrace
  } deriving (Show)

instance FromJSON ConsoleAPICalledEvent where
  parseJSON =
    withObject "exceptionRevokedEvent" $ \o ->
      ConsoleAPICalledEvent <$> o .: "type" <*> o .: "args" <*>
      o .: "executionContextId" <*>
      o .: "timestamp" <*>
      o .:? "stackTrace"

data InspectRequestedEvent = InspectRequestedEvent
  { object :: RemoteObject
  , hints  :: Value
  } deriving (Show)

instance FromJSON InspectRequestedEvent where
  parseJSON =
    withObject "inspectRequestedEvent" $ \o ->
      InspectRequestedEvent <$> o .: "object" <*> o .: "hints"
