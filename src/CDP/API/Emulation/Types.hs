{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module CDP.API.Emulation.Types where

import           CDP.Target.Message.TH (deriveJSONMsg)

data ScreenOrientation = ScreenOrientation
                         { _type :: String
                         , angle :: String
                         } deriving Show

$(deriveJSONMsg ''ScreenOrientation)

data SetMetricsOverrideParams = SetMetricsOverrideParams
                                { width             :: Int
                                , height            :: Int
                                , deviceScaleFactor :: Double
                                , mobile            :: Bool
                                , fitWindow         :: Bool
                                , scale             :: Maybe Double
                                , screenWidth       :: Maybe Int
                                , screenHeight      :: Maybe Int
                                , positionX         :: Maybe Int
                                , positionY         :: Maybe Int
                                , screenOrientation :: Maybe ScreenOrientation
                                } deriving Show

$(deriveJSONMsg ''SetMetricsOverrideParams)

data TouchEmulationEnabledParams = TouchEmulationEnabledParams
                             { enabled       :: Bool
                             , configuration :: Maybe String
                             } deriving Show

$(deriveJSONMsg ''TouchEmulationEnabledParams)

data EmulatedMediaParams = EmulatedMediaParams
                           { media :: String }
                           deriving Show

$(deriveJSONMsg ''EmulatedMediaParams)
