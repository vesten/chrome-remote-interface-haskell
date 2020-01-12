{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module CDP.API.Schema.Types where

import           CDP.Target.Message.TH

data Domain = Domain
              { name    :: String
              , version :: String
              } deriving Show

$(deriveJSONMsg ''Domain)

data DomainsResult = DomainsResult
                     { domains :: [Domain] }
                     deriving Show

$(deriveJSONMsg ''DomainsResult)
