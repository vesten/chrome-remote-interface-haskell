module CDP.API.Schema where

import           CDP.Target.Client
import           CDP.Target.Message

import           CDP.API.Schema.Types

getDomains :: TargetClientAsync (MethodResult DomainsResult)
getDomains = callMethod $ Method "Schema.getDomains" noParam

