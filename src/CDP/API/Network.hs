module CDP.API.Network
  ( module CDP.API.Network.Types
  , enable
  , disable
  , setUserAgentOverride
  , setExtraHTTPHeaders
  , getResponseBody
  , canClearBrowserCache
  , clearBrowserCache
  , canClearBrowserCookies
  , clearBrowserCookies
  , emulateNetworkConditions
  , setCacheDisabled
  , onRequestWillBeSent
  , onRequestServedFromCache
  , onResponseReceived
  , onDataReceived
  , onLoadingFinished
  , onLoadingFailed ) where

import           Data.Map                 (empty, insert)

import           CDP.Target.Client
import           CDP.Target.Message

import           CDP.API.Network.Types

enable :: NetworkEnableParams -> TargetClientAsync (MethodResult AnyResult)
enable = callMethod . Method "Network.enable"

disable :: TargetClientAsync (MethodResult AnyResult)
disable = callMethod $ Method "Network.disable" noParam

setUserAgentOverride :: String -> TargetClientAsync (MethodResult AnyResult)
setUserAgentOverride userAgent = callMethod $ Method "Network.setUserAgentOverride" (insert "userAgent" userAgent empty)

setExtraHTTPHeaders :: Headers -> TargetClientAsync (MethodResult AnyResult)
setExtraHTTPHeaders = callMethod . Method "Network.setExtraHTTPHeaders"

getResponseBody :: String -> TargetClientAsync (MethodResult ResponseBody)
getResponseBody requestId' = callMethod $ Method "Network.getResponseBody" (insert "requestId" requestId' empty)

canClearBrowserCache :: TargetClientAsync (MethodResult CanClear)
canClearBrowserCache = callMethod $ Method "Network.canClearBrowserCache" noParam

clearBrowserCache :: TargetClientAsync (MethodResult AnyResult)
clearBrowserCache = callMethod $ Method "Network.clearBrowserCache" noParam

canClearBrowserCookies :: TargetClientAsync (MethodResult CanClear)
canClearBrowserCookies = callMethod $ Method "Network.canClearBrowserCookies" noParam

clearBrowserCookies :: TargetClientAsync (MethodResult AnyResult)
clearBrowserCookies = callMethod $ Method "Network.clearBrowserCookies" noParam

emulateNetworkConditions :: NetworkConditionsParams -> TargetClientAsync (MethodResult AnyResult)
emulateNetworkConditions = callMethod . Method "Network.emulateNetworkConditions"

setCacheDisabled :: Bool -> TargetClientAsync (MethodResult AnyResult)
setCacheDisabled disabled = callMethod $ Method "Network.setCacheDisabled" (insert "cacheDisabled"disabled empty)

onRequestWillBeSent :: TargetClientAsync (MethodResult RequestEvent)
onRequestWillBeSent = listenToEventMethod "Network.requestWillBeSent"

onRequestServedFromCache :: TargetClientAsync (MethodResult RequestFromCacheEvent)
onRequestServedFromCache = listenToEventMethod "Network.requestServedFromCache"

onResponseReceived :: TargetClientAsync (MethodResult ResponseReceivedEvent)
onResponseReceived = listenToEventMethod "Network.responseReceived"

onDataReceived :: TargetClientAsync (MethodResult DataReceivedEvent)
onDataReceived = listenToEventMethod "Network.dataReceived"

onLoadingFinished :: TargetClientAsync (MethodResult LoadingFinishedEvent)
onLoadingFinished = listenToEventMethod "Network.loadingFinished"

onLoadingFailed :: TargetClientAsync (MethodResult LoadingFailedEvent)
onLoadingFailed = listenToEventMethod "Network.loadingFailed"
