module Chrome.API.Target (
      module Chrome.API.Target.Types
    , activateTarget
    , attachToTarget
    , closeTarget
    , createTarget
    , deleteFromTarget
    , getTargets
    , setDiscoverTarget
    , onReceivedMessageFromTarget
    , onTargetCreated
    , onTargetDestroyed
    , onTargetCrashed
    , onTargetInfoChanged
    ) where

import           Data.Map              (empty, insert)

import           Chrome.Target.Client
import           Chrome.Target.Message

import           Chrome.API.Target.Types

activateTarget :: TargetId -> TargetClientAsync (MethodResult AnyResult)
activateTarget = callMethod . Method "Target.activateTarget"

attachToTarget :: AttachParams -> TargetClientAsync (MethodResult SessionId)
attachToTarget = callMethod . Method "Target.attachToTarget"

closeTarget :: TargetId -> TargetClientAsync (MethodResult Bool)
closeTarget = callMethod . Method "Target.closeTarget"

createTarget :: CreateParams -> TargetClientAsync (MethodResult TargetId)
createTarget = callMethod . Method "Target.createTarget"

deleteFromTarget :: DeleteParams -> TargetClientAsync (MethodResult AnyResult)
deleteFromTarget = callMethod . Method "Target.deleteFromTarget"

getTargets :: TargetClientAsync (MethodResult [TargetInfo])
getTargets = callMethod $ Method "Target.getTargets" noParam

setDiscoverTarget :: Bool -> TargetClientAsync (MethodResult AnyResult)
setDiscoverTarget discover' = callMethod $ Method "Target.setDiscoverTarget"
                                                  (insert "discover" discover' empty)

onReceivedMessageFromTarget :: TargetClientAsync (MethodResult ReceivedMessageEvent)
onReceivedMessageFromTarget = listenToEventMethod "Target.receivedMessageFromTarget"

onTargetCreated :: TargetClientAsync (MethodResult TargetInfo)
onTargetCreated = listenToEventMethod "Target.targetCreated"

onTargetDestroyed :: TargetClientAsync (MethodResult TargetId)
onTargetDestroyed = listenToEventMethod "Target.targetDestroyed"

onTargetCrashed :: TargetClientAsync (MethodResult TargetCrashedEvent)
onTargetCrashed = listenToEventMethod "Target.targetCrashed"

onTargetInfoChanged :: TargetClientAsync (MethodResult TargetInfo)
onTargetInfoChanged = listenToEventMethod "Target.targetInfoChanged"
