module CDP.API.Input where

import           CDP.Target.Client
import           CDP.Target.Message

import           CDP.API.Input.Types

dispatchKeyEvent :: KeyEvent -> TargetClientAsync (MethodResult AnyResult)
dispatchKeyEvent = callMethod . Method "Input.dispatchKeyEvent"

dispatchMouseEvent :: MouseEvent -> TargetClientAsync (MethodResult AnyResult)
dispatchMouseEvent = callMethod . Method "Input.dispatchMouseEvent"
