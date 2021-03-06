{-# LANGUAGE OverloadedStrings #-}

module CDP.Target.Client where

import           Network.WebSockets           as WS

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8   as B8
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T (putStrLn)

import           Control.Concurrent.Async
import           Control.Concurrent.STM.TChan
import           Control.Monad.STM            (atomically)

import           CDP.Target
import           CDP.Target.Message

socketClient :: (TChan T.Text, TChan T.Text) -> WS.ClientApp ()
socketClient (inChan, outChan) conn = do
  T.putStrLn "In socketClient..."
  readProc <-
    async $
    forever $ do
      T.putStrLn "Collecting chrome msg..."
      msgReceived <- WS.receiveData conn
      T.putStrLn msgReceived >> putStrLn "\n\n"
      atomically $ writeTChan outChan msgReceived
  writeProc <-
    async $
    forever $ do
      T.putStrLn "Getting msg to send..."
      msg <- atomically $ readTChan inChan
      T.putStrLn msg >> putStrLn "\n\n"
      let msg' = "{\"params\":{}, \"method\":\"Page.enable\",\"id\":17}"
      T.putStrLn msg'
      WS.sendTextData conn msg'
  mapM_ wait [readProc, writeProc]

type TargetClientChannels = (TChan T.Text, TChan T.Text)

createWSChannels :: IO TargetClientChannels
createWSChannels = (,) <$> newBroadcastTChanIO <*> newBroadcastTChanIO

type TargetClient = ReaderT TargetClientChannels IO

dupWSChannels :: TargetClient TargetClientChannels
dupWSChannels = do
  (chanCmd, chanRes) <- ask
  liftIO . atomically $ (,) <$> dupTChan chanCmd <*> dupTChan chanRes

callMethod ::
     (ToJSON req, Show res, FromJSON res)
  => Method req
  -> TargetClientAsync (MethodResult res)
callMethod cmd = do
  (chanCmd, chanRes) <- dupWSChannels
  msg <- liftIO $ methodToMsg cmd
  liftIO $ do
    T.putStrLn $ "Sending: " <> msgToText msg
    atomically $ writeTChan chanCmd (msgToText msg)
    -- putStrLn "Msg sent..."
    async (waitResponse chanRes (msgId msg))
  where
    waitResponse ::
         (FromJSON res) => TChan T.Text -> Int -> IO (MethodResult res)
    waitResponse chanRes' id' = do
      -- putStrLn "Awaiting response..."
      res <- atomically $ readTChan chanRes'

      let decodedMsg = decode . B8.pack . T.unpack $ res
      case decodedMsg of
        Just (Result result) ->
          if _resId result == id'
            then return $ _resResult result
            else waitResponse chanRes' id'
        _ -> waitResponse chanRes' id'

type TargetClientAsync res = TargetClient (Async res)

listenToEventMethod ::
     (FromJSON res) => String -> TargetClientAsync (MethodResult res)
listenToEventMethod method = do
  (_, chanRes) <- dupWSChannels
  liftIO $ async $ waitForMsg method chanRes
  where
    waitForMsg ::
         (FromJSON res) => String -> TChan T.Text -> IO (MethodResult res)
    waitForMsg method' inChan = do
      res <- atomically $ readTChan inChan
      let event = decode . B8.pack . T.unpack $ res
      case event of
        Just (Event event') ->
          if _eventMethod event' == method'
            then pure $ _eventContent event'
            else waitForMsg method inChan
        _ -> waitForMsg method inChan

wsServer :: Target -> TargetClient (Maybe ())
wsServer page =
  case wsClientFromTarget page of
    Nothing -> pure Nothing
    Just (domain', port', path') -> do
      liftIO $ putStrLn "In wsServer..."
      (chanCmd, chanRes) <- dupWSChannels
      liftIO $ putStrLn "Dup'd channels..."
      void . liftIO . async $
        WS.runClient
          domain'
          (fromInteger port')
          path'
          (socketClient (chanCmd, chanRes))
    -- TODO : remove this or use Either to encode error
      -- liftIO $ putStrLn "websocket ok..."
      return $ Just ()

withTarget :: Target -> TargetClient a -> IO ()
withTarget page actions = do
  putStrLn $ "page: " ++ show page
  channels <- createWSChannels
  -- putStrLn $ "channels: " ++ show channels
  runReaderT actions' channels
  where
    actions' :: TargetClient ()
    actions' = do
      void $ wsServer page
      void actions
