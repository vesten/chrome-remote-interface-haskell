{-# LANGUAGE RecordWildCards #-}

{-|
Module: Chrome.Launcher
Description: Helpers allowing to launch Chrome programmatically
|-}

module Chrome.Launcher
  ( ChromeConfig(..)
  , defaultConfig
  , locateChrome
  , withChrome
  ) where

import           Control.Monad        (filterM)
import           Data.Default         (Default (..))
import           Safe                 (headMay)
import           System.Directory     (doesFileExist)
import           System.Info          (os)
import           System.Process.Typed (proc, stopProcess, withProcessWait)

-- | Spawn Google Chrome using the provided config, execute an action and close the browser once the action is done
--
-- Example:
--
-- >>> withChrome defaultConfig (\_ -> putStrLn "Chrome started !")
withChrome :: ChromeConfig -> (ChromeConfig -> IO a) -> IO a
withChrome cfg action = withProcessWait (mkChromeProcess cfg) run
  where
    mkChromeProcess ChromeConfig{..} = proc chromeExecutablePath [ "--remote-debugging-port=" <> show chromeRemoteDebuggingPort ]

    run p = action cfg >>= (<$ stopProcess p)

data ChromeConfig
  = ChromeConfig { chromeExecutablePath      :: FilePath
                  -- ^ Path to the Google Chrome executable
                 , chromeRemoteDebuggingPort :: Int
                  -- ^ Port for using the remote debugging protocol
                 }

instance Default ChromeConfig where
  def = defaultConfig

-- | Default configuration for launching "google-chrome" with the remote debugging port set to 9222
defaultConfig :: ChromeConfig
defaultConfig = ChromeConfig { chromeExecutablePath = "google-chrome"
                             , chromeRemoteDebuggingPort = 9222
                             }

-- | locateChrome returns a path to the Chrome binary, or an empty string if
--  Chrome installation is not found.
--  Ported form https://github.com/zserge/lorca/locate.go
locateChrome :: IO (Maybe FilePath)
locateChrome = headMay <$> filterM doesFileExist paths
  where
    paths =
      case os of
        "windows" ->
          -- let
          --   pfx = [ unsafePerformIO $ getEnv "LocalAppData"
          --         , unsafePerformIO $ getEnv "ProgramFiles"
          --         , unsafePerformIO $ getEnv "ProgramFiles (x86)"
          --         ]
          --   sfx = [ "/Google/Chrome/Appliaction/chrome.exe"
          --         , "/Google/Chrome/chrome.exe"
          --         , "/Chromium/Application/chrome.exe"
          --         , "/Chromium/chrome.exe"
          --         ]
          --  in [a++b | a <- pfx, b <- sfx]
          [ -- "C:/Users/" + os.Getenv("USERNAME") + "/AppData/Local/Google/Chrome/Application/chrome.exe"
            "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe"
          , "C:/Program Files/Google/Application/chrome.exe"
          , "C:/Program Files/Google/Chrome/Application/chrome.exe"
          -- , "C:/Users/" + os.Getenv("USERNAME") + "/AppData/Local/Chromium/Application/chrome.exe"
          -- , "C:/Users/" + os.Getenv("USERNAME") + "/AppData/Local/Google/Chrome/Application/chrome.exe"
          -- , "C:/Users/" + os.Getenv("USERNAME") + "/AppData/Local/Google/Chrome/chrome.exe"
          ]
        "darwin" ->
          [ "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
          , "/Applications/Google Chrome Canary.app/Contents/MacOS/Google Chrome Canary"
          , "/Applications/Chromium.app/Contents/MacOS/Chromium"
          , "/usr/bin/google-chrome-stable"
          , "/usr/bin/google-chrome"
          , "/usr/bin/chromium"
          , "/usr/bin/chromium-browser"
          ]
        _ ->
          [ "/usr/bin/google-chrome-stable"
          , "/usr/bin/google-chrome"
          , "/usr/bin/chromium"
          , "/usr/bin/chromium-browser"
          , "/snap/bin/chromium"
          ]
