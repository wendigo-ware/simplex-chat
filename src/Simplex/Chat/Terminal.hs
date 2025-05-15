{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Terminal where

import Control.Monad
import qualified Data.List.NonEmpty as L
import Simplex.Chat (defaultChatConfig, operatorCommunity)
import Simplex.Chat.Controller
import Simplex.Chat.Core
import Simplex.Chat.Help (chatWelcome)
import Simplex.Chat.Library.Commands (_defaultNtfServers)
import Simplex.Chat.Operators
import Simplex.Chat.Options
import Simplex.Chat.Terminal.Input
import Simplex.Chat.Terminal.Output
import Simplex.FileTransfer.Client.Presets (defaultXFTPServers)
import Simplex.Messaging.Client (NetworkConfig (..), SMPProxyFallback (..), SMPProxyMode (..), defaultNetworkConfig)
import Simplex.Messaging.Util (raceAny_)
#if !defined(dbPostgres)
import Control.Exception (handle, throwIO)
import qualified Data.ByteArray as BA
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Database.SQLite.Simple (SQLError (..))
import qualified Database.SQLite.Simple as DB
import Simplex.Chat.Options.DB
import System.IO (hFlush, hSetEcho, stdin, stdout)
#endif

terminalChatConfig :: ChatConfig
terminalChatConfig =
  defaultChatConfig
    { presetServers =
        PresetServers
          { operators =
              [ PresetOperator
                  { operator = Just operatorCommunity,
                    smp =
                      map
                        (presetServer True)
                        [
			  "smp://uDH4cu81seeKT_rnrLMprQ6jwsnHY12awH8JP0gfIfc=@154.26.139.40",
			  "smp://HZOF26feHaaMXqYuLTzhBFB652DEYENZEN8zpLxIeKs=@simplex.notrustverify.ch",
			  "smp://t10qBxn155jIZ2zLjQjftyVbh6YkPaR7-hx31y9or5E=@agorist.space",
			  "smp://L5jrGV2L_Bb20Oj0aE4Gn-m5AHet9XdpYDotiqpcpGc=@nowhere.moe",
                        ],
                    useSMP = 3,
                    xftp = map (presetServer True) $ L.toList defaultXFTPServers,
                    useXFTP = 3
                  }
              ],
            ntf = _defaultNtfServers,
            netCfg =
              defaultNetworkConfig
                {
		  -- Defaults changed: can't find where these are defined, so I
		  -- guessed! This might break something, especially second one.
		  smpProxyMode = SPMAlways,
                  smpProxyFallback = SPFNo
                }
          },
      deviceNameForRemote = "SimpleX CLI"
    }

simplexChatTerminal :: WithTerminal t => ChatConfig -> ChatOpts -> t -> IO ()
simplexChatTerminal cfg options t = run options
  where
#if defined(dbPostgres)
    run opts =
      simplexChatCore cfg opts $ \u cc -> do
        ct <- newChatTerminal t opts
        when (firstTime cc) . printToTerminal ct $ chatWelcome u
        runChatTerminal ct cc opts
#else
    run opts@ChatOpts {coreOptions = coreOptions@CoreChatOpts {dbOptions}} =
      handle checkDBKeyError . simplexChatCore cfg opts $ \u cc -> do
        ct <- newChatTerminal t opts
        when (firstTime cc) . printToTerminal ct $ chatWelcome u
        runChatTerminal ct cc opts
      where
        checkDBKeyError :: SQLError -> IO ()
        checkDBKeyError e = case sqlError e of
          DB.ErrorNotADatabase -> do
            putStrLn $ "Database file is invalid or " <> if BA.null (dbKey dbOptions) then "encrypted." else "you passed an incorrect encryption key."
            run =<< getKeyOpts
          _ -> throwIO e
        getKeyOpts :: IO ChatOpts
        getKeyOpts = do
          putStr "Enter database encryption key (Ctrl-C to exit):"
          hFlush stdout
          hSetEcho stdin False
          key <- getLine
          hSetEcho stdin True
          putStrLn ""
          pure opts {coreOptions = coreOptions {dbOptions = dbOptions {dbKey = BA.convert $ encodeUtf8 $ T.pack key}}}
#endif

runChatTerminal :: ChatTerminal -> ChatController -> ChatOpts -> IO ()
runChatTerminal ct cc opts = raceAny_ [runTerminalInput ct cc, runTerminalOutput ct cc opts, runInputLoop ct cc]
