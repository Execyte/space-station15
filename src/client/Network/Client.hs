module Network.Client(processEvent) where

import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TBQueue

import Network.Message
import Network.Client.ConnectionStatus

import Game
import Game.Client

processEvent :: Client -> Message -> IO ()
processEvent _ n = do
  putStrLn $ "Seen " <> show n
  pure ()
processEvent _ _ = pure ()

