module Network.Server
  (handleConnecting,
   handleMessage) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.Async

import Network.Message
import Network.Snapshot
import Network.Server.ConnectionStatus
import Network.Server.NetStatus

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TBQueue

import Data.Text(Text)
import Data.Text qualified as Text
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as Map
import Data.IntMap.Strict(IntMap)
import Data.IntMap.Strict qualified as IntMap

import Apecs

import Game.Components
import Game.Server
import Game.Server.World

type Login = Text

loginInfo :: Map.Map Text Text
loginInfo = Map.fromList [("test", "test")]

handleCall :: Server -> NetStatus -> Login -> Message -> IO (Maybe Message)
handleCall _ _ _ Ping = pure $ Just Pong
handleCall _ _ _ _ = pure Nothing

handleCast :: Server -> NetStatus -> Login -> Message -> IO ()
handleCast server netstatus name (Action action) = do
  players' <- readTVarIO netstatus.players
  case Map.lookup name players' of
    Just ent -> atomically $ writeTBQueue netstatus.actions (ent, action)
    Nothing -> pure ()
handleCast _ _ _ _ = pure ()

checkPass :: Text -> Text -> Bool
checkPass = (==)

tryLogin :: Server -> NetStatus -> Connection -> Login -> Text -> IO (Maybe Message)
tryLogin server netstatus conn name pass = do
  world <- readTVarIO server.world
  case Map.lookup name loginInfo of
    Just acctPass | checkPass acctPass pass -> do
      atomically $ modifyTVar' netstatus.logins \logins -> Map.insert conn.connId name logins
      ent <- tryMakeEntity world netstatus name
      pure $ Just (LoginSuccess (unEntity ent))
    _ -> pure Nothing
  where
    tryMakeEntity world netstatus name = do
      players <- readTVarIO netstatus.players
      case Map.lookup name players of
        Nothing -> do
          ent <- registerPlayer world name
          atomically $ modifyTVar' netstatus.players \players -> Map.insert name ent players
          pure $ ent
        Just ent ->
          pure $ ent

registerPlayer :: World -> Text -> IO Entity
registerPlayer world name = runWith world $ newEntity (Player name, Position 0 0)

handleConnecting :: Server -> NetStatus -> Connection -> ClientMessage Message -> IO (Connection, Maybe (ServerMessage Message))
handleConnecting server netstatus conn (Call id (TryLogin name pass)) = do
  let _str_name = Text.unpack name
  reply <- tryLogin server netstatus conn name pass
  case reply of
    Just x -> do
      putStrLn $ "Player login: " <> _str_name 
      pure (conn{connStatus = LoggedIn name}, (Reply id) <$> (Just x))
    Nothing -> do
      atomically $ writeTBQueue conn.writeQueue (Reply id LoginFail)
      error "disconnect"
      pure (conn, Nothing)
handleConnecting _ _ conn _ = pure (conn, Nothing)

handleMessage :: Server -> NetStatus -> Connection -> ClientMessage Message -> IO (Connection, Maybe (ServerMessage Message))
handleMessage server netstatus conn@Connection{connStatus=(LoggedIn name)} (Call id msg') = do
  reply <- handleCall server netstatus name msg'
  pure (conn, (Reply id) <$> reply)
handleMessage server netstatus conn@Connection{connStatus=(LoggedIn name)} (Cast msg') = do
  handleCast server netstatus name msg'
  pure (conn, Nothing)
handleMessage _ _ conn _ = pure (conn, Nothing)
