module Client.UI.ConnectMenu(drawConnectMenu, newConnectMenu, ConnectMenu(..)) where

import qualified Client.ImGui as Im
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad(void)
import Network.UDP(UDPSocket)
import Data.Text(Text)

data ConnectMenu = ConnectMenu
  { server_ip :: TVar Text
  , username :: TVar Text
  , password :: TVar Text
  }

newConnectMenu :: STM ConnectMenu
newConnectMenu = ConnectMenu <$> newTVar "localhost" <*> newTVar "" <*> newTVar ""

drawConnectMenu :: ConnectMenu -> TVar (Maybe UDPSocket) -> IO ()
drawConnectMenu ConnectMenu{username, password, server_ip} socket = Im.withWindowOpen "connect to server" $ do
  Im.text "Server IP:"
  Im.sameLine

  Im.setNextItemWidth 150
  void $ Im.inputText "##server_ip" server_ip 32

  Im.text "Username:"
  Im.sameLine

  Im.setNextItemWidth 150
  void $ Im.inputText "##toconnect_username" username 128

  Im.text "Password:"
  Im.sameLine

  Im.setNextItemWidth 150
  void $ Im.inputText "##toconnect_password" password 128

  Im.button "connect"
  return ()
