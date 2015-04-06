module Main where

import Data.Binary.Get (Get, runGet)
import Data.Binary.Put (runPut)
import qualified Data.ByteString.Lazy as BL
import Foreign.Cppop.Runtime.Binary
import Foreign.Cppop.Runtime.Client
import Foreign.Cppop.Generated.Qtpi hiding (String)

main :: IO ()
main = do
  putStrLn "Creating a client."
  c <- newClient $ ClientParams
       { paramInPath = "/tmp/serverout"
       , paramOutPath = "/tmp/serverin"
       }

  putStrLn "Creating a callback."
  callback <- newCallback c $ \argBytes -> do
    let str = runGet (hget :: Get String) argBytes
    return $ runPut $ hput $ str ++ "(in Haskell callback)"
  putStrLn "Binding the callback."
  cbtest_set c $ callbackId callback
  putStrLn "Invoking the callback."
  putStrLn . (++ "(end)") =<< cbtest_call c "(start)"

  putStrLn ""

  putStrLn "Creating a QApplication."
  app <- qApplication_new c
  putStrLn "Creating a QWidget."
  wnd <- qMainWindow_new c qWidget_null
  putStrLn "Setting window properties."
  qWidget_setWindowTitle c wnd "Hi!"
  qWidget_resize c wnd 640 480
  putStrLn "Creating a button."
  btn <- qPushButton_newWithText c "Hello!" qWidget_null
  qMainWindow_setCentralWidget c wnd btn
  putStrLn "Showing the window."
  qWidget_show c wnd
  putStrLn "Running the application."
  qApplication_exec c app
  putStrLn "Done!"
