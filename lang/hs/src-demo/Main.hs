module Main where

import Foreign.Cppop.Generated.Qtah

main :: IO ()
main = do
  putStrLn "Testing callbacks..."
  testIntCallback $ \x -> putStrLn $ "Received int " ++ show x ++ "."
  testIntCallback $ \x -> putStrLn $ "Received int " ++ show x ++ "."
  testStringCallback $ \x -> putStrLn $ "Received string " ++ show x ++ "."

  putStrLn ""
  putStrLn "Creating a QApplication."
  app <- qApplication_new
  putStrLn "Creating a QWidget."
  wnd <- qMainWindow_new qWidget_null
  putStrLn "Setting window properties."
  qWidget_setWindowTitle wnd "Hi!"
  qWidget_resize wnd 640 480
  putStrLn "Creating a button."
  btn <- qPushButton_newWithText "Hello!" qWidget_null
  qMainWindow_setCentralWidget wnd btn
  putStrLn "Showing the window."
  qWidget_show wnd
  putStrLn "Running the application."
  qApplication_exec app
  putStrLn "Done!"
