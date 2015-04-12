module Main where

import Control.Monad (unless)
import Graphics.UI.Qtah

main :: IO ()
main = do
  app <- qApplication_new

  mainWindow <- qMainWindow_new qWidget_null
  qWidget_setWindowTitle mainWindow "Greeter demo"
  qWidget_resize mainWindow 400 40

  label <- qLabel_newWithText "What's your name?"

  lineEdit <- qLineEdit_new

  button <- qPushButton_newWithText "Greet" qWidget_null
  connectOk <- on button qAbstractButton_clicked_signal $ \_ -> do
    name <- qLineEdit_text lineEdit
    putStrLn $ if null name
               then "You don't have a name?"
               else "Hello, " ++ name ++ "!"
  unless connectOk $ putStrLn "!!! Failed to connect to button's click signal !!!"

  mainWidget <- qWidget_new
  qMainWindow_setCentralWidget mainWindow mainWidget
  hbox <- qHBoxLayout_new
  qLayout_addWidget hbox label
  qLayout_addWidget hbox lineEdit
  qLayout_addWidget hbox button
  qWidget_setLayout mainWidget hbox
  qWidget_show mainWindow
  qApplication_exec app
  delete mainWindow
  delete app
