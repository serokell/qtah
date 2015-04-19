module Main where

import Control.Monad (unless)
import Graphics.UI.Qtah (delete, on, qApplication_new)
import qualified Graphics.UI.Qtah.Q.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Q.QApplication as QApplication
import qualified Graphics.UI.Qtah.Q.QHBoxLayout as QHBoxLayout
import qualified Graphics.UI.Qtah.Q.QLabel as QLabel
import qualified Graphics.UI.Qtah.Q.QLayout as QLayout
import qualified Graphics.UI.Qtah.Q.QLineEdit as QLineEdit
import qualified Graphics.UI.Qtah.Q.QMainWindow as QMainWindow
import qualified Graphics.UI.Qtah.Q.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Q.QWidget as QWidget

main :: IO ()
main = do
  app <- qApplication_new

  mainWindow <- QMainWindow.new QWidget.null
  QWidget.setWindowTitle mainWindow "Greeter demo"
  QWidget.resize mainWindow 400 40

  label <- QLabel.newWithText "What's your name?"

  lineEdit <- QLineEdit.new

  button <- QPushButton.newWithText "Greet" QWidget.null
  connectOk <- on button QAbstractButton.clickedSignal $ \_ -> do
    name <- QLineEdit.text lineEdit
    putStrLn $ if null name
               then "You don't have a name?"
               else "Hello, " ++ name ++ "!"
  unless connectOk $ putStrLn "!!! Failed to connect to button's click signal !!!"

  mainWidget <- QWidget.new
  QMainWindow.setCentralWidget mainWindow mainWidget
  hbox <- QHBoxLayout.new
  QLayout.addWidget hbox label
  QLayout.addWidget hbox lineEdit
  QLayout.addWidget hbox button
  QWidget.setLayout mainWidget hbox
  QWidget.show mainWindow
  QApplication.exec app
  delete mainWindow
  delete app
