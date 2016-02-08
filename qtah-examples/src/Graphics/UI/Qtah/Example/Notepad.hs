-- This file is part of Qtah.
--
-- Copyright 2015-2016 Bryan Gardiner <bog@khumba.net>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE ScopedTypeVariables #-}

-- | The Qt notepad example.
module Graphics.UI.Qtah.Example.Notepad (run) where

import Control.Monad (unless)
import Foreign.Hoppy.Runtime (withScopedPtr)
import Graphics.UI.Qtah.Event
import Graphics.UI.Qtah.Gui.QCloseEvent (QCloseEvent)
import Graphics.UI.Qtah.Signal (connect)
import Graphics.UI.Qtah.Widgets.QAbstractButton (clickedSignal)
import Graphics.UI.Qtah.Widgets.QAction (triggeredSignal)
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import qualified Graphics.UI.Qtah.Widgets.QFileDialog as QFileDialog
import qualified Graphics.UI.Qtah.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Widgets.QMainWindow as QMainWindow
import Graphics.UI.Qtah.Widgets.QMainWindow (QMainWindow)
import qualified Graphics.UI.Qtah.Widgets.QMenu as QMenu
import qualified Graphics.UI.Qtah.Widgets.QMenuBar as QMenuBar
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Widgets.QTextEdit as QTextEdit
import Graphics.UI.Qtah.Widgets.QTextEdit (QTextEdit)
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import System.Environment (getArgs)

data Notepad = Notepad
  { myWindow :: QMainWindow
  , myText :: QTextEdit
  }

run :: IO ()
run = withScopedPtr (getArgs >>= QApplication.new) $ \app -> do
  mainWindow <- makeMainWindow
  QWidget.show mainWindow
  QApplication.exec app

makeMainWindow :: IO QMainWindow
makeMainWindow = do
  window <- QMainWindow.new

  menu <- QMenuBar.new
  menuFile <- QMenuBar.addNewMenu menu "File"
  menuFileNew <- QMenu.addNewAction menuFile "New"
  menuFileOpen <- QMenu.addNewAction menuFile "Open"
  menuFileSave <- QMenu.addNewAction menuFile "Save"
  QMainWindow.setMenuBar window menu

  contents <- QWidget.new
  layout <- QVBoxLayout.new
  QWidget.setLayout contents layout

  text <- QTextEdit.new
  quitButton <- QPushButton.newWithText "Quit"
  QLayout.addWidget layout text
  QLayout.addWidget layout quitButton
  QMainWindow.setCentralWidget window contents

  let me = Notepad
           { myWindow = window
           , myText = text
           }

  _ <- onEvent window $ \(_ :: QCloseEvent) -> do
    putStrLn "Goodbye!"
    return False

  _ <- connect menuFileNew triggeredSignal $ \_ -> fileNew me
  _ <- connect menuFileOpen triggeredSignal $ \_ -> fileOpen me
  _ <- connect menuFileSave triggeredSignal $ \_ -> fileSave me
  _ <- connect quitButton clickedSignal $ \_ -> QWidget.close window

  return window

fileNew :: Notepad -> IO ()
fileNew me = QTextEdit.setText (myText me) ""

fileOpen :: Notepad -> IO ()
fileOpen me = do
  fileName <- QFileDialog.getOpenFileName
              (myWindow me)
              "Open File"
              ""
              fileDialogFilter

  unless (null fileName) $ do
    contents <- readFile fileName
    QTextEdit.setText (myText me) contents

fileSave :: Notepad -> IO ()
fileSave me = do
  fileName <- QFileDialog.getSaveFileName
              (myWindow me)
              "Save File"
              ""
              fileDialogFilter

  unless (null fileName) $ do
    contents <- QTextEdit.toPlainText $ myText me
    writeFile fileName contents

fileDialogFilter :: String
fileDialogFilter = "Text Files (*.txt);;C++ Files (*.cpp *.h);;All Files (*)"
