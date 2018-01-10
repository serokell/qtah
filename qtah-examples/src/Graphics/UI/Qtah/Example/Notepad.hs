-- This file is part of Qtah.
--
-- Copyright 2015-2018 The Qtah Authors.
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

-- | A notepad based on the Qt notepad example.
module Graphics.UI.Qtah.Example.Notepad (run) where

import Control.Monad (forM_, unless, when)
import Data.Bits ((.|.))
import Data.Functor (void)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
import Graphics.UI.Qtah.Event
import Graphics.UI.Qtah.Gui.QCloseEvent (QCloseEvent)
import Graphics.UI.Qtah.Signal (connect_)
import qualified Graphics.UI.Qtah.Core.QEvent as QEvent
import qualified Graphics.UI.Qtah.Widgets.QAction as QAction
import Graphics.UI.Qtah.Widgets.QAction (triggeredSignal)
import qualified Graphics.UI.Qtah.Widgets.QFileDialog as QFileDialog
import qualified Graphics.UI.Qtah.Widgets.QMainWindow as QMainWindow
import Graphics.UI.Qtah.Widgets.QMainWindow (QMainWindow)
import qualified Graphics.UI.Qtah.Widgets.QMenu as QMenu
import qualified Graphics.UI.Qtah.Widgets.QMenuBar as QMenuBar
import qualified Graphics.UI.Qtah.Widgets.QMessageBox as QMessageBox
import qualified Graphics.UI.Qtah.Widgets.QTextEdit as QTextEdit
import Graphics.UI.Qtah.Widgets.QTextEdit (
  QTextEdit,
  copyAvailableSignal,
  redoAvailableSignal,
  textChangedSignal,
  undoAvailableSignal,
  )
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import System.FilePath (takeFileName)

data Notepad = Notepad
  { myWindow :: QMainWindow
  , myText :: QTextEdit
  , myFilePathRef :: IORef (Maybe FilePath)
  , myDirtyRef :: IORef Bool
  }

run :: IO ()
run = do
  mainWindow <- makeMainWindow
  QWidget.show mainWindow

makeMainWindow :: IO QMainWindow
makeMainWindow = do
  window <- QMainWindow.new
  QWidget.resizeRaw window 640 480

  menu <- QMenuBar.new
  QMainWindow.setMenuBar window menu

  menuFile <- QMenuBar.addNewMenu menu "&File"
  menuFileNew <- QMenu.addNewAction menuFile "&New"
  menuFileOpen <- QMenu.addNewAction menuFile "&Open..."
  menuFileSave <- QMenu.addNewAction menuFile "&Save"
  menuFileSaveAs <- QMenu.addNewAction menuFile "Sa&ve as..."
  _ <- QMenu.addSeparator menuFile
  menuFileClose <- QMenu.addNewAction menuFile "&Close"
  menuFileQuit <- QMenu.addNewAction menuFile "&Quit Examples"

  menuEdit <- QMenuBar.addNewMenu menu "&Edit"
  menuEditUndo <- QMenu.addNewAction menuEdit "&Undo"
  menuEditRedo <- QMenu.addNewAction menuEdit "&Redo"
  _ <- QMenu.addSeparator menuEdit
  menuEditCut <- QMenu.addNewAction menuEdit "Cu&t"
  menuEditCopy <- QMenu.addNewAction menuEdit "&Copy"
  menuEditPaste <- QMenu.addNewAction menuEdit "&Paste"
  _ <- QMenu.addSeparator menuEdit
  menuEditSelectAll <- QMenu.addNewAction menuEdit "Select all"
  forM_ [menuEditUndo, menuEditRedo, menuEditCut, menuEditCopy] $ \action ->
    QAction.setEnabled action False

  text <- QTextEdit.new
  QMainWindow.setCentralWidget window text
  QTextEdit.setUndoRedoEnabled text True

  filePathRef <- newIORef Nothing
  dirtyRef <- newIORef False

  let me = Notepad
           { myWindow = window
           , myText = text
           , myFilePathRef = filePathRef
           , myDirtyRef = dirtyRef
           }

  _ <- onEvent window $ \(event :: QCloseEvent) -> do
    continue <- confirmSaveIfDirty me "Quit"
    unless continue $ QEvent.ignore event
    return $ not continue

  connect_ menuFileNew triggeredSignal $ \_ -> fileNew me
  connect_ menuFileOpen triggeredSignal $ \_ -> fileOpen me
  connect_ menuFileSave triggeredSignal $ \_ -> void $ fileSave me
  connect_ menuFileSaveAs triggeredSignal $ \_ -> void $ fileSaveAs me
  connect_ menuFileClose triggeredSignal $ \_ -> void $ QWidget.close window
  connect_ menuFileQuit triggeredSignal $ \_ -> do
    closed <- QWidget.close window
    when closed QCoreApplication.quit

  connect_ menuEditUndo triggeredSignal $ \_ -> QTextEdit.undo text
  connect_ menuEditRedo triggeredSignal $ \_ -> QTextEdit.redo text
  connect_ menuEditCut triggeredSignal $ \_ -> QTextEdit.cut text
  connect_ menuEditCopy triggeredSignal $ \_ -> QTextEdit.copy text
  connect_ menuEditPaste triggeredSignal $ \_ -> QTextEdit.paste text
  connect_ menuEditSelectAll triggeredSignal $ \_ -> QTextEdit.selectAll text

  connect_ text textChangedSignal $ setDirty me True
  connect_ text undoAvailableSignal $ \b -> QAction.setEnabled menuEditUndo b
  connect_ text redoAvailableSignal $ \b -> QAction.setEnabled menuEditRedo b
  connect_ text copyAvailableSignal $ \b -> do
    QAction.setEnabled menuEditCut b
    QAction.setEnabled menuEditCopy b

  updateTitle me
  return window

fileNew :: Notepad -> IO ()
fileNew me = do
  continue <- confirmSaveIfDirty me "New file"
  when continue $ do
    QTextEdit.clear $ myText me
    setFilePath me Nothing
    setDirty me False

fileOpen :: Notepad -> IO ()
fileOpen me = do
  continue <- confirmSaveIfDirty me "Open file"
  when continue $ do
    path <- QFileDialog.getOpenFileName (myWindow me) "Open file" "" fileDialogFilter
    unless (null path) $ do
      contents <- readFile path
      QTextEdit.setText (myText me) contents
      setFilePath me $ Just path
      setDirty me False

-- | Returns true if the save was performed.
fileSave :: Notepad -> IO Bool
fileSave me = do
  pathMaybe <- readIORef $ myFilePathRef me
  case pathMaybe of
    Nothing -> fileSaveAs me
    Just path -> do
      contents <- QTextEdit.toPlainText $ myText me
      writeFile path contents
      setDirty me False
      return True

-- | Returns true if the save was performed.
fileSaveAs :: Notepad -> IO Bool
fileSaveAs me = do
  path <- QFileDialog.getSaveFileName
          (myWindow me)
          "Save file"
          ""
          fileDialogFilter

  if null path
    then return False
    else do setFilePath me $ Just path
            fileSave me

-- | Returns true if the surrounding action should continue: that is, if the
-- editor was not dirty, or if the editor was dirty and the save was performed.
confirmSaveIfDirty :: Notepad -> String -> IO Bool
confirmSaveIfDirty me title = do
  let dirtyRef = myDirtyRef me
  dirty <- readIORef dirtyRef
  if dirty
    then do response <- QMessageBox.questionWithButtons
                        (myWindow me)
                        title
                        "There are unsaved changes.  Would you like to save them?"
                        (QMessageBox.yes .|.
                         QMessageBox.no .|.
                         QMessageBox.cancel)
                        QMessageBox.Cancel
            case response of
              QMessageBox.Yes -> fileSave me
              QMessageBox.No -> return True
              _ -> return False
    else return True

setDirty :: Notepad -> Bool -> IO ()
setDirty me dirty = do
  let ref = myDirtyRef me
  dirtyOld <- readIORef ref
  when (dirty /= dirtyOld) $ do
    writeIORef ref dirty
    updateTitle me

setFilePath :: Notepad -> Maybe FilePath -> IO ()
setFilePath me path = do
  writeIORef (myFilePathRef me) path
  updateTitle me

updateTitle :: Notepad -> IO ()
updateTitle me = do
  dirty <- readIORef $ myDirtyRef me
  file <- fmap (maybe "(Untitled)" takeFileName) $ readIORef $ myFilePathRef me
  QWidget.setWindowTitle (myWindow me) $
    (if dirty then ('*':) else id) $
    file ++ " - Notepad"

fileDialogFilter :: String
fileDialogFilter =
  "Text Files (*.txt);;Haskell sources (*.hs *.hs-boot *.lhs *.chs);;All Files (*)"
