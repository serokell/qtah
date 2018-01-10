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

module Main where

import Control.Arrow ((&&&))
import Control.Monad (forM_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map as M
import Foreign.Hoppy.Runtime (withScopedPtr)
import qualified Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
import qualified Graphics.UI.Qtah.Core.QItemSelectionModel as QItemSelectionModel
import qualified Graphics.UI.Qtah.Core.QModelIndex as QModelIndex
import qualified Graphics.UI.Qtah.Core.QStringListModel as QStringListModel
import qualified Graphics.UI.Qtah.Core.QVariant as QVariant
import Graphics.UI.Qtah.Event (onEvent)
import qualified Graphics.UI.Qtah.Example.Notepad as Notepad
import qualified Graphics.UI.Qtah.Gui.QCloseEvent as QCloseEvent
import qualified Graphics.UI.Qtah.Gui.QFont as QFont
import qualified Graphics.UI.Qtah.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Widgets.QAbstractItemView as QAbstractItemView
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QListView as QListView
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Widgets.QSplitter as QSplitter
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import Graphics.UI.Qtah.Signal (connect_)
import System.Environment (getArgs)

data Example = Example
  { exTitle :: String
  , exDescription :: String
  , exMain :: IO ()
  }

examples :: [Example]
examples =
  [ Example
    { exTitle = "Notepad"
    , exDescription = "A notepad program for editing text files."
    , exMain = Notepad.run
    }

  ]

examplesByTitle :: M.Map String Example
examplesByTitle = M.fromList $ map (exTitle &&& id) examples

-- | State of the example chooser UI.
data UI = UI
  { uiWindow :: QWidget.QWidget
  , uiListModel :: QStringListModel.QStringListModel
  , uiCurrentExampleRef :: IORef (Maybe Example)
  , uiDescriptionLabel :: QLabel.QLabel
  }

main :: IO ()
main = withScopedPtr (getArgs >>= QApplication.new) $ \_ -> do
  ui <- newChooserWindow
  QWidget.show $ uiWindow ui
  QCoreApplication.exec

newChooserWindow :: IO UI
newChooserWindow = do
  -- Create and initialize widgets.

  window <- QWidget.new
  QWidget.setWindowTitle window "Qtah Examples"
  QWidget.resizeRaw window 500 350

  model <- QStringListModel.newWithContents $ M.keys examplesByTitle
  listView <- QListView.new
  QAbstractItemView.setModel listView model
  QAbstractItemView.setEditTriggers listView QAbstractItemView.noEditTriggers

  titleLabel <- QLabel.newWithText "Qtah Example Programs"
  titleFont <- QWidget.font titleLabel
  QFont.setPixelSize titleFont 25
  QWidget.setFont titleLabel titleFont

  descriptionLabel <- QLabel.new
  QLabel.setWordWrap descriptionLabel True

  runButton <- QPushButton.newWithText "&Launch"

  quitButton <- QPushButton.newWithText "&Quit"

  rightBox <- QWidget.new
  rightBoxLayout <- QVBoxLayout.new
  QWidget.setLayout rightBox rightBoxLayout
  QBoxLayout.addWidget rightBoxLayout descriptionLabel
  QBoxLayout.addStretch rightBoxLayout
  QBoxLayout.addWidget rightBoxLayout runButton
  QBoxLayout.addWidget rightBoxLayout quitButton

  splitter <- QSplitter.new
  QSplitter.addWidget splitter listView
  QSplitter.addWidget splitter rightBox
  QSplitter.setSizes splitter [200 :: Int, 300]

  layout <- QVBoxLayout.newWithParent window
  QBoxLayout.addWidget layout titleLabel
  QBoxLayout.addWidgetWithStretch layout splitter 1

  -- Set up signals.

  currentExampleRef <- newIORef Nothing

  let ui = UI { uiWindow = window
              , uiListModel = model
              , uiCurrentExampleRef = currentExampleRef
              , uiDescriptionLabel = descriptionLabel
              }

  _ <- onEvent window $ \(_ :: QCloseEvent.QCloseEvent) -> do
    QCoreApplication.quit
    return False

  selectionModel <- QAbstractItemView.selectionModel listView
  connect_ selectionModel QItemSelectionModel.currentChangedSignal $ \index _ ->
    exampleSelected ui index

  connect_ listView QAbstractItemView.doubleClickedSignal $ \_ -> runSelectedExample ui

  connect_ runButton QAbstractButton.clickedSignal $ \_ -> runSelectedExample ui

  connect_ quitButton QAbstractButton.clickedSignal $ \_ -> QCoreApplication.quit

  return ui

exampleSelected :: UI -> QModelIndex.QModelIndex -> IO ()
exampleSelected ui index = do
  name <- QVariant.toString =<< QModelIndex.getData index
  -- If we can't find the example, then do nothing.
  forM_ (M.lookup name examplesByTitle) $ \example -> do
    writeIORef (uiCurrentExampleRef ui) $ Just example
    QLabel.setText (uiDescriptionLabel ui) $ exDescription example

runSelectedExample :: UI -> IO ()
runSelectedExample ui = do
  maybeExample <- readIORef $ uiCurrentExampleRef ui
  forM_ maybeExample $ \example -> exMain example
