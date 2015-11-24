-- This file is part of Qtah.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
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

module Graphics.UI.Qtah.Internal.Interface.Widgets (modules) where

import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractScrollArea as QAbstractScrollArea
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QApplication as QApplication
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QAction as QAction
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QActionGroup as QActionGroup
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QButtonGroup as QButtonGroup
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QCheckBox as QCheckBox
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QDialog as QDialog
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QFileDialog as QFileDialog
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QFormLayout as QFormLayout
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QFrame as QFrame
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QGridLayout as QGridLayout
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QGroupBox as QGroupBox
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QHBoxLayout as QHBoxLayout
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QLayoutItem as QLayoutItem
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QLineEdit as QLineEdit
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QMainWindow as QMainWindow
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QMenu as QMenu
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QMenuBar as QMenuBar
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QRadioButton as QRadioButton
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QSplitter as QSplitter
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QStackedLayout as QStackedLayout
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QStatusBar as QStatusBar
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QTextEdit as QTextEdit
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget as QWidget

{-# ANN module "HLint: ignore Use camelCase" #-}

modules :: [AModule]
modules =
  collect
  [ just QAbstractButton.aModule
  , just QAbstractScrollArea.aModule
  , just QApplication.aModule
  , just QAction.aModule
  , just QActionGroup.aModule
  , just QBoxLayout.aModule
  , just QButtonGroup.aModule
  , just QCheckBox.aModule
  , just QDialog.aModule
  , just QFileDialog.aModule
  , test (qtVersion >= QFormLayout.minVersion) QFormLayout.aModule
  , just QFrame.aModule
  , just QGridLayout.aModule
  , just QGroupBox.aModule
  , just QHBoxLayout.aModule
  , just QLabel.aModule
  , just QLayout.aModule
  , just QLayoutItem.aModule
  , just QLineEdit.aModule
  , just QMainWindow.aModule
  , just QMenu.aModule
  , just QMenuBar.aModule
  , just QPushButton.aModule
  , just QRadioButton.aModule
  , just QSplitter.aModule
  , just QStackedLayout.aModule
  , just QStatusBar.aModule
  , just QTextEdit.aModule
  , just QVBoxLayout.aModule
  , just QWidget.aModule
  ]
