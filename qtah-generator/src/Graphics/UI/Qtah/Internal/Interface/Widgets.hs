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

module Graphics.UI.Qtah.Internal.Interface.Widgets (modules) where

import Graphics.UI.Qtah.Internal.Generator.Types
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractScrollArea as QAbstractScrollArea
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractSlider as QAbstractSlider
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractSpinBox as QAbstractSpinBox
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QAction as QAction
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QActionGroup as QActionGroup
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QApplication as QApplication
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QButtonGroup as QButtonGroup
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QCheckBox as QCheckBox
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QDial as QDial
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QDialog as QDialog
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QDialogButtonBox as QDialogButtonBox
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QDoubleSpinBox as QDoubleSpinBox
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
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QMessageBox as QMessageBox
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QProgressBar as QProgressBar
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QRadioButton as QRadioButton
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QRubberBand as QRubberBand
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QScrollArea as QScrollArea
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QScrollBar as QScrollBar
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QSlider as QSlider
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QSpinBox as QSpinBox
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QSplitter as QSplitter
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QStackedLayout as QStackedLayout
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QStackedWidget as QStackedWidget
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QStatusBar as QStatusBar
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QTabWidget as QTabWidget
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QTextEdit as QTextEdit
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget as QWidget

{-# ANN module "HLint: ignore Use camelCase" #-}

modules :: [AModule]
modules =
  [ QAbstractButton.aModule
  , QAbstractScrollArea.aModule
  , QAbstractSlider.aModule
  , QAbstractSpinBox.aModule
  , QAction.aModule
  , QActionGroup.aModule
  , QApplication.aModule
  , QBoxLayout.aModule
  , QButtonGroup.aModule
  , QCheckBox.aModule
  , QDial.aModule
  , QDialog.aModule
  , QDialogButtonBox.aModule
  , QDoubleSpinBox.aModule
  , QFileDialog.aModule
  , QFormLayout.aModule
  , QFrame.aModule
  , QGridLayout.aModule
  , QGroupBox.aModule
  , QHBoxLayout.aModule
  , QLabel.aModule
  , QLayout.aModule
  , QLayoutItem.aModule
  , QLineEdit.aModule
  , QMainWindow.aModule
  , QMenu.aModule
  , QMenuBar.aModule
  , QMessageBox.aModule
  , QProgressBar.aModule
  , QPushButton.aModule
  , QRadioButton.aModule
  , QRubberBand.aModule
  , QScrollArea.aModule
  , QScrollBar.aModule
  , QSlider.aModule
  , QSpinBox.aModule
  , QSplitter.aModule
  , QStackedLayout.aModule
  , QStackedWidget.aModule
  , QStatusBar.aModule
  , QTabWidget.aModule
  , QTextEdit.aModule
  , QVBoxLayout.aModule
  , QWidget.aModule
  ]
