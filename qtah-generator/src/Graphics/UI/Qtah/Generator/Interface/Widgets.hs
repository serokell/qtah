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

module Graphics.UI.Qtah.Generator.Interface.Widgets (modules) where

import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractGraphicsShapeItem as QAbstractGraphicsShapeItem
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractItemDelegate as QAbstractItemDelegate
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractItemView as QAbstractItemView
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractScrollArea as QAbstractScrollArea
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractSlider as QAbstractSlider
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractSpinBox as QAbstractSpinBox
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QAction as QAction
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QActionGroup as QActionGroup
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QApplication as QApplication
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QButtonGroup as QButtonGroup
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QCheckBox as QCheckBox
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QDateEdit as QDateEdit
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QDateTimeEdit as QDateTimeEdit
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QDial as QDial
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QDialog as QDialog
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QDialogButtonBox as QDialogButtonBox
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QDoubleSpinBox as QDoubleSpinBox
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QFileDialog as QFileDialog
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QFormLayout as QFormLayout
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QFrame as QFrame
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsEllipseItem as QGraphicsEllipseItem
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsItem as QGraphicsItem
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsPolygonItem as QGraphicsPolygonItem
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsRectItem as QGraphicsRectItem
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsScene as QGraphicsScene
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsSceneEvent as QGraphicsSceneEvent
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsSceneMouseEvent as QGraphicsSceneMouseEvent
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsSceneWheelEvent as QGraphicsSceneWheelEvent
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsView as QGraphicsView
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QGridLayout as QGridLayout
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QGroupBox as QGroupBox
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QHBoxLayout as QHBoxLayout
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QInputDialog as QInputDialog
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QLayoutItem as QLayoutItem
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QLineEdit as QLineEdit
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QListView as QListView
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QMainWindow as QMainWindow
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QMenu as QMenu
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QMenuBar as QMenuBar
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QMessageBox as QMessageBox
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QOpenGLWidget as QOpenGLWidget
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QProgressBar as QProgressBar
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QRadioButton as QRadioButton
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QRubberBand as QRubberBand
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QScrollArea as QScrollArea
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QScrollBar as QScrollBar
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QSizePolicy as QSizePolicy
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QSlider as QSlider
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QSpacerItem as QSpacerItem
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QSpinBox as QSpinBox
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QSplitter as QSplitter
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QStackedLayout as QStackedLayout
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QStackedWidget as QStackedWidget
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QStatusBar as QStatusBar
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QStyledItemDelegate as QStyledItemDelegate
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QSystemTrayIcon as QSystemTrayIcon
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QTabWidget as QTabWidget
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QTextEdit as QTextEdit
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QToolBar as QToolBar
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QToolBox as QToolBox
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QTreeView as QTreeView
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QTreeWidget as QTreeWidget
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget as QWidget
import Graphics.UI.Qtah.Generator.Module (AModule)

{-# ANN module "HLint: ignore Use camelCase" #-}

modules :: [AModule]
modules =
  [ QAbstractButton.aModule
  , QAbstractGraphicsShapeItem.aModule
  , QAbstractItemDelegate.aModule
  , QAbstractItemView.aModule
  , QAbstractScrollArea.aModule
  , QAbstractSlider.aModule
  , QAbstractSpinBox.aModule
  , QAction.aModule
  , QActionGroup.aModule
  , QApplication.aModule
  , QBoxLayout.aModule
  , QButtonGroup.aModule
  , QCheckBox.aModule
  , QDateEdit.aModule
  , QDateTimeEdit.aModule
  , QDial.aModule
  , QDialog.aModule
  , QDialogButtonBox.aModule
  , QDoubleSpinBox.aModule
  , QFileDialog.aModule
  , QFormLayout.aModule
  , QFrame.aModule
  , QGraphicsEllipseItem.aModule
  , QGraphicsItem.aModule
  , QGraphicsPolygonItem.aModule
  , QGraphicsRectItem.aModule
  , QGraphicsScene.aModule
  , QGraphicsSceneEvent.aModule
  , QGraphicsSceneMouseEvent.aModule
  , QGraphicsSceneWheelEvent.aModule
  , QGraphicsView.aModule
  , QGridLayout.aModule
  , QGroupBox.aModule
  , QHBoxLayout.aModule
  , QInputDialog.aModule
  , QLabel.aModule
  , QLayout.aModule
  , QLayoutItem.aModule
  , QLineEdit.aModule
  , QListView.aModule
  , QMainWindow.aModule
  , QMenu.aModule
  , QMenuBar.aModule
  , QMessageBox.aModule
  , QOpenGLWidget.aModule
  , QProgressBar.aModule
  , QPushButton.aModule
  , QRadioButton.aModule
  , QRubberBand.aModule
  , QScrollArea.aModule
  , QScrollBar.aModule
  , QSizePolicy.aModule
  , QSlider.aModule
  , QSpacerItem.aModule
  , QSpinBox.aModule
  , QSplitter.aModule
  , QStackedLayout.aModule
  , QStackedWidget.aModule
  , QStatusBar.aModule
  , QStyledItemDelegate.aModule
  , QSystemTrayIcon.aModule
  , QTabWidget.aModule
  , QTextEdit.aModule
  , QToolBar.aModule
  , QToolBox.aModule
  , QTreeView.aModule
  , QTreeWidget.aModule
  , QTreeWidget.itemModule
  , QVBoxLayout.aModule
  , QWidget.aModule
  ]
