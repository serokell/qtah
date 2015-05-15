module Graphics.UI.Qtah.Internal.Interface.Widgets (mod_Widgets, qmods_Widgets) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractScrollArea as QAbstractScrollArea
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QApplication as QApplication
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QAction as QAction
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QActionGroup as QActionGroup
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QFrame as QFrame
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QHBoxLayout as QHBoxLayout
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QLayoutItem as QLayoutItem
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QLineEdit as QLineEdit
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QMainWindow as QMainWindow
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QMenu as QMenu
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QMenuBar as QMenuBar
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QTextEdit as QTextEdit
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget as QWidget

{-# ANN module "HLint: ignore Use camelCase" #-}

mod_Widgets :: Module
mod_Widgets = modifyModule' (makeModule "widgets" "widgets.hpp" "widgets.cpp") $
  addModuleExports $ concatMap qtModuleExports qmods_Widgets

qmods_Widgets :: [QtModule]
qmods_Widgets =
  [ QAbstractButton.qtModule
  , QAbstractScrollArea.qtModule
  , QApplication.qtModule
  , QAction.qtModule
  , QActionGroup.qtModule
  , QBoxLayout.qtModule
  , QFrame.qtModule
  , QHBoxLayout.qtModule
  , QLabel.qtModule
  , QLayout.qtModule
  , QLayoutItem.qtModule
  , QLineEdit.qtModule
  , QMainWindow.qtModule
  , QMenu.qtModule
  , QMenuBar.qtModule
  , QPushButton.qtModule
  , QTextEdit.qtModule
  , QVBoxLayout.qtModule
  , QWidget.qtModule
  ]
