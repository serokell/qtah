module Graphics.UI.Qtah.Internal.Interface.Widgets (mods_Widgets) where

import Foreign.Cppop.Generator.Spec (Module)
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
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QSplitter as QSplitter
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QTextEdit as QTextEdit
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget as QWidget

{-# ANN module "HLint: ignore Use camelCase" #-}

mods_Widgets :: [(Module, QtModule)]
mods_Widgets =
  [ (QAbstractButton.cppopModule, QAbstractButton.qtModule)
  , (QAbstractScrollArea.cppopModule, QAbstractScrollArea.qtModule)
  , (QApplication.cppopModule, QApplication.qtModule)
  , (QAction.cppopModule, QAction.qtModule)
  , (QActionGroup.cppopModule, QActionGroup.qtModule)
  , (QBoxLayout.cppopModule, QBoxLayout.qtModule)
  , (QFrame.cppopModule, QFrame.qtModule)
  , (QHBoxLayout.cppopModule, QHBoxLayout.qtModule)
  , (QLabel.cppopModule, QLabel.qtModule)
  , (QLayout.cppopModule, QLayout.qtModule)
  , (QLayoutItem.cppopModule, QLayoutItem.qtModule)
  , (QLineEdit.cppopModule, QLineEdit.qtModule)
  , (QMainWindow.cppopModule, QMainWindow.qtModule)
  , (QMenu.cppopModule, QMenu.qtModule)
  , (QMenuBar.cppopModule, QMenuBar.qtModule)
  , (QPushButton.cppopModule, QPushButton.qtModule)
  , (QSplitter.cppopModule, QSplitter.qtModule)
  , (QTextEdit.cppopModule, QTextEdit.qtModule)
  , (QVBoxLayout.cppopModule, QVBoxLayout.qtModule)
  , (QWidget.cppopModule, QWidget.qtModule)
  ]
