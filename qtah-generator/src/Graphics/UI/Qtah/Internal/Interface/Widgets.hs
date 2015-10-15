module Graphics.UI.Qtah.Internal.Interface.Widgets (mods_Widgets) where

import Foreign.Hoppy.Generator.Spec (Module)
import Graphics.UI.Qtah.Internal.Generator.Types
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractButton as QAbstractButton
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractScrollArea as QAbstractScrollArea
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QApplication as QApplication
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QAction as QAction
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QActionGroup as QActionGroup
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Internal.Interface.Widgets.QCheckBox as QCheckBox
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
  [ (QAbstractButton.hoppyModule, QAbstractButton.qtModule)
  , (QAbstractScrollArea.hoppyModule, QAbstractScrollArea.qtModule)
  , (QApplication.hoppyModule, QApplication.qtModule)
  , (QAction.hoppyModule, QAction.qtModule)
  , (QActionGroup.hoppyModule, QActionGroup.qtModule)
  , (QBoxLayout.hoppyModule, QBoxLayout.qtModule)
  , (QCheckBox.hoppyModule, QCheckBox.qtModule)
  , (QFrame.hoppyModule, QFrame.qtModule)
  , (QHBoxLayout.hoppyModule, QHBoxLayout.qtModule)
  , (QLabel.hoppyModule, QLabel.qtModule)
  , (QLayout.hoppyModule, QLayout.qtModule)
  , (QLayoutItem.hoppyModule, QLayoutItem.qtModule)
  , (QLineEdit.hoppyModule, QLineEdit.qtModule)
  , (QMainWindow.hoppyModule, QMainWindow.qtModule)
  , (QMenu.hoppyModule, QMenu.qtModule)
  , (QMenuBar.hoppyModule, QMenuBar.qtModule)
  , (QPushButton.hoppyModule, QPushButton.qtModule)
  , (QSplitter.hoppyModule, QSplitter.qtModule)
  , (QTextEdit.hoppyModule, QTextEdit.qtModule)
  , (QVBoxLayout.hoppyModule, QVBoxLayout.qtModule)
  , (QWidget.hoppyModule, QWidget.qtModule)
  ]
