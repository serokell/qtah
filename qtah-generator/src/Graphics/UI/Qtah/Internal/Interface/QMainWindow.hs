{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.QMainWindow (
  mod_QMainWindow,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.QWidget

this = c_QMainWindow
thisQt = qtc_QMainWindow
#include "MkQt.hs.inc"

mod_QMainWindow =
  makeQtModule "QMainWindow"
  [ QtExportClass thisQt ]

c_QMainWindow = qtClassClass qtc_QMainWindow

qtc_QMainWindow =
  makeQtClass (ident "QMainWindow") Nothing  [c_QWidget]
  [ _mkCtor "new" [TPtr $ TObj c_QWidget] ]
  [ _mkMethod "setCentralWidget" [TPtr $ TObj c_QWidget] TVoid
  ]
  []
