{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.QMainWindow (
  c_QMainWindow,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Interface.QWidget

this = c_QMainWindow
#include "Mk.hs.inc"

c_QMainWindow =
  makeClass (ident "QMainWindow") Nothing  [c_QWidget]
  [ _mkCtor "new" [TPtr $ TObj c_QWidget] ]
  [ _mkMethod "setCentralWidget" [TPtr $ TObj c_QWidget] TVoid
  ]
