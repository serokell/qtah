{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.QWidget (
  c_QWidget,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Interface.QObject
import Graphics.UI.Qtah.Internal.Interface.QString

this = c_QWidget
#include "Mk.hs.inc"

c_QWidget =
  makeClass (ident "QWidget") Nothing
  [ c_QObject ]
  [ _mkCtor "new" [TPtr $ TObj c_QWidget] ]
  [ _mkMethod "resize" [TInt, TInt] TVoid
  , _mkMethod "setWindowTitle" [TObj c_QString] TVoid
  , _mkMethod "show" [] TVoid
  ]
