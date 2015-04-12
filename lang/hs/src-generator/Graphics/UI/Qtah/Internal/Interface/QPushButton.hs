{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.QPushButton (
  c_QPushButton,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Interface.QAbstractButton
import Graphics.UI.Qtah.Internal.Interface.QString
import Graphics.UI.Qtah.Internal.Interface.QWidget

this = c_QPushButton
#include "Mk.hs.inc"

c_QPushButton =
  makeClass (ident "QPushButton") Nothing
  [ c_QAbstractButton ]
  [ _mkCtor "new" [TPtr $ TObj c_QWidget]
  , _mkCtor "newWithText" [TObj c_QString, TPtr $ TObj c_QWidget]
  ]
  []
