{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.QAbstractScrollArea (
  c_QAbstractScrollArea,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Interface.QWidget

this = c_QAbstractScrollArea
#include "Mk.hs.inc"

c_QAbstractScrollArea =
  makeClass (ident "QAbstractScrollArea") Nothing [c_QWidget]
  [ _mkCtor "new" []
  , _mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ]
  []
