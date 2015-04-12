{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.QVBoxLayout (
  c_QVBoxLayout,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Interface.QBoxLayout
import Graphics.UI.Qtah.Internal.Interface.QWidget

this = c_QVBoxLayout
#include "Mk.hs.inc"

c_QVBoxLayout =
  makeClass (ident "QVBoxLayout") Nothing [c_QBoxLayout]
  [ _mkCtor "new" []
  , _mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ]
  []
