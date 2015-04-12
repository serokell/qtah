{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.QHBoxLayout (
  c_QHBoxLayout,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Interface.QBoxLayout
import Graphics.UI.Qtah.Internal.Interface.QWidget

this = c_QHBoxLayout
#include "Mk.hs.inc"

c_QHBoxLayout =
  makeClass (ident "QHBoxLayout") Nothing [c_QBoxLayout]
  [ _mkCtor "new" []
  , _mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ]
  []
