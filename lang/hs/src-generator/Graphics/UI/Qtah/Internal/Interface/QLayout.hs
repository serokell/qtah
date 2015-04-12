{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.QLayout (
  c_QLayout,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Interface.QLayoutItem
import Graphics.UI.Qtah.Internal.Interface.QObject
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.QWidget

this = c_QLayout
#include "Mk.hs.inc"

c_QLayout =
  makeClass (ident "QLayout") Nothing [c_QObject, c_QLayoutItem]
  []
  [ _mkMethod "addWidget" [TPtr $ TObj c_QWidget] TVoid
  , _mkMethod "removeWidget" [TPtr $ TObj c_QWidget] TVoid
  ]
