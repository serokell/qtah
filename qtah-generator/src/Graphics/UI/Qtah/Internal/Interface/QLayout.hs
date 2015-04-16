{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.QLayout (
  mod_QLayout,
  c_QLayout,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.QLayoutItem
import Graphics.UI.Qtah.Internal.Interface.QObject
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.QWidget

this = c_QLayout
thisQt = qtc_QLayout
#include "MkQt.hs.inc"

mod_QLayout =
  makeQtModule "QLayout"
  [ QtExportClass qtc_QLayout ]

c_QLayout = qtClassClass qtc_QLayout

qtc_QLayout =
  makeQtClass (ident "QLayout") Nothing [c_QObject, c_QLayoutItem]
  []
  [ _mkMethod "addWidget" [TPtr $ TObj c_QWidget] TVoid
  , _mkMethod "removeWidget" [TPtr $ TObj c_QWidget] TVoid
  ]
  []
