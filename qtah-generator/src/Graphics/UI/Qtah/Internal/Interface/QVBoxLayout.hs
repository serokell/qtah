{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.QVBoxLayout (
  mod_QVBoxLayout,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.QBoxLayout
import Graphics.UI.Qtah.Internal.Interface.QWidget

this = c_QVBoxLayout
thisQt = qtc_QVBoxLayout
#include "MkQt.hs.inc"

mod_QVBoxLayout =
  makeQtModule "QVBoxLayout" []
  [ QtExportClass qtc_QVBoxLayout ]

c_QVBoxLayout = qtClassClass qtc_QVBoxLayout

qtc_QVBoxLayout =
  makeQtClass (ident "QVBoxLayout") Nothing [c_QBoxLayout]
  [ _mkCtor "new" []
  , _mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ]
  []
  []
