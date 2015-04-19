{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.QHBoxLayout (
  mod_QHBoxLayout,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.QBoxLayout
import Graphics.UI.Qtah.Internal.Interface.QWidget

{-# ANN module "HLint: ignore Use camelCase" #-}

this = c_QHBoxLayout
thisQt = qtc_QHBoxLayout
#include "MkQt.hs.inc"

mod_QHBoxLayout =
  makeQtModule "QHBoxLayout" []
  [ QtExportClass thisQt ]

c_QHBoxLayout = qtClassClass qtc_QHBoxLayout

qtc_QHBoxLayout =
  makeQtClass (ident "QHBoxLayout") Nothing [c_QBoxLayout]
  [ _mkCtor "new" []
  , _mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ]
  []
  []
