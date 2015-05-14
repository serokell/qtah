{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractScrollArea (
  qtModule,
  c_QAbstractScrollArea,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

qtModule = makeQtModuleForClass c_QAbstractScrollArea []

this = c_QAbstractScrollArea
#include "../Mk.hs.inc"

c_QAbstractScrollArea =
  addReqIncludes [includeStd "QAbstractScrollArea"] $
  makeClass (ident "QAbstractScrollArea") Nothing [c_QWidget]
  [ _mkCtor "new" []
  , _mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ]
  []
