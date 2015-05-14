{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Widgets.QFrame (
  qtModule,
  c_QFrame,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

qtModule = makeQtModuleForClass c_QFrame []

this = c_QFrame
#include "../Mk.hs.inc"

c_QFrame =
  addReqIncludes [includeStd "QFrame"] $
  makeClass (ident "QFrame") Nothing [c_QWidget]
  [ _mkCtor "new" []
  , _mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ]
  [ _mkConstMethod "frameWidth" [] TInt
  , _mkConstMethod "lineWidth" [] TInt
  , _mkConstMethod "midLineWidth" [] TInt
  , _mkConstMethod "setLineWidth" [TInt] TVoid
  , _mkConstMethod "setMidLineWidth" [TInt] TVoid
  ]
