{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (
  qtModule,
  c_QWidget,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QLayout (c_QLayout)

{-# ANN module "HLint: ignore Use camelCase" #-}

qtModule = makeQtModuleForClass c_QWidget []

this = c_QWidget
#include "../Mk.hs.inc"

c_QWidget =
  addReqIncludes [includeStd "QWidget"] $
  makeClass (ident "QWidget") Nothing
  [ c_QObject ]
  [ _mkCtor "new" []
  , _mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ]
  [ _mkConstMethod "layout" [] $ TPtr $ TObj c_QLayout
  , _mkMethod "resize" [TInt, TInt] TVoid
  , _mkMethod "setLayout" [TPtr $ TObj c_QLayout] TVoid
  , _mkMethod "setWindowTitle" [TObj c_QString] TVoid
  , _mkMethod "show" [] TVoid
  ]
