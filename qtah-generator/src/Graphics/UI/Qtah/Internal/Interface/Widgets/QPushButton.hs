{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Widgets.QPushButton (
  qtModule,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractButton (c_QAbstractButton)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

qtModule = makeQtModuleForClass c_QPushButton []

this = c_QPushButton
#include "../Mk.hs.inc"

c_QPushButton =
  addReqIncludes [includeStd "QPushButton"] $
  makeClass (ident "QPushButton") Nothing
  [ c_QAbstractButton ]
  [ _mkCtor "new" [TPtr $ TObj c_QWidget]
  , _mkCtor "newWithText" [TObj c_QString, TPtr $ TObj c_QWidget]
  ]
  []
