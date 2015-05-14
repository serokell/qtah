{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Widgets.QMainWindow (
  qtModule,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

qtModule = makeQtModuleForClass c_QMainWindow []

this = c_QMainWindow
#include "../Mk.hs.inc"

c_QMainWindow =
  addReqIncludes [includeStd "QMainWindow"] $
  makeClass (ident "QMainWindow") Nothing [c_QWidget]
  [ _mkCtor "new" [TPtr $ TObj c_QWidget] ]
  [ _mkMethod "setCentralWidget" [TPtr $ TObj c_QWidget] TVoid
  ]
