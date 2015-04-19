{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.QWidget (
  mod_QWidget,
  c_QWidget,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.QLayout
import Graphics.UI.Qtah.Internal.Interface.QObject
import Graphics.UI.Qtah.Internal.Interface.QString

{-# ANN module "HLint: ignore Use camelCase" #-}

this = c_QWidget
thisQt = qtc_QWidget
#include "MkQt.hs.inc"

mod_QWidget =
  makeQtModule "QWidget" []
  [ QtExportClass qtc_QWidget ]

c_QWidget = qtClassClass qtc_QWidget

qtc_QWidget =
  makeQtClass (ident "QWidget") Nothing
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
  []
