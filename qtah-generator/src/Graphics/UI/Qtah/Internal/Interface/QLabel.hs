{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.QLabel (
  mod_QLabel,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.QFrame
import Graphics.UI.Qtah.Internal.Interface.QString
import Graphics.UI.Qtah.Internal.Interface.QWidget

{-# ANN module "HLint: ignore Use camelCase" #-}

this = c_QLabel
thisQt = qtc_QLabel
#include "MkQt.hs.inc"

mod_QLabel =
  makeQtModule "QLabel" []
  [ QtExportClass thisQt ]

c_QLabel = qtClassClass qtc_QLabel

qtc_QLabel =
  makeQtClass (ident "QLabel") Nothing [c_QFrame]
  [ _mkCtor "new" []
  , _mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  , _mkCtor "newWithText" [TObj c_QString]
  , _mkCtor "newWithTextAndParent" [TObj c_QString, TPtr $ TObj c_QWidget]
  ]
  [ _mkMethod "setText" [TObj c_QString] TVoid
  , _mkConstMethod "text" [] $ TObj c_QString
  ]
  []
