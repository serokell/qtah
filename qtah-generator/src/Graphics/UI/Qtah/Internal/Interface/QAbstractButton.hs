{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.QAbstractButton (
  mod_QAbstractButton,
  c_QAbstractButton,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Listener
import Graphics.UI.Qtah.Internal.Interface.QString
import Graphics.UI.Qtah.Internal.Interface.QWidget

{-# ANN module "HLint: ignore Use camelCase" #-}

this = c_QAbstractButton
thisQt = qtc_QAbstractButton
#include "MkQt.hs.inc"

mod_QAbstractButton =
  makeQtModule "QAbstractButton" []
  [ QtExportClass thisQt ]

c_QAbstractButton = qtClassClass qtc_QAbstractButton

qtc_QAbstractButton =
  makeQtClass (ident "QAbstractButton") Nothing
  [ c_QWidget ]
  []
  [ _mkMethod "setText" [TObj c_QString] TVoid
  , _mkConstMethod "text" [] $ TObj c_QString
  ]
  [ _mkSignal "clicked" c_ListenerBool
  ]
