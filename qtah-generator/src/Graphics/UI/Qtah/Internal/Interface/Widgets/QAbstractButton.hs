{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractButton (
  qtModule,
  c_QAbstractButton,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Listener (c_ListenerBool)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

qtModule = makeQtModuleForClass c_QAbstractButton $ map QtExportSignal signals

this = c_QAbstractButton
#include "../Mk.hs.inc"

c_QAbstractButton =
  addReqIncludes [includeStd "QAbstractButton"] $
  makeClass (ident "QAbstractButton") Nothing
  [ c_QWidget ]
  []
  [ _mkMethod "setText" [TObj c_QString] TVoid
  , _mkConstMethod "text" [] $ TObj c_QString
  ]

signals =
  [ _mkSignal "clicked" c_ListenerBool
  ]
