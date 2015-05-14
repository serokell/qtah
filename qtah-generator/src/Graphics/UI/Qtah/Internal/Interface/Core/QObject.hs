{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Core.QObject (
  qtModule,
  c_QObject,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Listener
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)

{-# ANN module "HLint: ignore Use camelCase" #-}

qtModule = makeQtModuleForClass c_QObject $ map QtExportSignal signals

this = c_QObject
#include "../Mk.hs.inc"

c_QObject =
  addReqIncludes [includeStd "QObject"] $
  makeClass (ident "QObject") Nothing []
  []
  [ _mkMethod "blockSignals" [TBool] TBool
  , _mkMethod "dumpObjectInfo" [] TVoid
  , _mkMethod "dumpObjectTree" [] TVoid
  , _mkMethod "installEventFilter" [TPtr $ TObj c_QObject] TVoid
  , _mkConstMethod "isWidgetType" [] TBool
  , _mkMethod "killTimer" [TInt] TVoid
  , _mkConstMethod "objectName" [] $ TObj c_QString
  , _mkConstMethod "parent" [] $ TPtr $ TObj c_QObject
  , _mkMethod "removeEventFilter" [TPtr $ TObj c_QObject] TVoid
  , _mkMethod "setParent" [TPtr $ TObj c_QObject] TVoid
  , _mkConstMethod "signalsBlocked" [] TBool
  , _mkMethod "startTimer" [TInt] TInt
  ]

signals =
  [ _mkSignal "destroyed" c_ListenerPtrQObject
  ]
