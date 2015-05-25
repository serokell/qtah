{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Core.QObject (
  cppopModule,
  qtModule,
  c_QObject,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Listener
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
#include "../Mk.hs.inc"

cppopModule = makeCppopModule "Core" "QObject" qtModule

qtModule =
  makeQtModule "Core.QObject" $
  [ QtExport $ ExportClass c_QObject
  ] ++ map QtExportSignal signals

this = c_QObject

c_QObject =
  addReqIncludes [includeStd "QObject"] $
  makeClass (ident "QObject") Nothing []
  [] $
  [ _mkMethod "blockSignals" [TBool] TBool
  , _mkMethod "dumpObjectInfo" [] TVoid
  , _mkMethod "dumpObjectTree" [] TVoid
  , _mkMethod "installEventFilter" [TPtr $ TObj c_QObject] TVoid
  , _mkConstMethod "isWidgetType" [] TBool
  , _mkMethod "killTimer" [TInt] TVoid
  , _mkConstMethod "objectName" [] $ TObj c_QString
  , _mkMethod "removeEventFilter" [TPtr $ TObj c_QObject] TVoid
  , _mkConstMethod "signalsBlocked" [] TBool
  , _mkMethod "startTimer" [TInt] TInt
  ] ++
  _props
  [ _mkProp "parent" $ TPtr $ TObj c_QObject
  ]

signals =
  [ _mkSignal "destroyed" c_ListenerPtrQObject
  ]
