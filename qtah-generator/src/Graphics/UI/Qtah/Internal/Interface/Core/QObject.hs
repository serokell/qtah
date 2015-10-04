module Graphics.UI.Qtah.Internal.Interface.Core.QObject (
  cppopModule,
  qtModule,
  c_QObject,
  ) where

import Foreign.Cppop.Generator.Spec (
  Export (ExportClass),
  Type (TBool, TInt, TObj, TPtr, TVoid),
  addReqIncludes,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkMethod,
  mkProp,
  mkProps,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Listener
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)

{-# ANN module "HLint: ignore Use camelCase" #-}

cppopModule = makeCppopModule "Core" "QObject" qtModule

qtModule =
  makeQtModule "Core.QObject" $
  [ QtExport $ ExportClass c_QObject
  ] ++ map QtExportSignal signals

c_QObject =
  addReqIncludes [includeStd "QObject"] $
  makeClass (ident "QObject") Nothing []
  [] $
  [ mkMethod "blockSignals" [TBool] TBool
  , mkMethod "dumpObjectInfo" [] TVoid
  , mkMethod "dumpObjectTree" [] TVoid
  , mkMethod "installEventFilter" [TPtr $ TObj c_QObject] TVoid
  , mkConstMethod "isWidgetType" [] TBool
  , mkMethod "killTimer" [TInt] TVoid
  , mkConstMethod "objectName" [] $ TObj c_QString
  , mkMethod "removeEventFilter" [TPtr $ TObj c_QObject] TVoid
  , mkConstMethod "signalsBlocked" [] TBool
  , mkMethod "startTimer" [TInt] TInt
  ] ++
  mkProps
  [ mkProp "parent" $ TPtr $ TObj c_QObject
  ]

signals =
  [ makeSignal c_QObject "destroyed" c_ListenerPtrQObject
  ]
