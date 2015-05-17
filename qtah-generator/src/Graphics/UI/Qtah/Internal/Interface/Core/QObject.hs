module Graphics.UI.Qtah.Internal.Interface.Core.QObject (
  cppopModule,
  qtModule,
  c_QObject,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Listener
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)

{-# ANN module "HLint: ignore Use camelCase" #-}

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
  [ mkMethod this "blockSignals" [TBool] TBool
  , mkMethod this "dumpObjectInfo" [] TVoid
  , mkMethod this "dumpObjectTree" [] TVoid
  , mkMethod this "installEventFilter" [TPtr $ TObj c_QObject] TVoid
  , mkConstMethod this "isWidgetType" [] TBool
  , mkMethod this "killTimer" [TInt] TVoid
  , mkConstMethod this "objectName" [] $ TObj c_QString
  , mkMethod this "removeEventFilter" [TPtr $ TObj c_QObject] TVoid
  , mkConstMethod this "signalsBlocked" [] TBool
  , mkMethod this "startTimer" [TInt] TInt
  ] ++
  mkProps
  [ mkProp this "parent" $ TPtr $ TObj c_QObject
  ]

signals =
  [ makeSignal this "destroyed" c_ListenerPtrQObject
  ]
