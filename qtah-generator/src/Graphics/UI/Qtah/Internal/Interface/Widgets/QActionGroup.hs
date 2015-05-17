module Graphics.UI.Qtah.Internal.Interface.Widgets.QActionGroup (
  cppopModule,
  qtModule,
  c_QActionGroup,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Listener (c_ListenerPtrQAction)
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Widgets.QAction (c_QAction)

{-# ANN module "HLint: ignore Use camelCase" #-}

cppopModule = makeCppopModule "Widgets" "QActionGroup" qtModule

qtModule =
  makeQtModule "Widgets.QActionGroup" $
  QtExport (ExportClass c_QActionGroup) :
  map QtExportSignal signals

this = c_QActionGroup

c_QActionGroup =
  addReqIncludes [includeStd "QActionGroup"] $
  makeClass (ident "QActionGroup") Nothing
  [ c_QObject ]
  [ mkCtor this "new" [TPtr $ TObj c_QObject]
  ] $
  [ -- TODO actions
    mkMethod' this "addAction" "addAction" [TPtr $ TObj c_QAction] $ TPtr $ TObj c_QAction
  , mkMethod' this "addAction" "addNewAction" [TObj c_QString] $ TPtr $ TObj c_QAction
    -- TODO addNewActionWithIcon
  , mkConstMethod this "checkedAction" [] $ TPtr $ TObj c_QAction
  , mkMethod this "removeAction" [TPtr $ TObj c_QAction] TVoid
  , mkMethod this "setDisabled" [TBool] TVoid
  ] ++
  mkProps
  [ mkBoolIsProp this "enabled"
  , mkBoolIsProp this "exclusive"
  , mkBoolIsProp this "visible"
  ]

signals =
  [ makeSignal this "hovered" c_ListenerPtrQAction
  , makeSignal this "triggered" c_ListenerPtrQAction
  ]
