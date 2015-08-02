module Graphics.UI.Qtah.Internal.Interface.Widgets.QSplitter (
  cppopModule,
  qtModule,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.Types (e_Orientation)
import Graphics.UI.Qtah.Internal.Interface.Listener (c_ListenerIntInt)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QFrame (c_QFrame)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

cppopModule = makeCppopModule "Widgets" "QSplitter" qtModule

qtModule =
  makeQtModule "Widgets.QSplitter" $
  [ QtExport $ ExportClass c_QSplitter
  ] ++ map QtExportSignal signals

c_QSplitter =
  addReqIncludes [includeStd "QSplitter"] $
  makeClass (ident "QSplitter") Nothing [c_QFrame]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  , mkCtor "newWithOrientation" [TEnum e_Orientation]
  , mkCtor "newWithOrientationAndParent" [TEnum e_Orientation, TPtr $ TObj c_QWidget]
  ] $
  [ mkMethod "addWidget" [TPtr $ TObj c_QWidget] TVoid
  , mkConstMethod "count" [] TInt
    -- TODO getRange
    -- TODO handle
  , mkConstMethod "indexOf" [TPtr $ TObj c_QWidget] TInt
  , mkMethod "insertWidget" [TInt, TPtr $ TObj c_QWidget] TVoid
  , mkConstMethod "isCollapsible" [TInt] TBool
  , mkMethod "refresh" [] TVoid
    -- TODO restoreState
    -- TODO saveState
  , mkMethod "setCollapsible" [TInt, TBool] TVoid
    -- TODO setSizes
  , mkMethod "setStretchFactor" [TInt, TInt] TVoid
    -- TODO sizes
  , mkConstMethod "widget" [TInt] $ TPtr $ TObj c_QWidget
  ] ++
  mkProps
  [ mkProp "childrenCollapsible" TBool
  , mkProp "handleWidth" TInt
  , mkProp "opaqueResize" TBool
  , mkProp "orientation" $ TEnum e_Orientation
  ]

signals =
  [ makeSignal c_QSplitter "splitterMoved" c_ListenerIntInt ]
