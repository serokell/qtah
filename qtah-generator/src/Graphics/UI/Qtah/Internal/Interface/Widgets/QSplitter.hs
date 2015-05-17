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

this = c_QSplitter

c_QSplitter =
  addReqIncludes [includeStd "QSplitter"] $
  makeClass (ident "QSplitter") Nothing [c_QFrame]
  [ mkCtor this "new" []
  , mkCtor this "newWithParent" [TPtr $ TObj c_QWidget]
  , mkCtor this "newWithOrientation" [TEnum e_Orientation]
  , mkCtor this "newWithOrientationAndParent" [TEnum e_Orientation, TPtr $ TObj c_QWidget]
  ] $
  [ mkMethod this "addWidget" [TPtr $ TObj c_QWidget] TVoid
  , mkConstMethod this "count" [] TInt
    -- TODO getRange
    -- TODO handle
  , mkConstMethod this "indexOf" [TPtr $ TObj c_QWidget] TInt
  , mkMethod this "insertWidget" [TInt, TPtr $ TObj c_QWidget] TVoid
  , mkConstMethod this "isCollapsible" [TInt] TBool
  , mkMethod this "refresh" [] TVoid
    -- TODO restoreState
    -- TODO saveState
  , mkMethod this "setCollapsible" [TInt, TBool] TVoid
    -- TODO setSizes
  , mkMethod this "setStretchFactor" [TInt, TInt] TVoid
    -- TODO sizes
  , mkConstMethod this "widget" [TInt] $ TPtr $ TObj c_QWidget
  ] ++
  mkProps
  [ mkProp this "childrenCollapsible" TBool
  , mkProp this "handleWidth" TInt
  , mkProp this "opaqueResize" TBool
  , mkProp this "orientation" $ TEnum e_Orientation
  ]

signals =
  [ makeSignal this "splitterMoved" c_ListenerIntInt ]
