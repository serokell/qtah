{-# LANGUAGE CPP #-}

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
#include "../Mk.hs.inc"

cppopModule = makeCppopModule "Widgets" "QSplitter" qtModule

qtModule =
  makeQtModule "Widgets.QSplitter" $
  [ QtExport $ ExportClass c_QSplitter
  ] ++ map QtExportSignal signals

this = c_QSplitter

c_QSplitter =
  addReqIncludes [includeStd "QSplitter"] $
  makeClass (ident "QSplitter") Nothing [c_QFrame]
  [ _mkCtor "new" []
  , _mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  , _mkCtor "newWithOrientation" [TEnum e_Orientation]
  , _mkCtor "newWithOrientationAndParent" [TEnum e_Orientation, TPtr $ TObj c_QWidget]
  ] $
  [ _mkMethod "addWidget" [TPtr $ TObj c_QWidget] TVoid
  , _mkConstMethod "count" [] TInt
    -- TODO getRange
    -- TODO handle
  , _mkConstMethod "indexOf" [TPtr $ TObj c_QWidget] TInt
  , _mkMethod "insertWidget" [TInt, TPtr $ TObj c_QWidget] TVoid
  , _mkConstMethod "isCollapsible" [TInt] TBool
  , _mkMethod "refresh" [] TVoid
    -- TODO restoreState
    -- TODO saveState
  , _mkMethod "setCollapsible" [TInt, TBool] TVoid
    -- TODO setSizes
  , _mkMethod "setStretchFactor" [TInt, TInt] TVoid
    -- TODO sizes
  , _mkConstMethod "widget" [TInt] $ TPtr $ TObj c_QWidget
  ] ++
  _props
  [ _mkProp "childrenCollapsible" TBool
  , _mkProp "handleWidth" TInt
  , _mkProp "opaqueResize" TBool
  , _mkProp "orientation" $ TEnum e_Orientation
  ]

signals =
  [ _mkSignal "splitterMoved" c_ListenerIntInt ]
