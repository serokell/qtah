{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Widgets.QBoxLayout (
  qtModule,
  c_QBoxLayout,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.Types (e_Alignment)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QLayout (c_QLayout)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)
#include "../Mk.hs.inc"

qtModule =
  makeQtModule "Widgets.QBoxLayout"
  [ QtExport $ ExportClass c_QBoxLayout
  , QtExport $ ExportEnum e_Direction
  ]

this = c_QBoxLayout

c_QBoxLayout =
  addReqIncludes [includeStd "QBoxLayout"] $
  makeClass (ident "QBoxLayout") Nothing [c_QLayout]
  [ _mkCtor "new" [TEnum e_Direction]
  , _mkCtor "newWithParent" [TEnum e_Direction, TPtr $ TObj c_QWidget]
  ] $
  [ _mkMethod' "addLayout" "addLayout" [TPtr $ TObj c_QLayout] TVoid
  , _mkMethod' "addLayout" "addLayoutWithStretch" [TPtr $ TObj c_QLayout, TInt] TVoid
  , _mkMethod "addSpacing" [TInt] TVoid
  , _mkMethod' "addStretch" "addStretch" [] TVoid
  , _mkMethod' "addStretch" "addStretchOf" [TInt] TVoid
  , _mkMethod "addStrut" [TInt] TVoid
  , _mkMethod' "addWidget" "addWidget" [TPtr $ TObj c_QWidget] TVoid
  , _mkMethod' "addWidget" "addWidgetWithStretch" [TPtr $ TObj c_QWidget, TInt] TVoid
  , _mkMethod' "addWidget" "addWidgetWithStretchAndAlignment"
    [TPtr $ TObj c_QWidget, TInt, TEnum e_Alignment] TVoid
  , _mkMethod' "insertLayout" "insertLayout" [TInt, TPtr $ TObj c_QLayout] TVoid
  , _mkMethod' "insertLayout" "insertLayoutWithStretch" [TInt, TPtr $ TObj c_QLayout, TInt] TVoid
    -- TODO insertSpacerItem
  , _mkMethod "insertSpacing" [TInt, TInt] TVoid
  , _mkMethod' "insertStretch" "insertStretch" [TInt] TVoid
  , _mkMethod' "insertStretch" "insertStretchOf" [TInt, TInt] TVoid
  , _mkMethod' "insertWidget" "insertWidget" [TInt, TPtr $ TObj c_QWidget] TVoid
  , _mkMethod' "insertWidget" "insertWidgetWithStretch" [TInt, TPtr $ TObj c_QWidget, TInt] TVoid
  , _mkMethod' "insertWidget" "insertWidgetWithStretchAndAlignment"
    [TInt, TPtr $ TObj c_QWidget, TInt, TEnum e_Alignment] TVoid
  , _mkMethod "setStretch" [TInt, TInt] TVoid
  , _mkMethod' "setStretchFactor" "setWidgetStretchFactor" [TPtr $ TObj c_QWidget, TInt] TBool
  , _mkMethod' "setStretchFactor" "setLayoutStretchFactor" [TPtr $ TObj c_QLayout, TInt] TBool
  ] ++
  _props
  [ _mkProp "direction" $ TEnum e_Direction
  , _mkProp "spacing" TInt
  ]

e_Direction =
  makeEnum (ident1 "QBoxLayout" "Direction") Nothing
  [ (0, ["left", "to", "right"])
  , (1, ["right", "to", "left"])
  , (2, ["top", "to", "bottom"])
  , (3, ["bottom", "to", "top"])
  ]
