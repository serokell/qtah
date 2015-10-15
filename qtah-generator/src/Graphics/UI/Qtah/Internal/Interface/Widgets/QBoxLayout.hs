module Graphics.UI.Qtah.Internal.Interface.Widgets.QBoxLayout (
  hoppyModule,
  qtModule,
  c_QBoxLayout,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportEnum, ExportClass),
  Type (TBitspace, TBool, TEnum, TInt, TObj, TPtr, TVoid),
  addReqIncludes,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  mkProps,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.Types (bs_Alignment)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QLayout (c_QLayout)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

hoppyModule = makeHoppyModule "Widgets" "QBoxLayout" qtModule

qtModule =
  makeQtModule "Widgets.QBoxLayout"
  [ QtExport $ ExportClass c_QBoxLayout
  , QtExport $ ExportEnum e_Direction
  ]

c_QBoxLayout =
  addReqIncludes [includeStd "QBoxLayout"] $
  makeClass (ident "QBoxLayout") Nothing [c_QLayout]
  [ mkCtor "new" [TEnum e_Direction]
  , mkCtor "newWithParent" [TEnum e_Direction, TPtr $ TObj c_QWidget]
  ] $
  [ mkMethod' "addLayout" "addLayout" [TPtr $ TObj c_QLayout] TVoid
  , mkMethod' "addLayout" "addLayoutWithStretch" [TPtr $ TObj c_QLayout, TInt] TVoid
  , mkMethod "addSpacing" [TInt] TVoid
  , mkMethod' "addStretch" "addStretch" [] TVoid
  , mkMethod' "addStretch" "addStretchOf" [TInt] TVoid
  , mkMethod "addStrut" [TInt] TVoid
  , mkMethod' "addWidget" "addWidget" [TPtr $ TObj c_QWidget] TVoid
  , mkMethod' "addWidget" "addWidgetWithStretch" [TPtr $ TObj c_QWidget, TInt] TVoid
  , mkMethod' "addWidget" "addWidgetWithStretchAndAlignment"
    [TPtr $ TObj c_QWidget, TInt, TBitspace bs_Alignment] TVoid
  , mkMethod' "insertLayout" "insertLayout" [TInt, TPtr $ TObj c_QLayout] TVoid
  , mkMethod' "insertLayout" "insertLayoutWithStretch" [TInt, TPtr $ TObj c_QLayout, TInt] TVoid
    -- TODO insertSpacerItem
  , mkMethod "insertSpacing" [TInt, TInt] TVoid
  , mkMethod' "insertStretch" "insertStretch" [TInt] TVoid
  , mkMethod' "insertStretch" "insertStretchOf" [TInt, TInt] TVoid
  , mkMethod' "insertWidget" "insertWidget" [TInt, TPtr $ TObj c_QWidget] TVoid
  , mkMethod' "insertWidget" "insertWidgetWithStretch" [TInt, TPtr $ TObj c_QWidget, TInt] TVoid
  , mkMethod' "insertWidget" "insertWidgetWithStretchAndAlignment"
    [TInt, TPtr $ TObj c_QWidget, TInt, TBitspace bs_Alignment] TVoid
  , mkMethod "setStretch" [TInt, TInt] TVoid
  , mkMethod' "setStretchFactor" "setWidgetStretchFactor" [TPtr $ TObj c_QWidget, TInt] TBool
  , mkMethod' "setStretchFactor" "setLayoutStretchFactor" [TPtr $ TObj c_QLayout, TInt] TBool
  ] ++
  mkProps
  [ mkProp "direction" $ TEnum e_Direction
  , mkProp "spacing" TInt
  ]

e_Direction =
  makeQtEnum (ident1 "QBoxLayout" "Direction")
  [ (0, ["left", "to", "right"])
  , (1, ["right", "to", "left"])
  , (2, ["top", "to", "bottom"])
  , (3, ["bottom", "to", "top"])
  ]
