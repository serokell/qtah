module Graphics.UI.Qtah.Internal.Interface.Widgets.QBoxLayout (
  cppopModule,
  qtModule,
  c_QBoxLayout,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.Types (e_Alignment)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QLayout (c_QLayout)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

cppopModule = makeCppopModule "Widgets" "QBoxLayout" qtModule

qtModule =
  makeQtModule "Widgets.QBoxLayout"
  [ QtExport $ ExportClass c_QBoxLayout
  , QtExport $ ExportEnum e_Direction
  ]

this = c_QBoxLayout

c_QBoxLayout =
  addReqIncludes [includeStd "QBoxLayout"] $
  makeClass (ident "QBoxLayout") Nothing [c_QLayout]
  [ mkCtor this "new" [TEnum e_Direction]
  , mkCtor this "newWithParent" [TEnum e_Direction, TPtr $ TObj c_QWidget]
  ] $
  [ mkMethod' this "addLayout" "addLayout" [TPtr $ TObj c_QLayout] TVoid
  , mkMethod' this "addLayout" "addLayoutWithStretch" [TPtr $ TObj c_QLayout, TInt] TVoid
  , mkMethod this "addSpacing" [TInt] TVoid
  , mkMethod' this "addStretch" "addStretch" [] TVoid
  , mkMethod' this "addStretch" "addStretchOf" [TInt] TVoid
  , mkMethod this "addStrut" [TInt] TVoid
  , mkMethod' this "addWidget" "addWidget" [TPtr $ TObj c_QWidget] TVoid
  , mkMethod' this "addWidget" "addWidgetWithStretch" [TPtr $ TObj c_QWidget, TInt] TVoid
  , mkMethod' this "addWidget" "addWidgetWithStretchAndAlignment"
    [TPtr $ TObj c_QWidget, TInt, TEnum e_Alignment] TVoid
  , mkMethod' this "insertLayout" "insertLayout" [TInt, TPtr $ TObj c_QLayout] TVoid
  , mkMethod' this "insertLayout" "insertLayoutWithStretch" [TInt, TPtr $ TObj c_QLayout, TInt] TVoid
    -- TODO insertSpacerItem
  , mkMethod this "insertSpacing" [TInt, TInt] TVoid
  , mkMethod' this "insertStretch" "insertStretch" [TInt] TVoid
  , mkMethod' this "insertStretch" "insertStretchOf" [TInt, TInt] TVoid
  , mkMethod' this "insertWidget" "insertWidget" [TInt, TPtr $ TObj c_QWidget] TVoid
  , mkMethod' this "insertWidget" "insertWidgetWithStretch" [TInt, TPtr $ TObj c_QWidget, TInt] TVoid
  , mkMethod' this "insertWidget" "insertWidgetWithStretchAndAlignment"
    [TInt, TPtr $ TObj c_QWidget, TInt, TEnum e_Alignment] TVoid
  , mkMethod this "setStretch" [TInt, TInt] TVoid
  , mkMethod' this "setStretchFactor" "setWidgetStretchFactor" [TPtr $ TObj c_QWidget, TInt] TBool
  , mkMethod' this "setStretchFactor" "setLayoutStretchFactor" [TPtr $ TObj c_QLayout, TInt] TBool
  ] ++
  mkProps
  [ mkProp this "direction" $ TEnum e_Direction
  , mkProp this "spacing" TInt
  ]

e_Direction =
  makeEnum (ident1 "QBoxLayout" "Direction") Nothing
  [ (0, ["left", "to", "right"])
  , (1, ["right", "to", "left"])
  , (2, ["top", "to", "bottom"])
  , (3, ["bottom", "to", "top"])
  ]
