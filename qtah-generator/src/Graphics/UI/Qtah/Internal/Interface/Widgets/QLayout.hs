module Graphics.UI.Qtah.Internal.Interface.Widgets.QLayout (
  cppopModule,
  qtModule,
  c_QLayout,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QMargins (c_QMargins)
import Graphics.UI.Qtah.Internal.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Internal.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Internal.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (e_Alignment)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QLayoutItem (c_QLayoutItem)
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

cppopModule = makeCppopModule "Widgets" "QLayout" qtModule

qtModule =
  makeQtModule "Widgets.QLayout"
  [ QtExport $ ExportClass c_QLayout
  , QtExport $ ExportEnum e_SizeConstraint
  ]

this = c_QLayout

c_QLayout =
  addReqIncludes [includeStd "QLayout"] $
  makeClass (ident "QLayout") Nothing [c_QObject, c_QLayoutItem]
  [] $  -- Abstract.
  [ mkMethod this "activate" [] TBool
  , mkMethod this "addItem" [TPtr $ TObj c_QLayoutItem] TVoid
  , mkMethod this "addWidget" [TPtr $ TObj c_QWidget] TVoid
  , mkStaticMethod this "closestAcceptableSize" [TPtr $ TObj c_QWidget, TObj c_QSize] $ TObj c_QSize
  , mkConstMethod this "contentsMargins" [] $ TObj c_QMargins
  , mkConstMethod this "contentsRect" [] $ TObj c_QRect
  , mkConstMethod this "count" [] TInt
  , mkConstMethod this "indexOf" [TPtr $ TObj c_QWidget] TInt
  , mkConstMethod this "itemAt" [TInt] $ TPtr $ TObj c_QLayoutItem
  , mkConstMethod this "parentWidget" [] $ TPtr $ TObj c_QWidget
  , mkMethod this "removeItem" [TPtr $ TObj c_QLayoutItem] TVoid
  , mkMethod this "removeWidget" [TPtr $ TObj c_QWidget] TVoid
  , mkMethod' this "setAlignment" "setAlignment" [TEnum e_Alignment] TVoid
  , mkMethod' this "setAlignment" "setLayoutAlignment" [TPtr $ TObj c_QLayout, TEnum e_Alignment] TBool
  , mkMethod' this "setAlignment" "setWidgetAlignment" [TPtr $ TObj c_QWidget, TEnum e_Alignment] TBool
  , mkMethod this "setContentsMargins" [TObj c_QMargins] TVoid
  , mkMethod this "takeAt" [TInt] $ TPtr $ TObj c_QLayoutItem
  , mkMethod this "update" [] TVoid
  ] ++
  mkProps
  [ mkBoolIsProp this "enabled"
  , mkProp this "menuBar" $ TPtr $ TObj c_QWidget
  , mkProp this "sizeConstraint" $ TEnum e_SizeConstraint
  , mkProp this "spacing" TInt
  ]

e_SizeConstraint =
  makeQtEnum (ident1 "QLayout" "SizeConstraint")
  [ (0, ["set", "default", "size", "constraint"])
  , (1, ["set", "no", "constraint"])
  , (2, ["set", "minimum", "size"])
  , (3, ["set", "fixed", "size"])
  , (4, ["set", "maximum", "size"])
  , (5, ["set", "min", "and", "max", "size"])
  ]
