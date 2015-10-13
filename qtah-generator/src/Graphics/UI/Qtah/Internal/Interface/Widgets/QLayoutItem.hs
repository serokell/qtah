module Graphics.UI.Qtah.Internal.Interface.Widgets.QLayoutItem (
  cppopModule,
  qtModule,
  c_QLayoutItem,
  ) where

import Foreign.Cppop.Generator.Spec (
  Export (ExportClass),
  Type (TBitspace, TBool, TInt, TObj, TPtr, TVoid),
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
import Graphics.UI.Qtah.Internal.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Internal.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (bs_Alignment)
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Widgets.QLayout (c_QLayout)
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

cppopModule = makeCppopModule "Widgets" "QLayoutItem" qtModule

qtModule =
  makeQtModule "Widgets.QLayoutItem"
  [ QtExport $ ExportClass c_QLayoutItem ]

c_QLayoutItem =
  addReqIncludes [includeStd "QLayoutItem"] $
  makeClass (ident "QLayoutItem") Nothing []
  [] $  -- Abstract.
  [ -- TODO controlTypes
    -- TODO expandingDirections
    mkConstMethod "hasHeightForWidth" [] TBool
  , mkConstMethod "heightForWidth" [TInt] TInt
  , mkMethod "invalidate" [] TVoid
  , mkConstMethod "isEmpty" [] TBool
  , mkMethod "layout" [] $ TPtr $ TObj c_QLayout
  , mkConstMethod "maximumSize" [] $ TObj c_QSize
  , mkConstMethod "minimumHeightForWidth" [TInt] TInt
  , mkConstMethod "minimumSize" [] $ TObj c_QSize
  , mkConstMethod "sizeHint" [] $ TObj c_QSize
    -- TODO spacerItem
  , mkConstMethod "widget" [] $ TPtr $ TObj c_QWidget
  ] ++
  mkProps
  [ mkProp "alignment" $ TBitspace bs_Alignment
  , mkProp "geometry" $ TObj c_QRect
  ]
