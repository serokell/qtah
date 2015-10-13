module Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractScrollArea (
  cppopModule,
  qtModule,
  c_QAbstractScrollArea,
  ) where

import Foreign.Cppop.Generator.Spec (
  Export (ExportClass),
  Type (TBitspace, TEnum, TObj, TPtr, TVoid),
  addReqIncludes,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkProp,
  mkProps,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (bs_Alignment, e_ScrollBarPolicy)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

cppopModule = makeCppopModule "Widgets" "QAbstractScrollArea" qtModule

qtModule =
  makeQtModule "Widgets.QAbstractScrollArea"
  [ QtExport $ ExportClass c_QAbstractScrollArea ]

c_QAbstractScrollArea =
  addReqIncludes [includeStd "QAbstractScrollArea"] $
  makeClass (ident "QAbstractScrollArea") Nothing [c_QWidget]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ] $
  [ mkMethod "addScrollBarWidget" [TPtr $ TObj c_QWidget, TBitspace bs_Alignment] TVoid
  , mkConstMethod "maximumViewportSize" [] $ TObj c_QSize
    -- TODO scrollBarWidgets
  ] ++
  mkProps
  [ mkProp "cornerWidget" $ TPtr $ TObj c_QWidget
    -- TODO horizontalScrollBar
  , mkProp "horizontalScrollBarPolicy" $ TEnum e_ScrollBarPolicy
    -- TODO verticalScrollBar
  , mkProp "verticalScrollBarPolicy" $ TEnum e_ScrollBarPolicy
  , mkProp "viewport" $ TPtr $ TObj c_QWidget
  ]
