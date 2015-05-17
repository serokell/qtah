module Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractScrollArea (
  cppopModule,
  qtModule,
  c_QAbstractScrollArea,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (e_Alignment, e_ScrollBarPolicy)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

cppopModule = makeCppopModule "Widgets" "QAbstractScrollArea" qtModule

qtModule =
  makeQtModule "Widgets.QAbstractScrollArea"
  [ QtExport $ ExportClass c_QAbstractScrollArea ]

this = c_QAbstractScrollArea

c_QAbstractScrollArea =
  addReqIncludes [includeStd "QAbstractScrollArea"] $
  makeClass (ident "QAbstractScrollArea") Nothing [c_QWidget]
  [ mkCtor this "new" []
  , mkCtor this "newWithParent" [TPtr $ TObj c_QWidget]
  ] $
  [ mkMethod this "addScrollBarWidget" [TPtr $ TObj c_QWidget, TEnum e_Alignment] TVoid
  , mkConstMethod this "maximumViewportSize" [] $ TObj c_QSize
    -- TODO scrollBarWidgets
  ] ++
  mkProps
  [ mkProp this "cornerWidget" $ TPtr $ TObj c_QWidget
    -- TODO horizontalScrollBar
  , mkProp this "horizontalScrollBarPolicy" $ TEnum e_ScrollBarPolicy
    -- TODO verticalScrollBar
  , mkProp this "verticalScrollBarPolicy" $ TEnum e_ScrollBarPolicy
  , mkProp this "viewport" $ TPtr $ TObj c_QWidget
  ]
