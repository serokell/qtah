module Graphics.UI.Qtah.Internal.Interface.Widgets.QHBoxLayout (
  cppopModule,
  qtModule,
  ) where

import Foreign.Cppop.Generator.Spec (
  Export (ExportClass),
  Type (TObj, TPtr),
  addReqIncludes,
  ident,
  includeStd,
  makeClass,
  mkCtor,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Widgets.QBoxLayout (c_QBoxLayout)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

cppopModule = makeCppopModule "Widgets" "QHBoxLayout" qtModule

qtModule =
  makeQtModule "Widgets.QHBoxLayout"
  [ QtExport $ ExportClass c_QHBoxLayout ]

c_QHBoxLayout =
  addReqIncludes [includeStd "QHBoxLayout"] $
  makeClass (ident "QHBoxLayout") Nothing [c_QBoxLayout]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ]
  []
