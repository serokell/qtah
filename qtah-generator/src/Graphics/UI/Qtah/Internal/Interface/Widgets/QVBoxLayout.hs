module Graphics.UI.Qtah.Internal.Interface.Widgets.QVBoxLayout (
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

cppopModule = makeCppopModule "Widgets" "QVBoxLayout" qtModule

qtModule =
  makeQtModule "Widgets.QVBoxLayout"
  [ QtExport $ ExportClass c_QVBoxLayout ]

c_QVBoxLayout =
  addReqIncludes [includeStd "QVBoxLayout"] $
  makeClass (ident "QVBoxLayout") Nothing [c_QBoxLayout]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ]
  []
