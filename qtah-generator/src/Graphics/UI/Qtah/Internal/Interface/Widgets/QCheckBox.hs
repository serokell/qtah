module Graphics.UI.Qtah.Internal.Interface.Widgets.QCheckBox (
  cppopModule,
  qtModule,
  ) where

import Foreign.Cppop.Generator.Spec (
  Export (ExportClass),
  Type (TEnum, TObj, TPtr),
  addReqIncludes,
  ident,
  includeStd,
  makeClass,
  mkBoolIsProp,
  mkCtor,
  mkProp,
  mkProps,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (e_CheckState)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractButton (c_QAbstractButton)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

cppopModule = makeCppopModule "Widgets" "QCheckBox" qtModule

qtModule =
  makeQtModule "Widgets.QCheckBox"
  [ QtExport $ ExportClass c_QCheckBox ]

c_QCheckBox =
  addReqIncludes [includeStd "QCheckBox"] $
  makeClass (ident "QCheckBox") Nothing [ c_QAbstractButton ]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  , mkCtor "newWithText" [TObj c_QString]
  , mkCtor "newWithTextAndParent" [TObj c_QString, TPtr $ TObj c_QWidget]
  ] $
  mkProps
  [ mkProp "checkState" $ TEnum e_CheckState
  , mkBoolIsProp "tristate"
  ]
