module Graphics.UI.Qtah.Internal.Interface.Widgets.QPushButton (
  hoppyModule,
  qtModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Type (TBool, TObj, TPtr, TVoid),
  addReqIncludes,
  ident,
  includeStd,
  makeClass,
  mkBoolIsProp,
  mkCtor,
  mkMethod,
  mkProp,
  mkProps,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractButton (c_QAbstractButton)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QMenu (c_QMenu)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

hoppyModule = makeHoppyModule "Widgets" "QPushButton" qtModule

qtModule =
  makeQtModule "Widgets.QPushButton"
  [ QtExport $ ExportClass c_QPushButton ]

c_QPushButton =
  addReqIncludes [includeStd "QPushButton"] $
  makeClass (ident "QPushButton") Nothing
  [ c_QAbstractButton ]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  , mkCtor "newWithText" [TObj c_QString]
  , mkCtor "newWithTextAndParent" [TObj c_QString, TPtr $ TObj c_QWidget]
    -- TODO Ctors with QIcon.
  ] $
  [ mkMethod "showMenu" [] TVoid
  ] ++
  mkProps
  [ mkProp "autoDefault" TBool
  , mkBoolIsProp "default"
  , mkBoolIsProp "flat"
  , mkProp "menu" $ TPtr $ TObj c_QMenu
  ]
