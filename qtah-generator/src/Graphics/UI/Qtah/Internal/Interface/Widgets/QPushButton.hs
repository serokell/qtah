module Graphics.UI.Qtah.Internal.Interface.Widgets.QPushButton (
  cppopModule,
  qtModule,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractButton (c_QAbstractButton)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QMenu (c_QMenu)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

cppopModule = makeCppopModule "Widgets" "QPushButton" qtModule

qtModule =
  makeQtModule "Widgets.QPushButton"
  [ QtExport $ ExportClass c_QPushButton ]

this = c_QPushButton

c_QPushButton =
  addReqIncludes [includeStd "QPushButton"] $
  makeClass (ident "QPushButton") Nothing
  [ c_QAbstractButton ]
  [ mkCtor this "new" []
  , mkCtor this "newWithParent" [TPtr $ TObj c_QWidget]
  , mkCtor this "newWithText" [TObj c_QString]
  , mkCtor this "newWithTextAndParent" [TObj c_QString, TPtr $ TObj c_QWidget]
    -- TODO Ctors with QIcon.
  ] $
  [ mkMethod this "showMenu" [] TVoid
  ] ++
  mkProps
  [ mkProp this "autoDefault" TBool
  , mkBoolIsProp this "default"
  , mkBoolIsProp this "flat"
  , mkProp this "menu" $ TPtr $ TObj c_QMenu
  ]
