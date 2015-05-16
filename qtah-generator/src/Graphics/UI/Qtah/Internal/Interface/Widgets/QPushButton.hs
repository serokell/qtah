{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Widgets.QPushButton (
  qtModule,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractButton (c_QAbstractButton)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QMenu (c_QMenu)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)
#include "../Mk.hs.inc"

qtModule = makeQtModuleForClass c_QPushButton []

this = c_QPushButton

c_QPushButton =
  addReqIncludes [includeStd "QPushButton"] $
  makeClass (ident "QPushButton") Nothing
  [ c_QAbstractButton ]
  [ _mkCtor "new" []
  , _mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  , _mkCtor "newWithText" [TObj c_QString]
  , _mkCtor "newWithTextAndParent" [TObj c_QString, TPtr $ TObj c_QWidget]
    -- TODO Ctors with QIcon.
  ] $
  [ _mkMethod "showMenu" [] TVoid
  ] ++
  _props
  [ _mkProp "autoDefault" TBool
  , _mkBoolIsProp "default"
  , _mkBoolIsProp "flat"
  , _mkProp "menu" $ TPtr $ TObj c_QMenu
  ]
