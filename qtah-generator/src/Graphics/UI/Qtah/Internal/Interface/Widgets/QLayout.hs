{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Widgets.QLayout (
  qtModule,
  c_QLayout,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QMargins (c_QMargins)
import Graphics.UI.Qtah.Internal.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Internal.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Internal.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Internal.Interface.Qt (e_Alignment)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QLayoutItem (c_QLayoutItem)
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

qtModule =
  makeQtModuleForClass c_QLayout
  [ QtExport $ ExportEnum e_SizeConstraint
  ]

this = c_QLayout
#include "../Mk.hs.inc"

c_QLayout =
  addReqIncludes [includeStd "QLayout"] $
  makeClass (ident "QLayout") Nothing [c_QObject, c_QLayoutItem]
  []
  [ _mkMethod "activate" [] TBool
  , _mkMethod "addItem" [TPtr $ TObj c_QLayoutItem] TVoid
  , _mkMethod "addWidget" [TPtr $ TObj c_QWidget] TVoid
  , _mkStaticMethod "closestAcceptableSize" [TPtr $ TObj c_QWidget, TObj c_QSize] $ TObj c_QSize
  , _mkConstMethod "contentsMargins" [] $ TObj c_QMargins
  , _mkConstMethod "contentsRect" [] $ TObj c_QRect
  , _mkConstMethod "count" [] TInt
  , _mkConstMethod "indexOf" [TPtr $ TObj c_QWidget] TInt
  , _mkConstMethod "isEnabled" [] TBool
  , _mkConstMethod "itemAt" [TInt] $ TPtr $ TObj c_QLayoutItem
  , _mkConstMethod "menuBar" [] $ TPtr $ TObj c_QWidget
  , _mkConstMethod "parentWidget" [] $ TPtr $ TObj c_QWidget
  , _mkMethod "removeItem" [TPtr $ TObj c_QLayoutItem] TVoid
  , _mkMethod "removeWidget" [TPtr $ TObj c_QWidget] TVoid
  , _mkMethod' "setAlignment" "setAlignment" [TEnum e_Alignment] TVoid
  , _mkMethod' "setAlignment" "setLayoutAlignment" [TPtr $ TObj c_QLayout, TEnum e_Alignment] TBool
  , _mkMethod' "setAlignment" "setWidgetAlignment" [TPtr $ TObj c_QWidget, TEnum e_Alignment] TBool
  , _mkMethod "setContentsMargins" [TObj c_QMargins] TVoid
  , _mkMethod "setEnabled" [TBool] TVoid
  , _mkMethod "setMenuBar" [TPtr $ TObj c_QWidget] TVoid
  , _mkMethod "setSizeConstraint" [TEnum e_SizeConstraint] TVoid
  , _mkMethod "setSpacing" [TInt] TVoid
  , _mkConstMethod "sizeConstraint" [] $ TEnum e_SizeConstraint
  , _mkConstMethod "spacing" [] TInt
  , _mkMethod "takeAt" [TInt] $ TPtr $ TObj c_QLayoutItem
  , _mkMethod "update" [] TVoid
  ]

e_SizeConstraint =
  makeEnum (ident1 "QLayout" "SizeConstraint") Nothing
  [ (0, ["set", "default", "size", "constraint"])
  , (1, ["set", "no", "constraint"])
  , (2, ["set", "minimum", "size"])
  , (3, ["set", "fixed", "size"])
  , (4, ["set", "maximum", "size"])
  , (5, ["set", "min", "and", "max", "size"])
  ]
