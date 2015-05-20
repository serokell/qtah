{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Widgets.QLayoutItem (
  qtModule,
  c_QLayoutItem,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Internal.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (e_Alignment)
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Widgets.QLayout (c_QLayout)
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)
#include "../Mk.hs.inc"

qtModule =
  makeQtModule "Widgets.QLayoutItem"
  [ QtExport $ ExportClass c_QLayoutItem ]

this = c_QLayoutItem

c_QLayoutItem =
  addReqIncludes [includeStd "QLayoutItem"] $
  makeClass (ident "QLayoutItem") Nothing []
  [] $  -- Abstract.
  [ -- TODO controlTypes
    -- TODO expandingDirections
    _mkConstMethod "hasHeightForWidth" [] TBool
  , _mkConstMethod "heightForWidth" [TInt] TInt
  , _mkMethod "invalidate" [] TVoid
  , _mkConstMethod "isEmpty" [] TBool
  , _mkMethod "layout" [] $ TPtr $ TObj c_QLayout
  , _mkConstMethod "maximumSize" [] $ TObj c_QSize
  , _mkConstMethod "minimumHeightForWidth" [TInt] TInt
  , _mkConstMethod "minimumSize" [] $ TObj c_QSize
  , _mkConstMethod "sizeHint" [] $ TObj c_QSize
    -- TODO spacerItem
  , _mkConstMethod "widget" [] $ TPtr $ TObj c_QWidget
  ] ++
  _props
  [ _mkProp "alignment" $ TEnum e_Alignment
  , _mkProp "geometry" $ TObj c_QRect
  ]
