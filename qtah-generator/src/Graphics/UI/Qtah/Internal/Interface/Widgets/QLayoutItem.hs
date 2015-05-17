module Graphics.UI.Qtah.Internal.Interface.Widgets.QLayoutItem (
  cppopModule,
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

{-# ANN module "HLint: ignore Use camelCase" #-}

cppopModule = makeCppopModule "Widgets" "QLayoutItem" qtModule

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
    mkConstMethod this "hasHeightForWidth" [] TBool
  , mkConstMethod this "heightForWidth" [TInt] TInt
  , mkMethod this "invalidate" [] TVoid
  , mkConstMethod this "isEmpty" [] TBool
  , mkMethod this "layout" [] $ TPtr $ TObj c_QLayout
  , mkConstMethod this "maximumSize" [] $ TObj c_QSize
  , mkConstMethod this "minimumHeightForWidth" [TInt] TInt
  , mkConstMethod this "minimumSize" [] $ TObj c_QSize
  , mkConstMethod this "sizeHint" [] $ TObj c_QSize
    -- TODO spacerItem
  , mkConstMethod this "widget" [] $ TPtr $ TObj c_QWidget
  ] ++
  mkProps
  [ mkProp this "alignment" $ TEnum e_Alignment
  , mkProp this "geometry" $ TObj c_QRect
  ]
