{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractScrollArea (
  qtModule,
  c_QAbstractScrollArea,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (e_Alignment, e_ScrollBarPolicy)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)
#include "../Mk.hs.inc"

qtModule =
  makeQtModule "Widgets.QAbstractScrollArea"
  [ QtExport $ ExportClass c_QAbstractScrollArea ]

this = c_QAbstractScrollArea

c_QAbstractScrollArea =
  addReqIncludes [includeStd "QAbstractScrollArea"] $
  makeClass (ident "QAbstractScrollArea") Nothing [c_QWidget]
  [ _mkCtor "new" []
  , _mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ] $
  [ _mkMethod "addScrollBarWidget" [TPtr $ TObj c_QWidget, TEnum e_Alignment] TVoid
  , _mkConstMethod "maximumViewportSize" [] $ TObj c_QSize
    -- TODO scrollBarWidgets
  ] ++
  _props
  [ _mkProp "cornerWidget" $ TPtr $ TObj c_QWidget
    -- TODO horizontalScrollBar
  , _mkProp "horizontalScrollBarPolicy" $ TEnum e_ScrollBarPolicy
    -- TODO verticalScrollBar
  , _mkProp "verticalScrollBarPolicy" $ TEnum e_ScrollBarPolicy
  , _mkProp "viewport" $ TPtr $ TObj c_QWidget
  ]
