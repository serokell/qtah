{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Widgets.QHBoxLayout (
  cppopModule,
  qtModule,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Widgets.QBoxLayout (c_QBoxLayout)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)
#include "../Mk.hs.inc"

cppopModule = makeCppopModule "Widgets" "QHBoxLayout" qtModule

qtModule =
  makeQtModule "Widgets.QHBoxLayout"
  [ QtExport $ ExportClass c_QHBoxLayout ]

this = c_QHBoxLayout

c_QHBoxLayout =
  addReqIncludes [includeStd "QHBoxLayout"] $
  makeClass (ident "QHBoxLayout") Nothing [c_QBoxLayout]
  [ _mkCtor "new" []
  , _mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ]
  []
