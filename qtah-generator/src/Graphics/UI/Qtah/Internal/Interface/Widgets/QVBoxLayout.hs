{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Widgets.QVBoxLayout (
  qtModule,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Widgets.QBoxLayout (c_QBoxLayout)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)
#include "../Mk.hs.inc"

this = c_QVBoxLayout

qtModule =
  makeQtModule "Widgets.QVBoxLayout"
  [ QtExport $ ExportClass c_QVBoxLayout ]

c_QVBoxLayout =
  addReqIncludes [includeStd "QVBoxLayout"] $
  makeClass (ident "QVBoxLayout") Nothing [c_QBoxLayout]
  [ _mkCtor "new" []
  , _mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ]
  []
