module Graphics.UI.Qtah.Internal.Interface.Widgets.QBoxLayout (
  qtModule,
  c_QBoxLayout,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Widgets.QLayout (c_QLayout)

{-# ANN module "HLint: ignore Use camelCase" #-}

qtModule = makeQtModuleForClass c_QBoxLayout []

c_QBoxLayout =
  addReqIncludes [includeStd "QBoxLayout"] $
  makeClass (ident "QBoxLayout") Nothing [c_QLayout]
  []
  []
