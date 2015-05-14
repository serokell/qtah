module Graphics.UI.Qtah.Internal.Interface.Widgets.QLayoutItem (
  qtModule,
  c_QLayoutItem,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

qtModule = makeQtModuleForClass c_QLayoutItem []

c_QLayoutItem =
  addReqIncludes [includeStd "QLayoutItem"] $
  makeClass (ident "QLayoutItem") Nothing []
  []
  []
