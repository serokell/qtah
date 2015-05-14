module Graphics.UI.Qtah.Internal.Interface.Core.QCoreApplication (
  qtModule,
  c_QCoreApplication,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QObject (c_QObject)

{-# ANN module "HLint: ignore Use camelCase" #-}

qtModule = makeQtModuleForClass c_QCoreApplication []

c_QCoreApplication =
  addReqIncludes [includeStd "QCoreApplication"] $
  makeClass (ident "QCoreApplication") Nothing [c_QObject] [] []
