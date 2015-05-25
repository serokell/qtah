module Graphics.UI.Qtah.Internal.Interface.Core.QCoreApplication (
  cppopModule,
  qtModule,
  c_QCoreApplication,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QObject (c_QObject)

{-# ANN module "HLint: ignore Use camelCase" #-}

cppopModule = makeCppopModule "Core" "QCoreApplication" qtModule

qtModule =
  makeQtModule "Core.QCoreApplication"
  [ QtExport $ ExportClass c_QCoreApplication ]

c_QCoreApplication =
  addReqIncludes [includeStd "QCoreApplication"] $
  makeClass (ident "QCoreApplication") Nothing [c_QObject] [] []
