module Graphics.UI.Qtah.Internal.Interface.Core.QCoreApplication (
  cppopModule,
  qtModule,
  c_QCoreApplication,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QObject (c_QObject)

{-# ANN module "HLint: ignore Use camelCase" #-}

cppopModule =
  modifyModule' (makeModule "qcoreapplication" "gen_qcoreapplication.hpp" "gen_qcoreapplication.cpp") $ do
    addModuleHaskellName ["Core", "QCoreApplication"]
    addModuleExports exports

qtModule = makeQtModule "Core.QCoreApplication" $ map QtExport exports

exports = [ExportClass c_QCoreApplication]

c_QCoreApplication =
  addReqIncludes [includeStd "QCoreApplication"] $
  makeClass (ident "QCoreApplication") Nothing [c_QObject] [] []
