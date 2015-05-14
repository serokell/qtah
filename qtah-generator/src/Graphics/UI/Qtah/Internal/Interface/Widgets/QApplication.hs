{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Widgets.QApplication (
  qtModule,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QCoreApplication (c_QCoreApplication)

{-# ANN module "HLint: ignore Use camelCase" #-}

qtModule =
  makeQtModule "QApplication"
  [ QtExport $ ExportFn f_QApplication_new
  , QtExport $ ExportClass c_QApplication
  ]

this = c_QApplication
#include "../Mk.hs.inc"

f_QApplication_new =
  addReqIncludes [includeLocal "shim_qapplication.hpp"] $
  makeFn (ident1 "qtah" "shim_QApplication_new") (Just $ toExtName "QApplication_new") Nonpure
  [] $ TPtr $ TObj c_QApplication

c_QApplication =
  addReqIncludes [includeStd "QApplication"] $
  makeClass (ident "QApplication") Nothing [c_QCoreApplication]
  [ {-Ctor (toExtName "QApplication_new") []-} ]
  [ _mkMethod "exec" [] TVoid ]
