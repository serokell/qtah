{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.QApplication (
  mod_QApplication,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.QCoreApplication

this = c_QApplication
thisQt = qtc_QApplication
#include "MkQt.hs.inc"

mod_QApplication =
  makeQtModule "QApplication"
  [ QtExportFn f_QApplication_new
  , QtExportClass thisQt
  ]

f_QApplication_new =
  makeFn (ident1 "qtah" "shim_QApplication_new") (toExtName "QApplication_new") Nonpure
  [] $ TPtr $ TObj c_QApplication

c_QApplication = qtClassClass qtc_QApplication

qtc_QApplication =
  makeQtClass (ident "QApplication") Nothing [c_QCoreApplication]
  [ {-Ctor (toExtName "QApplication_new") []-} ]
  [ _mkMethod "exec" [] TVoid ]
  []
