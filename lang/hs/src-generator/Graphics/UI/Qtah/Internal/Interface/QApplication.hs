{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.QApplication (
  f_QApplication_new,
  c_QApplication,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Interface.QCoreApplication

this = c_QApplication
#include "Mk.hs.inc"

f_QApplication_new =
  makeFn (ident1 "qtah" "shim_QApplication_new") (toExtName "QApplication_new") Nonpure
  [] $ TPtr $ TObj c_QApplication

c_QApplication =
  makeClass (ident "QApplication") Nothing
  [ c_QCoreApplication ]
  [ {-Ctor (toExtName "QApplication_new") []-} ]
  [ _mkMethod "exec" [] TVoid ]
