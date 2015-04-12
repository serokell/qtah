module Graphics.UI.Qtah.Internal.Interface.QApplication where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Interface.QCoreApplication

f_QApplication_new =
  makeFn (ident1 "qtah" "shim_QApplication_new") (toExtName "QApplication_new") Nonpure
  [] $ TPtr $ TObj c_QApplication

c_QApplication =
  makeClass (ident "QApplication") Nothing
  [ c_QCoreApplication ]
  [ {-Ctor (toExtName "QApplication_new") []-} ]
  [ makeMethod "exec" (toExtName "QApplication_exec") MNormal Nonpure [] TVoid ]
