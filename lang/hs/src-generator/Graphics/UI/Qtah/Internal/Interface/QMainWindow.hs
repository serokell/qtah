module Graphics.UI.Qtah.Internal.Interface.QMainWindow where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Interface.QWidget

c_QMainWindow =
  makeClass (ident "QMainWindow") Nothing
  [ c_QWidget ]
  [ makeCtor (toExtName "QMainWindow_new") [TPtr $ TObj c_QWidget] ]
  [ makeMethod "setCentralWidget" (toExtName "QMainWindow_setCentralWidget") MNormal Nonpure
    [TPtr $ TObj c_QWidget] TVoid
  ]
