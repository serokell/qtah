module Graphics.UI.Qtah.Internal.Interface.QWidget where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Interface.QObject
import Graphics.UI.Qtah.Internal.Interface.QString

c_QWidget =
  makeClass (ident "QWidget") Nothing
  [ c_QObject ]
  [ makeCtor (toExtName "QWidget_new") [TPtr $ TObj c_QWidget] ]
  [ makeMethod "resize" (toExtName "QWidget_resize") MNormal Nonpure
    [TInt, TInt] TVoid
  , makeMethod "setWindowTitle" (toExtName "QWidget_setWindowTitle") MNormal Nonpure
    [TObj c_QString] TVoid
  , makeMethod "show" (toExtName "QWidget_show") MNormal Nonpure
    [] TVoid
  ]
