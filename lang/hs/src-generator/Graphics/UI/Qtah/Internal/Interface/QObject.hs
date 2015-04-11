module Graphics.UI.Qtah.Internal.Interface.QObject where

import Foreign.Cppop.Generator.Spec

c_QObject =
  makeClass (ident "QObject") Nothing []
  []
  [ Method "parent" (toExtName "QObject_parent") MConst Nonpure
    [] $ TPtr $ TObj c_QObject
  , Method "setParent" (toExtName "QObject_setParent") MNormal Nonpure
    [TPtr $ TObj c_QObject] TVoid
  ]
