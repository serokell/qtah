module Graphics.UI.Qtah.Internal.Interface.QObject where

import Foreign.Cppop.Generator.Spec

c_QObject =
  makeClass (ident "QObject") Nothing []
  []
  [ makeMethod "parent" (toExtName "QObject_parent") MConst Nonpure
    [] $ TPtr $ TObj c_QObject
  , makeMethod "setParent" (toExtName "QObject_setParent") MNormal Nonpure
    [TPtr $ TObj c_QObject] TVoid
  ]
