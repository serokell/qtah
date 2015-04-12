module Graphics.UI.Qtah.Internal.Interface.QObject where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Interface.QString

c_QObject =
  makeClass (ident "QObject") Nothing []
  []
  [ makeMethod "blockSignals" (toExtName "QObject_blockSignals") MNormal Nonpure
    [TBool] TBool
  , makeMethod "objectName" (toExtName "QObject_objectName") MConst Nonpure
    [] $ TObj c_QString
  , makeMethod "parent" (toExtName "QObject_parent") MConst Nonpure
    [] $ TPtr $ TObj c_QObject
  , makeMethod "setParent" (toExtName "QObject_setParent") MNormal Nonpure
    [TPtr $ TObj c_QObject] TVoid
  , makeMethod "signalsBlocked" (toExtName "QObject_signalsBlocked") MConst Nonpure
    [] TBool
  ]
