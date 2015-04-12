module Graphics.UI.Qtah.Internal.Interface.QObject where

import Foreign.Cppop.Generator.Spec
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Listener
import Graphics.UI.Qtah.Internal.Interface.QString
import Graphics.UI.Qtah.Internal.Generator.Moc

c_QObject = qtClassClass qtc_QObject

qtc_QObject =
  makeQtClass (ident "QObject") Nothing []
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
  [ makeSignal "destroyed" (toExtName "QObject_destroyed")
    qtc_QObject c_ListenerPtrQObject
  ]
