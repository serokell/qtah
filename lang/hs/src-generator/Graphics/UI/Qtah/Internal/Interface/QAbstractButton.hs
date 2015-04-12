module Graphics.UI.Qtah.Internal.Interface.QAbstractButton where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Moc
import Graphics.UI.Qtah.Internal.Interface.Listener
import Graphics.UI.Qtah.Internal.Interface.QString
import Graphics.UI.Qtah.Internal.Interface.QWidget

c_QAbstractButton = qtClassClass qtc_QAbstractButton

qtc_QAbstractButton =
  makeQtClass (ident "QAbstractButton") Nothing
  [ c_QWidget ]
  []
  [ makeMethod "setText" (toExtName "QAbstractButton_setText") MNormal Nonpure
    [TObj c_QString] TVoid
  , makeMethod "text" (toExtName "QAbstractButton_text") MConst Nonpure
    [] $ TObj c_QString
  ]
  [ makeSignal "clicked" (toExtName "QAbstractButton_clicked")
    qtc_QAbstractButton c_ListenerBool
  ]
