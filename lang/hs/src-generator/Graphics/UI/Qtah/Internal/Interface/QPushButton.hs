module Graphics.UI.Qtah.Internal.Interface.QPushButton where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Interface.QAbstractButton
import Graphics.UI.Qtah.Internal.Interface.QString
import Graphics.UI.Qtah.Internal.Interface.QWidget

c_QPushButton =
  makeClass (ident "QPushButton") Nothing
  [ c_QAbstractButton ]
  [ Ctor (toExtName "QPushButton_new") [TPtr $ TObj c_QWidget]
  , Ctor (toExtName "QPushButton_newWithText") [TObj c_QString, TPtr $ TObj c_QWidget]
  ]
  []
