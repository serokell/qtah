module Graphics.UI.Qtah.Internal.Interface.QCoreApplication where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Interface.QObject

c_QCoreApplication =
  makeClass (ident "QCoreApplication") Nothing [c_QObject] [] []
