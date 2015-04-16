module Graphics.UI.Qtah.Internal.Interface.QCoreApplication (
  mod_QCoreApplication,
  c_QCoreApplication,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.QObject

mod_QCoreApplication =
  makeQtModule "QCoreApplication"
  [ QtExportClass qtc_QCoreApplication ]

c_QCoreApplication = qtClassClass qtc_QCoreApplication

qtc_QCoreApplication =
  makeQtClass (ident "QCoreApplication") Nothing [c_QObject] [] [] []
