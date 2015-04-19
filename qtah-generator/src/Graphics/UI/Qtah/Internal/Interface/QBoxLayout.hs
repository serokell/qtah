module Graphics.UI.Qtah.Internal.Interface.QBoxLayout (
  mod_QBoxLayout,
  c_QBoxLayout,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.QLayout

mod_QBoxLayout =
  makeQtModule "QBoxLayout" []
  [ QtExportClass qtc_QBoxLayout ]

c_QBoxLayout = qtClassClass qtc_QBoxLayout

qtc_QBoxLayout =
  makeQtClass (ident "QBoxLayout") Nothing [c_QLayout]
  []
  []
  []
