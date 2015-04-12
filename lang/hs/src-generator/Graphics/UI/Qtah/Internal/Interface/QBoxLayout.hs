module Graphics.UI.Qtah.Internal.Interface.QBoxLayout (
  c_QBoxLayout,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Interface.QLayout

c_QBoxLayout =
  makeClass (ident "QBoxLayout") Nothing [c_QLayout]
  []
  []
