module Graphics.UI.Qtah.Internal.Interface.QLayoutItem (
  mod_QLayoutItem,
  c_QLayoutItem,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types

mod_QLayoutItem =
  makeQtModule "QLayoutItem" []
  [ QtExportClass qtc_QLayoutItem ]

c_QLayoutItem = qtClassClass qtc_QLayoutItem

qtc_QLayoutItem =
  makeQtClass (ident "QLayoutItem") Nothing []
  []
  []
  []
