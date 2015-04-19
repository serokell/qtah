{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.QAbstractScrollArea (
  mod_QAbstractScrollArea,
  c_QAbstractScrollArea,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.QWidget

this = c_QAbstractScrollArea
thisQt = qtc_QAbstractScrollArea
#include "MkQt.hs.inc"

mod_QAbstractScrollArea =
  makeQtModule "QAbstractScrollArea" []
  [ QtExportClass thisQt ]

c_QAbstractScrollArea = qtClassClass qtc_QAbstractScrollArea

qtc_QAbstractScrollArea =
  makeQtClass (ident "QAbstractScrollArea") Nothing [c_QWidget]
  [ _mkCtor "new" []
  , _mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ]
  []
  []
