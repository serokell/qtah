{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.QPushButton (
  mod_QPushButton,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.QAbstractButton
import Graphics.UI.Qtah.Internal.Interface.QString
import Graphics.UI.Qtah.Internal.Interface.QWidget

this = c_QPushButton
thisQt = qtc_QPushButton
#include "MkQt.hs.inc"

mod_QPushButton =
  makeQtModule "QPushButton"
  [ QtExportClass qtc_QPushButton ]

c_QPushButton = qtClassClass qtc_QPushButton

qtc_QPushButton =
  makeQtClass (ident "QPushButton") Nothing
  [ c_QAbstractButton ]
  [ _mkCtor "new" [TPtr $ TObj c_QWidget]
  , _mkCtor "newWithText" [TObj c_QString, TPtr $ TObj c_QWidget]
  ]
  []
  []
