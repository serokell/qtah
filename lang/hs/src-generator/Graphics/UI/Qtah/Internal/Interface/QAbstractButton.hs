{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.QAbstractButton (
  c_QAbstractButton,
  qtc_QAbstractButton,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Moc
import Graphics.UI.Qtah.Internal.Interface.Listener
import Graphics.UI.Qtah.Internal.Interface.QString
import Graphics.UI.Qtah.Internal.Interface.QWidget

this = c_QAbstractButton
thisQt = qtc_QAbstractButton
#include "MkQt.hs.inc"

c_QAbstractButton = qtClassClass qtc_QAbstractButton

qtc_QAbstractButton =
  makeQtClass (ident "QAbstractButton") Nothing
  [ c_QWidget ]
  []
  [ _mkMethod "setText" [TObj c_QString] TVoid
  , _mkConstMethod "text" [] $ TObj c_QString
  ]
  [ _mkSignal "clicked" c_ListenerBool
  ]
