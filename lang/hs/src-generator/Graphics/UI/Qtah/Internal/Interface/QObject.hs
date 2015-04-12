{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.QObject (
  c_QObject,
  qtc_QObject,
  ) where

import Foreign.Cppop.Generator.Spec
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Listener
import Graphics.UI.Qtah.Internal.Interface.QString
import Graphics.UI.Qtah.Internal.Generator.Moc

this = c_QObject
thisQt = qtc_QObject
#include "MkQt.hs.inc"

c_QObject = qtClassClass qtc_QObject

qtc_QObject =
  makeQtClass (ident "QObject") Nothing []
  []
  [ _mkMethod "blockSignals" [TBool] TBool
  , _mkMethod "dumpObjectInfo" [] TVoid
  , _mkMethod "dumpObjectTree" [] TVoid
  , _mkMethod "installEventFilter" [TPtr $ TObj c_QObject] TVoid
  , _mkConstMethod "isWidgetType" [] TBool
  , _mkMethod "killTimer" [TInt] TVoid
  , _mkConstMethod "objectName" [] $ TObj c_QString
  , _mkConstMethod "parent" [] $ TPtr $ TObj c_QObject
  , _mkMethod "removeEventFilter" [TPtr $ TObj c_QObject] TVoid
  , _mkMethod "setParent" [TPtr $ TObj c_QObject] TVoid
  , _mkConstMethod "signalsBlocked" [] TBool
  , _mkMethod "startTimer" [TInt] TInt
  ]
  [ _mkSignal "destroyed" c_ListenerPtrQObject
  ]
