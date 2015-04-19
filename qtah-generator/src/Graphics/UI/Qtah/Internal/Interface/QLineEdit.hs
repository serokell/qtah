{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.QLineEdit (
  mod_QLineEdit,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Listener
import Graphics.UI.Qtah.Internal.Interface.QString
import Graphics.UI.Qtah.Internal.Interface.QWidget

this = c_QLineEdit
thisQt = qtc_QLineEdit
#include "MkQt.hs.inc"

mod_QLineEdit =
  makeQtModule "QLineEdit" []
  [ QtExportClass qtc_QLineEdit ]

c_QLineEdit = qtClassClass qtc_QLineEdit

qtc_QLineEdit =
  makeQtClass (ident "QLineEdit") Nothing [c_QWidget]
  [ _mkCtor "new" []
  , _mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  , _mkCtor "newWithText" [TObj c_QString]
  , _mkCtor "newWithTextAndParent" [TObj c_QString, TPtr $ TObj c_QWidget]
  ]
  [ _mkMethod "backspace" [] TVoid
  , _mkMethod "clear" [] TVoid
  , _mkConstMethod "copy" [] TVoid
  , _mkMethod "cut" [] TVoid
  , _mkConstMethod "displayText" [] $ TObj c_QString
  , _mkMethod "paste" [] TVoid
  , _mkMethod "redo" [] TVoid
  , _mkMethod "selectAll" [] TVoid
  , _mkMethod "setText" [TObj c_QString] TVoid
  , _mkConstMethod "text" [] $ TObj c_QString
  , _mkMethod "undo" [] TVoid
  ]
  [ _mkSignal "cursorPositionChanged" c_ListenerIntInt
  , _mkSignal "editingFinished" c_Listener
  , _mkSignal "returnPressed" c_Listener
  , _mkSignal "selectionChanged" c_Listener
  , _mkSignal "textEdited" c_ListenerQString
  , _mkSignal "textChanged" c_ListenerQString
  ]
