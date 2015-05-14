{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Widgets.QLineEdit (
  qtModule,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Listener
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

qtModule = makeQtModuleForClass c_QLineEdit $ map QtExportSignal signals

this = c_QLineEdit
#include "../Mk.hs.inc"

c_QLineEdit =
  addReqIncludes [includeStd "QLineEdit"] $
  makeClass (ident "QLineEdit") Nothing [c_QWidget]
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

signals =
  [ _mkSignal "cursorPositionChanged" c_ListenerIntInt
  , _mkSignal "editingFinished" c_Listener
  , _mkSignal "returnPressed" c_Listener
  , _mkSignal "selectionChanged" c_Listener
  , _mkSignal "textEdited" c_ListenerQString
  , _mkSignal "textChanged" c_ListenerQString
  ]
