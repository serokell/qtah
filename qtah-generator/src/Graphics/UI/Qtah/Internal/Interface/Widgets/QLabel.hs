{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Widgets.QLabel (
  qtModule,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (e_Alignment, e_TextFormat)
import Graphics.UI.Qtah.Internal.Interface.Listener (c_ListenerQString)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QFrame (c_QFrame)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)
#include "../Mk.hs.inc"

qtModule =
  makeQtModule "Widgets.QLabel" $
  QtExport (ExportClass c_QLabel) :
  map QtExportSignal signals

this = c_QLabel

c_QLabel =
  addReqIncludes [includeStd "QLabel"] $
  makeClass (ident "QLabel") Nothing [c_QFrame]
  [ _mkCtor "new" []
  , _mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  , _mkCtor "newWithText" [TObj c_QString]
  , _mkCtor "newWithTextAndParent" [TObj c_QString, TPtr $ TObj c_QWidget]
    -- Ctors taking Qt::WindowFlags.
  ] $
  [ _mkMethod "clear" [] TVoid
  , _mkConstMethod "hasSelectedText" [] TBool
  , _mkConstMethod "selectedText" [] $ TObj c_QString
  , _mkConstMethod "selectionStart" [] TInt
  , _mkMethod' "setNum" "setInt" [TInt] TVoid
  , _mkMethod' "setNum" "setDouble" [TDouble] TVoid
  , _mkMethod "setSelection" [TInt, TInt] TVoid
  ] ++
  _props
  [ _mkProp "alignment" $ TEnum e_Alignment
  , _mkProp "buddy" $ TPtr $ TObj c_QWidget
  , _mkProp "indent" TInt
  , _mkProp "margin" TInt
    -- TODO movie
  , _mkProp "openExternalLinks" TBool
    -- TODO picture
    -- TODO pixmap
  , _mkBoolHasProp "scaledContents"
  , _mkProp "text" $ TObj c_QString
  , _mkProp "textFormat" $ TEnum e_TextFormat
    -- TODO textInteractionFlags
  , _mkProp "wordWrap" TBool
  ]

signals =
  [ _mkSignal "linkActivated" c_ListenerQString
  , _mkSignal "linkHovered" c_ListenerQString
  ]
