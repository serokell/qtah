module Graphics.UI.Qtah.Internal.Interface.Widgets.QLabel (
  cppopModule,
  qtModule,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (e_Alignment, e_TextFormat)
import Graphics.UI.Qtah.Internal.Interface.Listener (c_ListenerQString)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QFrame (c_QFrame)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

cppopModule = makeCppopModule "Widgets" "QLabel" qtModule

qtModule =
  makeQtModule "Widgets.QLabel" $
  QtExport (ExportClass c_QLabel) :
  map QtExportSignal signals

this = c_QLabel

c_QLabel =
  addReqIncludes [includeStd "QLabel"] $
  makeClass (ident "QLabel") Nothing [c_QFrame]
  [ mkCtor this "new" []
  , mkCtor this "newWithParent" [TPtr $ TObj c_QWidget]
  , mkCtor this "newWithText" [TObj c_QString]
  , mkCtor this "newWithTextAndParent" [TObj c_QString, TPtr $ TObj c_QWidget]
    -- Ctors taking Qt::WindowFlags.
  ] $
  [ mkMethod this "clear" [] TVoid
  , mkConstMethod this "hasSelectedText" [] TBool
  , mkConstMethod this "selectedText" [] $ TObj c_QString
  , mkConstMethod this "selectionStart" [] TInt
  , mkMethod' this "setNum" "setInt" [TInt] TVoid
  , mkMethod' this "setNum" "setDouble" [TDouble] TVoid
  , mkMethod this "setSelection" [TInt, TInt] TVoid
  ] ++
  mkProps
  [ mkProp this "alignment" $ TEnum e_Alignment
  , mkProp this "buddy" $ TPtr $ TObj c_QWidget
  , mkProp this "indent" TInt
  , mkProp this "margin" TInt
    -- TODO movie
  , mkProp this "openExternalLinks" TBool
    -- TODO picture
    -- TODO pixmap
  , mkBoolHasProp this "scaledContents"
  , mkProp this "text" $ TObj c_QString
  , mkProp this "textFormat" $ TEnum e_TextFormat
    -- TODO textInteractionFlags
  , mkProp this "wordWrap" TBool
  ]

signals =
  [ makeSignal this "linkActivated" c_ListenerQString
  , makeSignal this "linkHovered" c_ListenerQString
  ]
