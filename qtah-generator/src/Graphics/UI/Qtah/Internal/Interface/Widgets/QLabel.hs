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

c_QLabel =
  addReqIncludes [includeStd "QLabel"] $
  makeClass (ident "QLabel") Nothing [c_QFrame]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  , mkCtor "newWithText" [TObj c_QString]
  , mkCtor "newWithTextAndParent" [TObj c_QString, TPtr $ TObj c_QWidget]
    -- Ctors taking Qt::WindowFlags.
  ] $
  [ mkMethod "clear" [] TVoid
  , mkConstMethod "hasSelectedText" [] TBool
  , mkConstMethod "selectedText" [] $ TObj c_QString
  , mkConstMethod "selectionStart" [] TInt
  , mkMethod' "setNum" "setInt" [TInt] TVoid
  , mkMethod' "setNum" "setDouble" [TDouble] TVoid
  , mkMethod "setSelection" [TInt, TInt] TVoid
  ] ++
  mkProps
  [ mkProp "alignment" $ TEnum e_Alignment
  , mkProp "buddy" $ TPtr $ TObj c_QWidget
  , mkProp "indent" TInt
  , mkProp "margin" TInt
    -- TODO movie
  , mkProp "openExternalLinks" TBool
    -- TODO picture
    -- TODO pixmap
  , mkBoolHasProp "scaledContents"
  , mkProp "text" $ TObj c_QString
  , mkProp "textFormat" $ TEnum e_TextFormat
    -- TODO textInteractionFlags
  , mkProp "wordWrap" TBool
  ]

signals =
  [ makeSignal c_QLabel "linkActivated" c_ListenerQString
  , makeSignal c_QLabel "linkHovered" c_ListenerQString
  ]
