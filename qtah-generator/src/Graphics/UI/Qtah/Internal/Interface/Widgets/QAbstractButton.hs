module Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractButton (
  cppopModule,
  qtModule,
  c_QAbstractButton,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Listener (c_Listener, c_ListenerBool)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

cppopModule = makeCppopModule "Widgets" "QAbstractButton" qtModule

qtModule =
  makeQtModule "Widgets.QAbstractButton" $
  QtExport (ExportClass c_QAbstractButton) :
  map QtExportSignal signals

this = c_QAbstractButton

c_QAbstractButton =
  addReqIncludes [includeStd "QAbstractButton"] $
  makeClass (ident "QAbstractButton") Nothing
  [ c_QWidget ]
  [] $  -- Abstact.
  [ mkMethod this "animateClick" [TInt] TVoid
  , mkMethod this "click" [] TVoid
    -- TODO group
  , mkMethod this "toggle" [] TVoid
  ] ++
  mkProps
  [ mkProp this "autoExclusive" TBool
  , mkProp this "autoRepeat" TBool
  , mkProp this "autoRepeatDelay" TInt
  , mkProp this "autoRepeatInterval" TInt
  , mkBoolIsProp this "checkable"
  , mkBoolIsProp this "checked"
  , mkBoolIsProp this "down"
    -- TODO icon
  , mkProp this "iconSize" $ TObj c_QSize
    -- TODO shortcut
  , mkProp this "text" $ TObj c_QString
  ]

signals =
  [ makeSignal this "clicked" c_ListenerBool
  , makeSignal this "pressed" c_Listener
  , makeSignal this "released" c_Listener
  , makeSignal this "toggled" c_ListenerBool
  ]
