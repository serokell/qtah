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

c_QAbstractButton =
  addReqIncludes [includeStd "QAbstractButton"] $
  makeClass (ident "QAbstractButton") Nothing
  [ c_QWidget ]
  [] $  -- Abstact.
  [ mkMethod "animateClick" [TInt] TVoid
  , mkMethod "click" [] TVoid
    -- TODO group
  , mkMethod "toggle" [] TVoid
  ] ++
  mkProps
  [ mkProp "autoExclusive" TBool
  , mkProp "autoRepeat" TBool
  , mkProp "autoRepeatDelay" TInt
  , mkProp "autoRepeatInterval" TInt
  , mkBoolIsProp "checkable"
  , mkBoolIsProp "checked"
  , mkBoolIsProp "down"
    -- TODO icon
  , mkProp "iconSize" $ TObj c_QSize
    -- TODO shortcut
  , mkProp "text" $ TObj c_QString
  ]

signals =
  [ makeSignal c_QAbstractButton "clicked" c_ListenerBool
  , makeSignal c_QAbstractButton "pressed" c_Listener
  , makeSignal c_QAbstractButton "released" c_Listener
  , makeSignal c_QAbstractButton "toggled" c_ListenerBool
  ]
