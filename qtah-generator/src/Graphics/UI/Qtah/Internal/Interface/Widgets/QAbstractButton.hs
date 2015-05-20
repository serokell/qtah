{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractButton (
  qtModule,
  c_QAbstractButton,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Listener (c_Listener, c_ListenerBool)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)
#include "../Mk.hs.inc"

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
  [ _mkMethod "animateClick" [TInt] TVoid
  , _mkMethod "click" [] TVoid
    -- TODO group
  , _mkMethod "toggle" [] TVoid
  ] ++
  _props
  [ _mkProp "autoExclusive" TBool
  , _mkProp "autoRepeat" TBool
  , _mkProp "autoRepeatDelay" TInt
  , _mkProp "autoRepeatInterval" TInt
  , _mkBoolIsProp "checkable"
  , _mkBoolIsProp "checked"
  , _mkBoolIsProp "down"
    -- TODO icon
  , _mkProp "iconSize" $ TObj c_QSize
    -- TODO shortcut
  , _mkProp "text" $ TObj c_QString
  ]

signals =
  [ _mkSignal "clicked" c_ListenerBool
  , _mkSignal "pressed" c_Listener
  , _mkSignal "released" c_Listener
  , _mkSignal "toggled" c_ListenerBool
  ]
