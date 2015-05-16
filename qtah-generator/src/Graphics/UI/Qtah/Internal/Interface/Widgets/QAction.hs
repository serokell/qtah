{-# LANGUAGE CPP #-}

module Graphics.UI.Qtah.Internal.Interface.Widgets.QAction (
  qtModule,
  c_QAction,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Listener (c_Listener, c_ListenerBool)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QActionGroup (c_QActionGroup)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QMenu (c_QMenu)
import {-# SOURCE #-} Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)
#include "../Mk.hs.inc"

qtModule =
  makeQtModuleForClass c_QAction $
  map QtExportSignal signals ++
  map (QtExport . ExportEnum)
      [ e_ActionEvent
      , e_MenuRole
      , e_Priority
      , e_SoftKeyRole
      ]

this = c_QAction

c_QAction =
  addReqIncludes [includeStd "QAction"] $
  makeClass (ident "QAction") Nothing
  [ c_QObject ]
  [ _mkCtor "new" [TPtr $ TObj c_QObject]
  , _mkCtor "newWithText" [TObj c_QString, TPtr $ TObj c_QObject]
    -- TODO newWithIconAndText
  ] $
  [ _mkMethod "activate" [TEnum e_ActionEvent] TVoid
    -- TODO associatedGraphicsWidgets
    -- TODO associatedWidgets
  , _mkMethod "hover" [] TVoid
  , _mkConstMethod "parentWidget" [] $ TPtr $ TObj c_QWidget
  , _mkConstMethod "priority" [] $ TEnum e_Priority
  , _mkMethod "setDisabled" [TBool] TVoid
  , _mkMethod "setPriority" [TEnum e_Priority] TVoid
    -- TODO setShortcuts
  , _mkMethod "setSoftKeyRole" [TEnum e_SoftKeyRole] TVoid
    -- TODO shortcuts
  , _mkMethod "showStatusText" [TPtr $ TObj c_QWidget] TBool
  , _mkConstMethod "softKeyRole" [] $ TEnum e_SoftKeyRole
  , _mkMethod "toggle" [] TVoid
  , _mkMethod "trigger" [] TVoid
  ] ++
  _props
  [ _mkProp "actionGroup" $ TPtr $ TObj c_QActionGroup
  , _mkProp "autoRepeat" TBool
  , _mkBoolIsProp "checkable"
  , _mkBoolIsProp "checked"
    -- TODO data
  , _mkBoolIsProp "enabled"
    -- TODO font
    -- TODO icon
  , _mkProp "iconText" $ TObj c_QString
  , _mkBoolIsProp "iconVisibleInMenu"
  , _mkProp "menu" $ TPtr $ TObj c_QMenu
  , _mkProp "menuRole" $ TEnum e_MenuRole
  , _mkBoolIsProp "separator"
    -- TODO shortcut
    -- TODO shortcutContext
  , _mkProp "statusTip" $ TObj c_QString
  , _mkProp "text" $ TObj c_QString
  , _mkProp "toolTip" $ TObj c_QString
  , _mkBoolIsProp "visible"
  , _mkProp "whatsThis" $ TObj c_QString
  ]

signals =
  [ _mkSignal "changed" c_Listener
  , _mkSignal "hovered" c_Listener
  , _mkSignal "toggled" c_ListenerBool
  , _mkSignal "triggered" c_ListenerBool
  ]

e_ActionEvent =
  makeEnum (ident1 "QAction" "ActionEvent") Nothing
  [ (0, ["trigger"])
  , (1, ["hover"])
  ]

e_MenuRole =
  makeEnum (ident1 "QAction" "MenuRole") Nothing
  [ (0, ["no", "role"])
  , (1, ["text", "heuristic", "role"])
  , (2, ["application", "specific", "role"])
  , (3, ["about", "qt", "role"])
  , (4, ["about", "role"])
  , (5, ["preferences", "role"])
  , (6, ["quit", "role"])
  ]

e_Priority =
  makeEnum (ident1 "QAction" "Priority") Nothing
  [ (0, ["low", "priority"])
  , (128, ["normal", "priority"])
  , (256, ["high", "priority"])
  ]

e_SoftKeyRole =
  makeEnum (ident1 "QAction" "SoftKeyRole") Nothing
  [ (0, ["no", "soft", "key"])
  , (1, ["positive", "soft", "key"])
  , (2, ["negative", "soft", "key"])
  , (3, ["select", "soft", "key"])
  ]
