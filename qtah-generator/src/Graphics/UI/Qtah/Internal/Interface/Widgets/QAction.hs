module Graphics.UI.Qtah.Internal.Interface.Widgets.QAction (
  cppopModule,
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

{-# ANN module "HLint: ignore Use camelCase" #-}

cppopModule = makeCppopModule "Widgets" "QAction" qtModule

qtModule =
  makeQtModule "Widgets.QAction" $
  QtExport (ExportClass c_QAction) :
  map QtExportSignal signals ++
  map (QtExport . ExportEnum)
      [ e_ActionEvent
      , e_MenuRole
      , e_Priority
      , e_SoftKeyRole
      ]

c_QAction =
  addReqIncludes [includeStd "QAction"] $
  makeClass (ident "QAction") Nothing
  [ c_QObject ]
  [ mkCtor "new" [TPtr $ TObj c_QObject]
  , mkCtor "newWithText" [TObj c_QString, TPtr $ TObj c_QObject]
    -- TODO newWithIconAndText
  ] $
  [ mkMethod "activate" [TEnum e_ActionEvent] TVoid
    -- TODO associatedGraphicsWidgets
    -- TODO associatedWidgets
  , mkMethod "hover" [] TVoid
  , mkConstMethod "parentWidget" [] $ TPtr $ TObj c_QWidget
  , mkConstMethod "priority" [] $ TEnum e_Priority
  , mkMethod "setDisabled" [TBool] TVoid
  , mkMethod "setPriority" [TEnum e_Priority] TVoid
    -- TODO setShortcuts
  , mkMethod "setSoftKeyRole" [TEnum e_SoftKeyRole] TVoid
    -- TODO shortcuts
  , mkMethod "showStatusText" [TPtr $ TObj c_QWidget] TBool
  , mkConstMethod "softKeyRole" [] $ TEnum e_SoftKeyRole
  , mkMethod "toggle" [] TVoid
  , mkMethod "trigger" [] TVoid
  ] ++
  mkProps
  [ mkProp "actionGroup" $ TPtr $ TObj c_QActionGroup
  , mkProp "autoRepeat" TBool
  , mkBoolIsProp "checkable"
  , mkBoolIsProp "checked"
    -- TODO data
  , mkBoolIsProp "enabled"
    -- TODO font
    -- TODO icon
  , mkProp "iconText" $ TObj c_QString
  , mkBoolIsProp "iconVisibleInMenu"
  , mkProp "menu" $ TPtr $ TObj c_QMenu
  , mkProp "menuRole" $ TEnum e_MenuRole
  , mkBoolIsProp "separator"
    -- TODO shortcut
    -- TODO shortcutContext
  , mkProp "statusTip" $ TObj c_QString
  , mkProp "text" $ TObj c_QString
  , mkProp "toolTip" $ TObj c_QString
  , mkBoolIsProp "visible"
  , mkProp "whatsThis" $ TObj c_QString
  ]

signals =
  [ makeSignal c_QAction "changed" c_Listener
  , makeSignal c_QAction "hovered" c_Listener
  , makeSignal c_QAction "toggled" c_ListenerBool
  , makeSignal c_QAction "triggered" c_ListenerBool
  ]

e_ActionEvent =
  makeQtEnum (ident1 "QAction" "ActionEvent")
  [ (0, ["trigger"])
  , (1, ["hover"])
  ]

e_MenuRole =
  makeQtEnum (ident1 "QAction" "MenuRole")
  [ (0, ["no", "role"])
  , (1, ["text", "heuristic", "role"])
  , (2, ["application", "specific", "role"])
  , (3, ["about", "qt", "role"])
  , (4, ["about", "role"])
  , (5, ["preferences", "role"])
  , (6, ["quit", "role"])
  ]

e_Priority =
  makeQtEnum (ident1 "QAction" "Priority")
  [ (0, ["low", "priority"])
  , (128, ["normal", "priority"])
  , (256, ["high", "priority"])
  ]

e_SoftKeyRole =
  makeQtEnum (ident1 "QAction" "SoftKeyRole")
  [ (0, ["no", "soft", "key"])
  , (1, ["positive", "soft", "key"])
  , (2, ["negative", "soft", "key"])
  , (3, ["select", "soft", "key"])
  ]
