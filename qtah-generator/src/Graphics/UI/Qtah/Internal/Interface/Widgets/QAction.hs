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

this = c_QAction

c_QAction =
  addReqIncludes [includeStd "QAction"] $
  makeClass (ident "QAction") Nothing
  [ c_QObject ]
  [ mkCtor this "new" [TPtr $ TObj c_QObject]
  , mkCtor this "newWithText" [TObj c_QString, TPtr $ TObj c_QObject]
    -- TODO newWithIconAndText
  ] $
  [ mkMethod this "activate" [TEnum e_ActionEvent] TVoid
    -- TODO associatedGraphicsWidgets
    -- TODO associatedWidgets
  , mkMethod this "hover" [] TVoid
  , mkConstMethod this "parentWidget" [] $ TPtr $ TObj c_QWidget
  , mkConstMethod this "priority" [] $ TEnum e_Priority
  , mkMethod this "setDisabled" [TBool] TVoid
  , mkMethod this "setPriority" [TEnum e_Priority] TVoid
    -- TODO setShortcuts
  , mkMethod this "setSoftKeyRole" [TEnum e_SoftKeyRole] TVoid
    -- TODO shortcuts
  , mkMethod this "showStatusText" [TPtr $ TObj c_QWidget] TBool
  , mkConstMethod this "softKeyRole" [] $ TEnum e_SoftKeyRole
  , mkMethod this "toggle" [] TVoid
  , mkMethod this "trigger" [] TVoid
  ] ++
  mkProps
  [ mkProp this "actionGroup" $ TPtr $ TObj c_QActionGroup
  , mkProp this "autoRepeat" TBool
  , mkBoolIsProp this "checkable"
  , mkBoolIsProp this "checked"
    -- TODO data
  , mkBoolIsProp this "enabled"
    -- TODO font
    -- TODO icon
  , mkProp this "iconText" $ TObj c_QString
  , mkBoolIsProp this "iconVisibleInMenu"
  , mkProp this "menu" $ TPtr $ TObj c_QMenu
  , mkProp this "menuRole" $ TEnum e_MenuRole
  , mkBoolIsProp this "separator"
    -- TODO shortcut
    -- TODO shortcutContext
  , mkProp this "statusTip" $ TObj c_QString
  , mkProp this "text" $ TObj c_QString
  , mkProp this "toolTip" $ TObj c_QString
  , mkBoolIsProp this "visible"
  , mkProp this "whatsThis" $ TObj c_QString
  ]

signals =
  [ makeSignal this "changed" c_Listener
  , makeSignal this "hovered" c_Listener
  , makeSignal this "toggled" c_ListenerBool
  , makeSignal this "triggered" c_ListenerBool
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
