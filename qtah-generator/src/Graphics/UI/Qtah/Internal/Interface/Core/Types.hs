-- This file is part of Qtah.
--
-- Copyright 2015-2016 Bryan Gardiner <bog@khumba.net>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- | Top-level bindings and bindings in the @Qt::@ namespace.
module Graphics.UI.Qtah.Internal.Interface.Core.Types (
  aModule,
  qreal,
  e_AlignmentFlag,
  bs_Alignment,
  e_AspectRatioMode,
  e_CaseSensitivity,
  e_CheckState,
  e_Corner,
  e_CursorMoveStyle,
  e_EventPriority,
  e_FillRule,
  e_FocusReason,
  e_GlobalColor,
  e_KeyboardModifier,
  bs_KeyboardModifiers,
  e_LayoutDirection,
  e_MouseButton,
  bs_MouseButtons,
  e_MouseEventFlag,
  bs_MouseEventFlags,
  e_MouseEventSource,
  e_NavigationMode,
  e_Orientation,
  bs_Orientations,
  e_ScrollBarPolicy,
  e_ScrollPhase,
  e_TextElideMode,
  e_TextFormat,
  e_TextInteractionFlag,
  bs_TextInteractionFlags,
  e_WindowModality,
  e_WindowState,
  bs_WindowStates,
  e_WindowType,
  bs_WindowFlags,
  ) where

import Data.Bits ((.|.))
import Foreign.Hoppy.Generator.Spec (
  Export (ExportBitspace, ExportEnum),
  Include,
  Type (TDouble, TFloat),
  ident1,
  includeStd,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (qrealFloat, qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule :: AModule
aModule = AQtModule $ makeQtModule ["Core", "Types"] exports

exports :: [QtExport]
exports =
  QtExportSpecials :
  (map QtExport . collect)
  [ just $ ExportEnum e_AlignmentFlag
  , just $ ExportBitspace bs_Alignment
  , just $ ExportEnum e_AspectRatioMode
  , just $ ExportEnum e_CaseSensitivity
  , just $ ExportEnum e_CheckState
  , just $ ExportEnum e_Corner
  , just $ ExportEnum e_CursorMoveStyle
  , just $ ExportEnum e_EventPriority
  , just $ ExportEnum e_FillRule
  , just $ ExportEnum e_FocusReason
  , just $ ExportEnum e_GlobalColor
  , just $ ExportEnum e_KeyboardModifier
  , just $ ExportBitspace bs_KeyboardModifiers
  , just $ ExportEnum e_LayoutDirection
  , just $ ExportEnum e_MouseButton
  , just $ ExportBitspace bs_MouseButtons
  , test (qtVersion >= e_MouseEventFlag_version) $ ExportEnum e_MouseEventFlag
  , test (qtVersion >= e_MouseEventFlag_version) $ ExportBitspace bs_MouseEventFlags
  , test (qtVersion >= e_MouseEventSource_version) $ ExportEnum e_MouseEventSource
  , just $ ExportEnum e_NavigationMode
  , just $ ExportEnum e_Orientation
  , just $ ExportBitspace bs_Orientations
  , just $ ExportEnum e_ScrollBarPolicy
  , test (qtVersion >= e_ScrollPhase_version) $ ExportEnum e_ScrollPhase
  , just $ ExportEnum e_TextElideMode
  , just $ ExportEnum e_TextFormat
  , just $ ExportEnum e_TextInteractionFlag
  , just $ ExportBitspace bs_TextInteractionFlags
  , just $ ExportEnum e_WindowModality
  , just $ ExportEnum e_WindowState
  , just $ ExportBitspace bs_WindowStates
  , just $ ExportEnum e_WindowType
  , just $ ExportBitspace bs_WindowFlags
  ]

qtInclude :: [Include]
qtInclude = [includeStd "Qt"]

qreal :: Type
qreal = if qrealFloat then TFloat else TDouble

(e_AlignmentFlag, bs_Alignment) =
  makeQtEnumBitspace (ident1 "Qt" "AlignmentFlag") "Alignment" qtInclude
  [ -- Horizontal flags.
    (0x01, ["align", "left"])
  , (0x02, ["align", "right"])
  , (0x04, ["align", "h", "center"])
  , (0x08, ["align", "justify"])
    -- Vertical flags.
  , (0x20, ["align", "top"])
  , (0x40, ["align", "bottom"])
  , (0x80, ["align", "v", "center"])
    -- Useful in right-to-left mode.
  , (0x10, ["align", "absolute"])
  ]

e_AspectRatioMode =
  makeQtEnum (ident1 "Qt" "AspectRatioMode") qtInclude
  [ (0, ["ignore", "aspect", "ratio"])
  , (1, ["keep", "aspect", "ratio"])
  , (2, ["keep", "aspect", "ratio", "by", "expanding"])
  ]

e_CaseSensitivity =
  makeQtEnum (ident1 "Qt" "CaseSensitivity") qtInclude
  [ (0, ["case", "insensitive"])
  , (1, ["case", "sensitive"])
  ]

e_CheckState =
  makeQtEnum (ident1 "Qt" "CheckState") qtInclude
  [ (0, ["unchecked"])
  , (1, ["partially", "checked"])
  , (2, ["checked"])
  ]

e_Corner =
  makeQtEnum (ident1 "Qt" "Corner") qtInclude
  [ (0x00000, ["top", "left", "corner"])
  , (0x00001, ["top", "right", "corner"])
  , (0x00002, ["bottom", "left", "corner"])
  , (0x00003, ["bottom", "right", "corner"])
  ]

e_CursorMoveStyle =
  makeQtEnum (ident1 "Qt" "CursorMoveStyle") qtInclude
  [ (0, ["logical", "move", "style"])
  , (1, ["visual", "move", "style"])
  ]

e_EventPriority =
  makeQtEnum (ident1 "Qt" "EventPriority") qtInclude
  [ (1, ["high", "event", "priority"])
  , (0, ["normal", "event", "priority"])
  , (-1, ["low", "event", "priority"])
  ]

e_FillRule =
  makeQtEnum (ident1 "Qt" "FillRule") qtInclude
  [ (0, ["odd", "even", "fill"])
  , (1, ["winding", "fill"])
  ]

e_FocusReason =
  makeQtEnum (ident1 "Qt" "FocusReason") qtInclude
  [ (0, ["mouse", "focus", "reason"])
  , (1, ["tab", "focus", "reason"])
  , (2, ["backtab", "focus", "reason"])
  , (3, ["active", "window", "focus", "reason"])
  , (4, ["popup", "focus", "reason"])
  , (5, ["shortcut", "focus", "reason"])
  , (6, ["menu", "bar", "focus", "reason"])
  , (7, ["other", "focus", "reason"])
  ]

e_GlobalColor =
  makeQtEnum (ident1 "Qt" "GlobalColor") qtInclude
  [ (3, ["white"])
  , (2, ["black"])
  , (7, ["red"])
  , (13, ["dark", "red"])
  , (8, ["green"])
  , (14, ["dark", "green"])
  , (9, ["blue"])
  , (15, ["dark", "blue"])
  , (10, ["cyan"])
  , (16, ["dark", "cyan"])
  , (11, ["magenta"])
  , (17, ["dark", "magenta"])
  , (12, ["yellow"])
  , (18, ["dark", "yellow"])
  , (5, ["gray"])
  , (4, ["dark", "gray"])
  , (6, ["light", "gray"])
  , (19, ["transparent"])
  , (0, ["color0"])
  , (1, ["color1"])
  ]

(e_KeyboardModifier, bs_KeyboardModifiers) =
  makeQtEnumBitspace (ident1 "Qt" "KeyboardModifier") "KeyboardModifiers" qtInclude
  [ (0x00000000, ["no", "modifier"])
  , (0x02000000, ["shift", "modifier"])
  , (0x04000000, ["control", "modifier"])
  , (0x08000000, ["alt", "modifier"])
  , (0x10000000, ["meta", "modifier"])
  , (0x20000000, ["keypad", "modifier"])
  , (0x40000000, ["group", "switch", "modifier"])
  ]

e_LayoutDirection =
  makeQtEnum (ident1 "Qt" "LayoutDirection") qtInclude
  [ (0, ["left", "to", "right"])
  , (1, ["right", "to", "left"])
  , (2, ["layout", "direction", "auto"])
  ]

(e_MouseButton, bs_MouseButtons) =
  makeQtEnumBitspace (ident1 "Qt" "MouseButton") "MouseButtons" qtInclude
  [ (0x00000000, ["no", "button"])
  , (0x07ffffff, ["all", "buttons"])
  , (0x00000001, ["left", "button"])
  , (0x00000002, ["right", "button"])
  , (0x00000004, ["middle", "button"])
    -- TODO Other mouse buttons.  Lots of synonyms here which Hoppy doesn't support.
  ]

(e_MouseEventFlag, bs_MouseEventFlags) =
  makeQtEnumBitspace (ident1 "Qt" "MouseEventFlag") "MouseEventFlags" qtInclude
  [ (0x01, ["mouse", "event", "created", "double", "click"])
  ]

e_MouseEventFlag_version = [5, 3]

e_MouseEventSource =
  makeQtEnum (ident1 "Qt" "MouseEventSource") qtInclude
  [ (0, ["mouse", "event", "not", "synthesized"])
  , (1, ["mouse", "event", "synthesized", "by", "system"])
  , (2, ["mouse", "event", "synthesized", "by", "qt"])
  ]

e_MouseEventSource_version = [5, 3]

e_NavigationMode =
  makeQtEnum (ident1 "Qt" "NavigationMode") qtInclude
  [ (0, ["navigation", "mode", "none"])
  , (1, ["navigation", "mode", "keypad", "tab", "order"])
  , (2, ["navigation", "mode", "keypad", "directional"])
  , (3, ["navigation", "mode", "cursor", "auto"])
  , (4, ["navigation", "mode", "cursor", "force", "visible"])
  ]

(e_Orientation, bs_Orientations) =
  makeQtEnumBitspace (ident1 "Qt" "Orientation") "Orientations" qtInclude
  [ (1, ["horizontal"])
  , (2, ["vertical"])
  ]

e_ScrollBarPolicy =
  makeQtEnum (ident1 "Qt" "ScrollBarPolicy") qtInclude
  [ (0, ["scroll", "bar", "as", "needed"])
  , (1, ["scroll", "bar", "always", "off"])
  , (2, ["scroll", "bar", "always", "on"])
  ]

e_ScrollPhase =
  makeQtEnum (ident1 "Qt" "ScrollPhase") qtInclude
  [ (1, ["scroll", "begin"])
  , (2, ["scroll", "update"])
  , (3, ["scroll", "end"])
  ]

e_ScrollPhase_version = [5, 2]

e_TextElideMode =
  makeQtEnum (ident1 "Qt" "TextElideMode") qtInclude
  [ (0, ["elide", "left"])
  , (1, ["elide", "right"])
  , (2, ["elide", "middle"])
  , (3, ["elide", "none"])
  ]

e_TextFormat =
  makeQtEnum (ident1 "Qt" "TextFormat") qtInclude
  [ (0, ["plain", "text"])
  , (1, ["rich", "text"])
  , (2, ["auto", "text"])
  , (3, ["log", "text"])
  ]

(e_TextInteractionFlag, bs_TextInteractionFlags) =
  makeQtEnumBitspace (ident1 "Qt" "TextInteractionFlag") "TextInteractionFlags" qtInclude $
  let noTextInteraction = 0
      textSelectableByMouse = 1
      textSelectableByKeyboard = 2
      linksAccessibleByMouse = 4
      linksAccessibleByKeyboard = 8
      textEditable = 16
      textEditorInteraction = textSelectableByMouse .|. textSelectableByKeyboard .|. textEditable
      textBrowserInteraction =
        textSelectableByMouse .|. linksAccessibleByMouse .|. linksAccessibleByKeyboard
  in [ (noTextInteraction, ["no", "text", "interaction"])
     , (textSelectableByMouse, ["text", "selectable", "by", "mouse"])
     , (textSelectableByKeyboard, ["text", "selectable", "by", "keyboard"])
     , (linksAccessibleByMouse, ["links", "accessible", "by", "mouse"])
     , (linksAccessibleByKeyboard, ["links", "accessible", "by", "keyboard"])
     , (textEditable, ["text", "editable"])
     , (textEditorInteraction, ["text", "editor", "interaction"])
     , (textBrowserInteraction, ["text", "browser", "interaction"])
     ]

e_WindowModality =
  makeQtEnum (ident1 "Qt" "WindowModality") qtInclude
  [ (0, ["non", "modal"])
  , (1, ["window", "modal"])
  , (2, ["application", "modal"])
  ]

(e_WindowState, bs_WindowStates) =
  makeQtEnumBitspace (ident1 "Qt" "WindowState") "WindowStates" qtInclude
  [ (0x00, ["window", "no", "state"])
  , (0x01, ["window", "minimized"])
  , (0x02, ["window", "maximized"])
  , (0x04, ["window", "full", "screen"])
  , (0x08, ["window", "active"])
  ]

(e_WindowType, bs_WindowFlags) =
  makeQtEnumBitspace (ident1 "Qt" "WindowType") "WindowFlags" qtInclude $
  let widget = 0x0
      window = 0x1
      dialog = 0x2 .|. window
      sheet = 0x4 .|. window
      drawer = sheet .|. dialog
      popup = 0x8 .|. window
      tool = popup .|. dialog
      toolTip = popup .|. sheet
      splashScreen = toolTip .|. dialog
      desktop = 0x10 .|. window
      subWindow = 0x12 .|. window
      foreignWindow = 0x20 .|. window
      coverWindow = 0x40 .|. window
  in [ (widget, ["widget"])
     , (window, ["window"])
     , (dialog, ["dialog"])
     , (sheet, ["sheet"])
     , (drawer, ["drawer"])
     , (popup, ["popup"])
     , (tool, ["tool"])
     , (toolTip, ["tool", "tip"])
     , (splashScreen, ["splash", "screen"])
     , (desktop, ["desktop"])
     , (subWindow, ["sub", "window"])
     , (foreignWindow, ["foreign", "window"])
     , (coverWindow, ["cover", "window"])
     ]
