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
  e_GlobalColor,
  e_LayoutDirection,
  e_NavigationMode,
  e_Orientation,
  bs_Orientations,
  e_ScrollBarPolicy,
  e_TextElideMode,
  e_TextFormat,
  e_TextInteractionFlag,
  bs_TextInteractionFlags,
  e_WindowModality,
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
import Graphics.UI.Qtah.Internal.Flags (qrealFloat)
import Graphics.UI.Qtah.Internal.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule :: AModule
aModule = AQtModule $ makeQtModule ["Core", "Types"] $ map QtExport exports

exports :: [Export]
exports =
  [ ExportEnum e_AlignmentFlag
  , ExportBitspace bs_Alignment
  , ExportEnum e_AspectRatioMode
  , ExportEnum e_CaseSensitivity
  , ExportEnum e_CheckState
  , ExportEnum e_Corner
  , ExportEnum e_CursorMoveStyle
  , ExportEnum e_EventPriority
  , ExportEnum e_GlobalColor
  , ExportEnum e_LayoutDirection
  , ExportEnum e_NavigationMode
  , ExportEnum e_Orientation
  , ExportBitspace bs_Orientations
  , ExportEnum e_ScrollBarPolicy
  , ExportEnum e_TextElideMode
  , ExportEnum e_TextFormat
  , ExportEnum e_TextInteractionFlag
  , ExportBitspace bs_TextInteractionFlags
  , ExportEnum e_WindowModality
  , ExportEnum e_WindowType
  , ExportBitspace bs_WindowFlags
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

e_LayoutDirection =
  makeQtEnum (ident1 "Qt" "LayoutDirection") qtInclude
  [ (0, ["left", "to", "right"])
  , (1, ["right", "to", "left"])
  , (2, ["layout", "direction", "auto"])
  ]

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
