-- This file is part of Qtah.
--
-- Copyright 2015-2018 The Qtah Authors.
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
module Graphics.UI.Qtah.Generator.Interface.Core.Types (
  aModule,
  qreal,
  gluint,
  e_AlignmentFlag,
  bs_Alignment,
  e_AspectRatioMode,
  e_BrushStyle,
  e_CaseSensitivity,
  e_CheckState,
  e_ContextMenuPolicy,
  e_Corner,
  e_CursorMoveStyle,
  e_DropAction,
  bs_DropActions,
  e_EventPriority,
  e_FillRule,
  e_FocusReason,
  e_GlobalColor,
  e_ImageConversionFlag,
  bs_ImageConversionFlags,
  e_InputMethodHint,
  bs_InputMethodHints,
  e_ItemDataRole,
  e_ItemFlag,
  bs_ItemFlags,
  e_Key,
  e_KeyboardModifier,
  bs_KeyboardModifiers,
  e_LayoutDirection,
  e_MaskMode,
  e_MatchFlag,
  bs_MatchFlags,
  e_MouseButton,
  bs_MouseButtons,
  e_MouseEventFlag,
  e_MouseEventFlag_minVersion,
  bs_MouseEventFlags,
  e_MouseEventSource,
  e_MouseEventSource_minVersion,
  e_NavigationMode,
  e_Orientation,
  bs_Orientations,
  e_ScreenOrientation,
  e_ScreenOrientation_minVersion,
  bs_ScreenOrientations,
  e_ScrollBarPolicy,
  e_ScrollPhase,
  e_ScrollPhase_minVersion,
  e_SortOrder,
  e_TextElideMode,
  e_TextFormat,
  e_TextInteractionFlag,
  bs_TextInteractionFlags,
  e_ToolBarArea,
  bs_ToolBarAreas,
  e_ToolButtonStyle,
  e_TransformationMode,
  e_WindowModality,
  e_WindowState,
  bs_WindowStates,
  e_WindowType,
  bs_WindowFlags,
  ) where

import Data.Bits ((.|.))
import Foreign.Hoppy.Generator.Spec (
  CppEnum,
  Export (ExportBitspace, ExportEnum, ExportFn),
  Include,
  Purity (Nonpure),
  Type,
  addReqIncludes,
  ident1,
  includeStd,
  makeFn,
  )
import Foreign.Hoppy.Generator.Types (doubleT, floatT, objT, word32T)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qrealFloat, qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

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
  , just $ ExportEnum e_BrushStyle
  , just $ ExportEnum e_CaseSensitivity
  , just $ ExportEnum e_CheckState
  , just $ ExportEnum e_ContextMenuPolicy
  , just $ ExportEnum e_Corner
  , just $ ExportEnum e_CursorMoveStyle
  , just $ ExportEnum e_DropAction
  , just $ ExportBitspace bs_DropActions
  , just $ ExportEnum e_EventPriority
  , just $ ExportEnum e_FillRule
  , just $ ExportEnum e_FocusReason
  , just $ ExportEnum e_GlobalColor
  , just $ ExportEnum e_ImageConversionFlag
  , just $ ExportBitspace bs_ImageConversionFlags
  , just $ ExportEnum e_InputMethodHint
  , just $ ExportBitspace bs_InputMethodHints
  , just $ ExportEnum e_ItemDataRole
  , just $ ExportEnum e_ItemFlag
  , just $ ExportBitspace bs_ItemFlags
  , just $ ExportEnum e_Key
  , just $ ExportEnum e_KeyboardModifier
  , just $ ExportBitspace bs_KeyboardModifiers
  , just $ ExportEnum e_LayoutDirection
  , just $ ExportEnum e_MaskMode
  , just $ ExportEnum e_MatchFlag
  , just $ ExportBitspace bs_MatchFlags
  , just $ ExportEnum e_MouseButton
  , just $ ExportBitspace bs_MouseButtons
  , test (qtVersion >= e_MouseEventFlag_minVersion) $ ExportEnum e_MouseEventFlag
  , test (qtVersion >= e_MouseEventFlag_minVersion) $ ExportBitspace bs_MouseEventFlags
  , test (qtVersion >= e_MouseEventSource_minVersion) $ ExportEnum e_MouseEventSource
  , just $ ExportEnum e_NavigationMode
  , just $ ExportEnum e_Orientation
  , just $ ExportBitspace bs_Orientations
  , test (qtVersion >= e_ScreenOrientation_minVersion) $ ExportEnum e_ScreenOrientation
  , test (qtVersion >= e_ScreenOrientation_minVersion) $ ExportBitspace bs_ScreenOrientations
  , just $ ExportEnum e_ScrollBarPolicy
  , test (qtVersion >= e_ScrollPhase_minVersion) $ ExportEnum e_ScrollPhase
  , just $ ExportEnum e_SortOrder
  , just $ ExportEnum e_TextElideMode
  , just $ ExportEnum e_TextFormat
  , just $ ExportEnum e_TextInteractionFlag
  , just $ ExportBitspace bs_TextInteractionFlags
  , just $ ExportEnum e_ToolBarArea
  , just $ ExportBitspace bs_ToolBarAreas
  , just $ ExportEnum e_ToolButtonStyle
  , just $ ExportEnum e_TransformationMode
  , just $ ExportEnum e_WindowModality
  , just $ ExportEnum e_WindowState
  , just $ ExportBitspace bs_WindowStates
  , just $ ExportEnum e_WindowType
  , just $ ExportBitspace bs_WindowFlags
  , test (qtVersion < [5, 0]) $ ExportFn f_escape
  ]

qtInclude :: [Include]
qtInclude = [includeStd "Qt"]

qreal :: Type
qreal = if qrealFloat then floatT else doubleT

gluint :: Type
gluint = word32T

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

e_BrushStyle =
  makeQtEnum (ident1 "Qt" "BrushStyle") qtInclude
  [ (0, ["no", "brush"])
  , (1, ["solid", "pattern"])
  , (2, ["dense", "1", "pattern"])
  , (3, ["dense", "2", "pattern"])
  , (4, ["dense", "3", "pattern"])
  , (5, ["dense", "4", "pattern"])
  , (6, ["dense", "5", "pattern"])
  , (7, ["dense", "6", "pattern"])
  , (8, ["dense", "7", "pattern"])
  , (9, ["hor", "pattern"])
  , (10, ["ver", "pattern"])
  , (11, ["cross", "pattern"])
  , (12, ["b", "diag", "pattern"])
  , (13, ["f", "diag", "pattern"])
  , (14, ["diag", "cross", "pattern"])
  , (15, ["linear", "gradient", "pattern"])
  , (16, ["radial", "gradient", "pattern"])
  , (17, ["conical", "gradient", "pattern"])
  , (24, ["texture", "pattern"])
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

e_ContextMenuPolicy :: CppEnum
e_ContextMenuPolicy =
  makeQtEnum (ident1 "Qt" "ContextMenuPolicy") qtInclude
  [ (0, ["no", "context", "menu"])
  , (4, ["prevent", "context", "menu"])
  , (1, ["default", "context", "menu"])
  , (2, ["actions", "context", "menu"])
  , (3, ["custom", "context", "menu"])
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

(e_DropAction, bs_DropActions) =
  makeQtEnumBitspace (ident1 "Qt" "DropAction") "DropActions" qtInclude
  [ (0x0, ["ignore", "action"])
  , (0x1, ["copy", "action"])
  , (0x2, ["move", "action"])
  , (0x4, ["link", "action"])
  , (0xff, ["action", "mask"])
  , (0x8002, ["target", "move", "action"])
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

(e_ImageConversionFlag, bs_ImageConversionFlags) =
  makeQtEnumBitspace (ident1 "Qt" "ImageConversionFlag") "ImageConversionFlags" qtInclude
  -- TODO Lots of synonyms for 0x0.  Hoppy doesn't support these.
  [ (0x0, ["auto"])  -- Not real, this is because Hoppy doesn't support duplicate enum values.
    -- Color/mono preference:
  , (0x3, ["color", "only"])
  , (0x2, ["mono", "only"])
    -- Dithering mode preference for RGB channels:
  , (0x10, ["ordered", "dither"])
  , (0x20, ["threshold", "dither"])
    -- Dithering mode preference for alpha channel:
  , (0x4, ["ordered", "alpha", "dither"])
  , (0x8, ["diffuse", "alpha", "dither"])
    -- Color matching versus dithering preference:
  , (0x40, ["prefer", "dither"])
  , (0x80, ["avoid", "dither"])
  , (0x100, ["no", "opaque", "detection"])
  , (0x200, ["no", "format", "conversion"])
  ]

(e_InputMethodHint, bs_InputMethodHints) =
  makeQtEnumBitspace (ident1 "Qt" "InputMethodHint") "InputMethodHints" qtInclude
  [ (0x0, ["imh", "none"])
  , (0x1, ["imh", "hidden", "text"])
  , (0x2, ["imh", "sensitive", "data"])
  , (0x4, ["imh", "no", "auto", "uppercase"])
  , (0x8, ["imh", "prefer", "numbers"])
  , (0x10, ["imh", "prefer", "uppercase"])
  , (0x20, ["imh", "prefer", "lowercase"])
  , (0x40, ["imh", "no", "predictive", "text"])
  , (0x80, ["imh", "date"])
  , (0x100, ["imh", "time"])
  , (0x200, ["imh", "prefer", "latin"])
  , (0x400, ["imh", "multi", "line"])
  , (0x10000, ["imh", "digits", "only"])
  , (0x20000, ["imh", "formatted", "numbers", "only"])
  , (0x40000, ["imh", "uppercase", "only"])
  , (0x80000, ["imh", "lowercase", "only"])
  , (0x100000, ["imh", "dialable", "characters", "only"])
  , (0x200000, ["imh", "email", "characters", "only"])
  , (0x400000, ["imh", "url", "characters", "only"])
  , (0x800000, ["imh", "latin", "only"])
  , (0xffff0000, ["imh", "exclusive", "input", "mask"])
  ]

-- TODO Support for custom ItemDataRole values.
e_ItemDataRole =
  makeQtEnum (ident1 "Qt" "ItemDataRole") qtInclude $
  collect
  [ -- General-purpose roles:
    just (0, ["display", "role"])
  , just (1, ["decoration", "role"])
  , just (2, ["edit", "role"])
  , just (3, ["tool", "tip", "role"])
  , just (4, ["status", "tip", "role"])
  , just (5, ["whats", "this", "role"])
  , just (13, ["size", "hint", "role"])

    -- Roles describing appearance and metadata:
  , just (6, ["font", "role"])
  , just (7, ["text", "alignment", "role"])
  , just (8, ["background", "role"])
  , just (9, ["foreground", "role"])
  , just (10, ["check", "state", "role"])
  , test (qtVersion >= [4, 8]) (14, ["initial", "sort", "order", "role"])

    -- Accessibility roles:
  , just (11, ["accessible", "text", "role"])
  , just (12, ["accessible", "description", "role"])

    -- User roles:
  , just (0x0100, ["user", "role"])
  ]

(e_ItemFlag, bs_ItemFlags) =
  makeQtEnumBitspace (ident1 "Qt" "ItemFlag") "ItemFlags" qtInclude $
  collect
  [ just (0, ["no", "item", "flags"])
  , just (1, ["item", "is", "selectable"])
  , just (2, ["item", "is", "editable"])
  , just (4, ["item", "is", "drag", "enabled"])
  , just (8, ["item", "is", "drop", "enabled"])
  , just (16, ["item", "is", "user", "checkable"])
  , just (32, ["item", "is", "enabled"])
  , just (64, ["item", "is", "auto", "tristate"])
  , just (128, ["item", "never", "has", "children"])
  , test (qtVersion >= [5, 5]) (256, ["item", "is", "user", "tristate"])
  ]

e_Key =
  makeQtEnum (ident1 "Qt" "Key") qtInclude
  [ (0x01000000, ["key", "escape"])
  , (0x01000001, ["key", "tab"])
  , (0x01000002, ["key", "backtab"])
  , (0x01000003, ["key", "backspace"])
  , (0x01000004, ["key", "return"])
  , (0x01000005, ["key", "enter"])
  , (0x01000006, ["key", "insert"])
  , (0x01000007, ["key", "delete"])
  , (0x01000008, ["key", "pause"])
  , (0x01000009, ["key", "print"])
  , (0x0100000a, ["key", "sys", "req"])
  , (0x0100000b, ["key", "clear"])
  , (0x01000010, ["key", "home"])
  , (0x01000011, ["key", "end"])
  , (0x01000012, ["key", "left"])
  , (0x01000013, ["key", "up"])
  , (0x01000014, ["key", "right"])
  , (0x01000015, ["key", "down"])
  , (0x01000016, ["key", "page", "up"])
  , (0x01000017, ["key", "page", "down"])
  , (0x01000020, ["key", "shift"])
  , (0x01000021, ["key", "control"])
  , (0x01000022, ["key", "meta"])
  , (0x01000023, ["key", "alt"])
  , (0x01001103, ["key", "alt", "gr"])
  , (0x01000024, ["key", "caps", "lock"])
  , (0x01000025, ["key", "num", "lock"])
  , (0x01000026, ["key", "scroll", "lock"])
  , (0x01000030, ["key", "f1"])
  , (0x01000031, ["key", "f2"])
  , (0x01000032, ["key", "f3"])
  , (0x01000033, ["key", "f4"])
  , (0x01000034, ["key", "f5"])
  , (0x01000035, ["key", "f6"])
  , (0x01000036, ["key", "f7"])
  , (0x01000037, ["key", "f8"])
  , (0x01000038, ["key", "f9"])
  , (0x01000039, ["key", "f10"])
  , (0x0100003a, ["key", "f11"])
  , (0x0100003b, ["key", "f12"])
  , (0x0100003c, ["key", "f13"])
  , (0x0100003d, ["key", "f14"])
  , (0x0100003e, ["key", "f15"])
  , (0x0100003f, ["key", "f16"])
  , (0x01000040, ["key", "f17"])
  , (0x01000041, ["key", "f18"])
  , (0x01000042, ["key", "f19"])
  , (0x01000043, ["key", "f20"])
  , (0x01000044, ["key", "f21"])
  , (0x01000045, ["key", "f22"])
  , (0x01000046, ["key", "f23"])
  , (0x01000047, ["key", "f24"])
  , (0x01000048, ["key", "f25"])
  , (0x01000049, ["key", "f26"])
  , (0x0100004a, ["key", "f27"])
  , (0x0100004b, ["key", "f28"])
  , (0x0100004c, ["key", "f29"])
  , (0x0100004d, ["key", "f30"])
  , (0x0100004e, ["key", "f31"])
  , (0x0100004f, ["key", "f32"])
  , (0x01000050, ["key", "f33"])
  , (0x01000051, ["key", "f34"])
  , (0x01000052, ["key", "f35"])
  , (0x01000053, ["key", "super", "l"])
  , (0x01000054, ["key", "super", "r"])
  , (0x01000055, ["key", "menu"])
  , (0x01000056, ["key", "hyper", "l"])
  , (0x01000057, ["key", "hyper", "r"])
  , (0x01000058, ["key", "help"])
  , (0x01000059, ["key", "direction", "l"])
  , (0x01000060, ["key", "direction", "r"])
  , (0x00000020, ["key", "space"])  -- Aka Key_Any.
  , (0x00000021, ["key", "exclam"])
  , (0x00000022, ["key", "quote", "dbl"])
  , (0x00000023, ["key", "number", "sign"])
  , (0x00000024, ["key", "dollar"])
  , (0x00000025, ["key", "percent"])
  , (0x00000026, ["key", "ampersand"])
  , (0x00000027, ["key", "apostrophe"])
  , (0x00000028, ["key", "paren", "left"])
  , (0x00000029, ["key", "paren", "right"])
  , (0x0000002a, ["key", "asterisk"])
  , (0x0000002b, ["key", "plus"])
  , (0x0000002c, ["key", "comma"])
  , (0x0000002d, ["key", "minus"])
  , (0x0000002e, ["key", "period"])
  , (0x0000002f, ["key", "slash"])
  , (0x00000030, ["key", "0"])
  , (0x00000031, ["key", "1"])
  , (0x00000032, ["key", "2"])
  , (0x00000033, ["key", "3"])
  , (0x00000034, ["key", "4"])
  , (0x00000035, ["key", "5"])
  , (0x00000036, ["key", "6"])
  , (0x00000037, ["key", "7"])
  , (0x00000038, ["key", "8"])
  , (0x00000039, ["key", "9"])
  , (0x0000003a, ["key", "colon"])
  , (0x0000003b, ["key", "semicolon"])
  , (0x0000003c, ["key", "less"])
  , (0x0000003d, ["key", "equal"])
  , (0x0000003e, ["key", "greater"])
  , (0x0000003f, ["key", "question"])
  , (0x00000040, ["key", "at"])
  , (0x00000041, ["key", "a"])
  , (0x00000042, ["key", "b"])
  , (0x00000043, ["key", "c"])
  , (0x00000044, ["key", "d"])
  , (0x00000045, ["key", "e"])
  , (0x00000046, ["key", "f"])
  , (0x00000047, ["key", "g"])
  , (0x00000048, ["key", "h"])
  , (0x00000049, ["key", "i"])
  , (0x0000004a, ["key", "j"])
  , (0x0000004b, ["key", "k"])
  , (0x0000004c, ["key", "l"])
  , (0x0000004d, ["key", "m"])
  , (0x0000004e, ["key", "n"])
  , (0x0000004f, ["key", "o"])
  , (0x00000050, ["key", "p"])
  , (0x00000051, ["key", "q"])
  , (0x00000052, ["key", "r"])
  , (0x00000053, ["key", "s"])
  , (0x00000054, ["key", "t"])
  , (0x00000055, ["key", "u"])
  , (0x00000056, ["key", "v"])
  , (0x00000057, ["key", "w"])
  , (0x00000058, ["key", "x"])
  , (0x00000059, ["key", "y"])
  , (0x0000005a, ["key", "z"])
  , (0x0000005b, ["key", "bracket", "left"])
  , (0x0000005c, ["key", "backslash"])
  , (0x0000005d, ["key", "bracket", "right"])
  , (0x0000005e, ["key", "ascii", "circum"])
  , (0x0000005f, ["key", "underscore"])
  , (0x00000060, ["key", "quote", "left"])
  , (0x0000007b, ["key", "brace", "left"])
  , (0x0000007c, ["key", "bar"])
  , (0x0000007d, ["key", "brace", "right"])
  , (0x0000007e, ["key", "ascii", "tilde"])
    -- TODO Additional Qt::Key_* constants.
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

e_MaskMode =
  makeQtEnum (ident1 "Qt" "MaskMode") qtInclude
  [ (0, ["mask", "in", "color"])
  , (1, ["mask", "out", "color"])
  ]

(e_MatchFlag, bs_MatchFlags) =
  makeQtEnumBitspace (ident1 "Qt" "MatchFlag") "MatchFlags" qtInclude
  [ ( 0, ["match", "exactly"])
  , ( 8, ["match", "fixed", "string"])
  , ( 1, ["match", "contains"])
  , ( 2, ["match", "starts", "with"])
  , ( 3, ["match", "ends", "with"])
  , (16, ["match", "case", "sensitive"])
  , ( 4, ["match", "reg", "exp"])
  , ( 5, ["match", "wildcard"])
  , (32, ["match", "wrap"])
  , (64, ["match", "recursive"])
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

e_MouseEventFlag_minVersion = [5, 3]

e_MouseEventSource =
  makeQtEnum (ident1 "Qt" "MouseEventSource") qtInclude
  [ (0, ["mouse", "event", "not", "synthesized"])
  , (1, ["mouse", "event", "synthesized", "by", "system"])
  , (2, ["mouse", "event", "synthesized", "by", "qt"])
  ]

e_MouseEventSource_minVersion = [5, 3]

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

(e_ScreenOrientation, bs_ScreenOrientations) =
  makeQtEnumBitspace (ident1 "Qt" "ScreenOrientation") "ScreenOrientations" qtInclude
  [ (0x0, ["primary", "orientation"])
  , (0x1, ["portrait", "orientation"])
  , (0x2, ["landscape", "orientation"])
  , (0x4, ["inverted", "portrait", "orientation"])
  , (0x8, ["inverted", "landscape", "orientation"])
  ]

e_ScreenOrientation_minVersion = [5, 0]

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

e_ScrollPhase_minVersion = [5, 2]

e_SortOrder =
  makeQtEnum (ident1 "Qt" "SortOrder") qtInclude
  [ (0, ["ascending", "order"])
  , (1, ["descending", "order"])
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

(e_ToolBarArea, bs_ToolBarAreas) =
  makeQtEnumBitspace (ident1 "Qt" "ToolBarArea") "ToolBarAreas" qtInclude
  [ (0x0, ["no", "tool", "bar", "area"])
  , (0x1, ["left", "tool", "bar", "area"])
  , (0x2, ["right", "tool", "bar", "area"])
  , (0x4, ["top", "tool", "bar", "area"])
  , (0x8, ["bottom", "tool", "bar", "area"])
  , (0xf, ["all", "tool", "bar", "areas"])
  ]

e_ToolButtonStyle =
  makeQtEnum (ident1 "Qt" "ToolButtonStyle") qtInclude
  [ (0, ["tool", "button", "icon", "only"])
  , (1, ["tool", "button", "text", "only"])
  , (2, ["tool", "button", "text", "beside", "icon"])
  , (3, ["tool", "button", "text", "under", "icon"])
  , (4, ["tool", "button", "follow", "style"])
  ]

e_TransformationMode =
  makeQtEnum (ident1 "Qt" "TransformationMode") qtInclude
  [ (0, ["fast", "transformation"])
  , (1, ["smooth", "transformation"])
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

f_escape =
  addReqIncludes [includeStd "QTextDocument"] $
  makeFn (ident1 "Qt" "escape") Nothing Nonpure [objT c_QString] $ objT c_QString
