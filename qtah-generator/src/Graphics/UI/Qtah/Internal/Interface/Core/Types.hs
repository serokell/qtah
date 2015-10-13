-- | Bindings in the top-level @Qt::@ namespace.
module Graphics.UI.Qtah.Internal.Interface.Core.Types (
  cppopModule,
  qtModule,
  e_AlignmentFlag,
  bs_Alignment,
  e_AspectRatioMode,
  e_CheckState,
  e_Corner,
  e_CursorMoveStyle,
  e_LayoutDirection,
  e_NavigationMode,
  e_Orientation,
  bs_Orientations,
  e_ScrollBarPolicy,
  e_TextFormat,
  ) where

import Foreign.Cppop.Generator.Spec (
  Export (ExportBitspace, ExportEnum),
  Module,
  ident1,
  )
import Graphics.UI.Qtah.Internal.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

cppopModule :: Module
cppopModule = makeCppopModule "Core" "Types" qtModule

qtModule :: QtModule
qtModule = makeQtModule "Core.Types" $ map QtExport exports

exports :: [Export]
exports =
  [ ExportEnum e_AlignmentFlag
  , ExportBitspace bs_Alignment
  , ExportEnum e_AspectRatioMode
  , ExportEnum e_CheckState
  , ExportEnum e_Corner
  , ExportEnum e_CursorMoveStyle
  , ExportEnum e_LayoutDirection
  , ExportEnum e_NavigationMode
  , ExportEnum e_Orientation
  , ExportBitspace bs_Orientations
  , ExportEnum e_ScrollBarPolicy
  , ExportEnum e_TextFormat
  ]

(e_AlignmentFlag, bs_Alignment) =
  makeQtEnumBitspace (ident1 "Qt" "AlignmentFlag") "Alignment"
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
  makeQtEnum (ident1 "Qt" "AspectRatioMode")
  [ (0, ["ignore", "aspect", "ratio"])
  , (1, ["keep", "aspect", "ratio"])
  , (2, ["keep", "aspect", "ratio", "by", "expanding"])
  ]

e_CheckState =
  makeQtEnum (ident1 "Qt" "CheckState")
  [ (0, ["unchecked"])
  , (1, ["partially", "checked"])
  , (2, ["checked"])
  ]

e_Corner =
  makeQtEnum (ident1 "Qt" "Corner")
  [ (0x00000, ["top", "left", "corner"])
  , (0x00001, ["top", "right", "corner"])
  , (0x00002, ["bottom", "left", "corner"])
  , (0x00003, ["bottom", "right", "corner"])
  ]

e_CursorMoveStyle =
  makeQtEnum (ident1 "Qt" "CursorMoveStyle")
  [ (0, ["logical", "move", "style"])
  , (1, ["visual", "move", "style"])
  ]

e_LayoutDirection =
  makeQtEnum (ident1 "Qt" "LayoutDirection")
  [ (0, ["left", "to", "right"])
  , (1, ["right", "to", "left"])
  , (2, ["layout", "direction", "auto"])
  ]

e_NavigationMode =
  makeQtEnum (ident1 "Qt" "NavigationMode")
  [ (0, ["navigation", "mode", "none"])
  , (1, ["navigation", "mode", "keypad", "tab", "order"])
  , (2, ["navigation", "mode", "keypad", "directional"])
  , (3, ["navigation", "mode", "cursor", "auto"])
  , (4, ["navigation", "mode", "cursor", "force", "visible"])
  ]

(e_Orientation, bs_Orientations) =
  makeQtEnumBitspace (ident1 "Qt" "Orientation") "Orientations"
  [ (1, ["horizontal"])
  , (2, ["vertical"])
  ]

e_ScrollBarPolicy =
  makeQtEnum (ident1 "Qt" "ScrollBarPolicy")
  [ (0, ["scroll", "bar", "as", "needed"])
  , (1, ["scroll", "bar", "always", "off"])
  , (2, ["scroll", "bar", "always", "on"])
  ]

e_TextFormat =
  makeQtEnum (ident1 "Qt" "TextFormat")
  [ (0, ["plain", "text"])
  , (1, ["rich", "text"])
  , (2, ["auto", "text"])
  , (3, ["log", "text"])
  ]
