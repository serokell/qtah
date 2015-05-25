-- | Bindings in the top-level @Qt::@ namespace.
module Graphics.UI.Qtah.Internal.Interface.Core.Types (
  cppopModule,
  qtModule,
  e_Alignment,
  e_AspectRatioMode,
  e_Corner,
  e_CursorMoveStyle,
  e_LayoutDirection,
  e_NavigationMode,
  e_Orientation,
  e_ScrollBarPolicy,
  e_TextFormat,
  ) where

import Foreign.Cppop.Generator.Spec
import Graphics.UI.Qtah.Internal.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

cppopModule :: Module
cppopModule = makeCppopModule "Core" "Types" qtModule

qtModule :: QtModule
qtModule = makeQtModule "Core.Types" $ map QtExport exports

exports :: [Export]
exports =
  [ ExportEnum e_Alignment
  , ExportEnum e_AspectRatioMode
  , ExportEnum e_Corner
  , ExportEnum e_CursorMoveStyle
  , ExportEnum e_LayoutDirection
  , ExportEnum e_NavigationMode
  , ExportEnum e_Orientation
  , ExportEnum e_ScrollBarPolicy
  , ExportEnum e_TextFormat
  ]

e_Alignment =
  makeEnum (ident1 "Qt" "Alignment") Nothing
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
  makeEnum (ident1 "Qt" "AspectRatioMode") Nothing
  [ (0, ["ignore", "aspect", "ratio"])
  , (1, ["keep", "aspect", "ratio"])
  , (2, ["keep", "aspect", "ratio", "by", "expanding"])
  ]

e_Corner =
  makeEnum (ident1 "Qt" "Corner") Nothing
  [ (0x00000, ["top", "left", "corner"])
  , (0x00001, ["top", "right", "corner"])
  , (0x00002, ["bottom", "left", "corner"])
  , (0x00003, ["bottom", "right", "corner"])
  ]

e_CursorMoveStyle =
  makeEnum (ident1 "Qt" "CursorMoveStyle") Nothing
  [ (0, ["logical", "move", "style"])
  , (1, ["visual", "move", "style"])
  ]

e_LayoutDirection =
  makeEnum (ident1 "Qt" "LayoutDirection") Nothing
  [ (0, ["left", "to", "right"])
  , (1, ["right", "to", "left"])
  , (2, ["layout", "direction", "auto"])
  ]

e_NavigationMode =
  makeEnum (ident1 "Qt" "NavigationMode") Nothing
  [ (0, ["navigation", "mode", "none"])
  , (1, ["navigation", "mode", "keypad", "tab", "order"])
  , (2, ["navigation", "mode", "keypad", "directional"])
  , (3, ["navigation", "mode", "cursor", "auto"])
  , (4, ["navigation", "mode", "cursor", "force", "visible"])
  ]

-- TODO Qt::Orientations
e_Orientation =
  makeEnum (ident1 "Qt" "Orientation") Nothing
  [ (1, ["horizontal"])
  , (2, ["vertical"])
  ]

e_ScrollBarPolicy =
  makeEnum (ident1 "Qt" "ScrollBarPolicy") Nothing
  [ (0, ["scroll", "bar", "as", "needed"])
  , (1, ["scroll", "bar", "always", "off"])
  , (2, ["scroll", "bar", "always", "on"])
  ]

e_TextFormat =
  makeEnum (ident1 "Qt" "TextFormat") Nothing
  [ (0, ["plain", "text"])
  , (1, ["rich", "text"])
  , (2, ["auto", "text"])
  , (3, ["log", "text"])
  ]
