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

module Graphics.UI.Qtah.Generator.Interface.Gui.QPainter (
  aModule,
  c_QPainter,
  e_RenderHint,
  bs_RenderHints,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass, ExportEnum, ExportBitspace),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkCtor,
  mkMethod,
  mkMethod',
  )
import Foreign.Hoppy.Generator.Types (bitspaceT, enumT, intT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just)
-- import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (bs_ImageConversionFlags, e_GlobalColor)
import Graphics.UI.Qtah.Generator.Interface.Gui.QImage (c_QImage)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPaintDevice (c_QPaintDevice)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QPainter"] $
  (QtExport $ ExportClass c_QPainter) :
  (QtExport $ ExportEnum e_RenderHint) :
  (QtExport $ ExportBitspace bs_RenderHints) :
  []

c_QPainter =
  addReqIncludes [includeStd "QPainter"] $
  classSetEntityPrefix "" $
  makeClass (ident "QPainter") Nothing [] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithDevice" [ptrT $ objT c_QPaintDevice]
  , just $ mkMethod' "drawImage" "drawImageAtRaw" [intT, intT, objT c_QImage] voidT
  , just $ mkMethod' "drawImage" "drawImageAtRawAll"
    [intT, intT, objT c_QImage, intT, intT, intT, intT, bitspaceT bs_ImageConversionFlags] voidT
  , just $ mkMethod' "fillRect" "fillRectWithGlobalColor" [objT c_QRect, enumT e_GlobalColor] voidT
  , just $ mkMethod "setRenderHint" [enumT e_RenderHint] voidT
  ]

(e_RenderHint, bs_RenderHints) =
  makeQtEnumBitspace (ident1 "QPainter" "RenderHint") "RenderHints" [includeStd "QPainter"] $
  [ (0x01, ["antialiasing"])
  , (0x02, ["text","antialiasing"])
  , (0x04, ["smooth","pixmap","transform"])
  , (0x08, ["high","quality","antialiasing"])
  , (0x10, ["non","cosmetic","default","pen"])
  , (0x20, ["qt4","compatible","painting"])
  ]
