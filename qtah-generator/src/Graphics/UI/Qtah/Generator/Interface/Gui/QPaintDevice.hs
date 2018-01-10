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

module Graphics.UI.Qtah.Generator.Interface.Gui.QPaintDevice (
  aModule,
  c_QPaintDevice,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass, ExportEnum),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  )
import Foreign.Hoppy.Generator.Types (boolT, intT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qreal)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QPaintDevice"]
  [ QtExport $ ExportClass c_QPaintDevice
  , QtExport $ ExportEnum e_PaintDeviceMetric
  ]

c_QPaintDevice =
  addReqIncludes [includeStd "QPaintDevice"] $
  classSetEntityPrefix "" $
  makeClass (ident "QPaintDevice") Nothing [] $
  collect
  [ just $ mkConstMethod "colorCount" [] intT
  , just $ mkConstMethod "depth" [] intT
  , test (qtVersion >= [5, 0]) $ mkConstMethod "devicePixelRatio" [] intT
  , test (qtVersion >= [5, 6]) $ mkConstMethod "devicePixelRatioF" [] qreal
  , just $ mkConstMethod "height" [] intT
  , just $ mkConstMethod "heightMM" [] intT
  , just $ mkConstMethod "logicalDpiX" [] intT
  , just $ mkConstMethod "logicalDpiY" [] intT
    -- TODO paintEngine
  , just $ mkConstMethod "paintingActive" [] boolT
  , just $ mkConstMethod "physicalDpiX" [] intT
  , just $ mkConstMethod "physicalDpiY" [] intT
  , just $ mkConstMethod "width" [] intT
  , just $ mkConstMethod "widthMM" [] intT
  ]

e_PaintDeviceMetric =
  makeQtEnum (ident1 "QPaintDevice" "PaintDeviceMetric") [includeStd "QPaintDevice"] $
  [ (1, ["pdm", "width"])
  , (2, ["pdm", "height"])
  , (3, ["pdm", "width", "mm"])
  , (4, ["pdm", "height", "mm"])
  , (5, ["pdm", "num", "colors"])
  , (6, ["pdm", "depth"])
  , (7, ["pdm", "dpi", "x"])
  , (8, ["pdm", "dpi", "y"])
  , (9, ["pdm", "physical", "dpi", "x"])
  , (10, ["pdm", "physical", "dpi", "y"])
  , (11, ["pdm", "device", "pixel", "ratio"])
  , (12, ["pdm", "device", "pixel", "ratio", "scaled"])
  ]
