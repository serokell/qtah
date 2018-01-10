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

module Graphics.UI.Qtah.Generator.Interface.Gui.QPaintDeviceWindow (
  aModule,
  c_QPaintDeviceWindow,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkMethod',
  )
import Foreign.Hoppy.Generator.Types (objT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPaintDevice (c_QPaintDevice)
import Graphics.UI.Qtah.Generator.Interface.Gui.QRegion (c_QRegion)
import Graphics.UI.Qtah.Generator.Interface.Gui.QWindow (c_QWindow)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

minVersion = [5, 4]

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Gui", "QPaintDeviceWindow"] minVersion
  [ QtExport $ ExportClass c_QPaintDeviceWindow
  ]

c_QPaintDeviceWindow =
  addReqIncludes [includeStd "QPaintDeviceWindow"] $
  classSetEntityPrefix "" $
  makeClass (ident "QPaintDeviceWindow") Nothing [c_QWindow, c_QPaintDevice]
  [ mkMethod' "update" "update" [] voidT
  , mkMethod' "update" "updateRect" [objT c_QRect] voidT
  , mkMethod' "update" "updateRegion" [objT c_QRegion] voidT
  ]
