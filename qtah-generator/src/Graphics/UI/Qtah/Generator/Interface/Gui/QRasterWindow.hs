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

module Graphics.UI.Qtah.Generator.Interface.Gui.QRasterWindow (
  minVersion,
  aModule,
  c_QRasterWindow,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkCtor,
  )
import Foreign.Hoppy.Generator.Types (objT, ptrT)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPaintDeviceWindow (c_QPaintDeviceWindow)
import Graphics.UI.Qtah.Generator.Interface.Gui.QWindow (c_QWindow)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

minVersion = [5, 4]

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Gui", "QRasterWindow"] minVersion $
  [ QtExport $ ExportClass c_QRasterWindow ]

c_QRasterWindow =
  addReqIncludes [includeStd "QRasterWindow"] $
  classSetEntityPrefix "" $
  makeClass (ident "QRasterWindow") Nothing [c_QPaintDeviceWindow]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [ptrT $ objT c_QWindow]
  ]
