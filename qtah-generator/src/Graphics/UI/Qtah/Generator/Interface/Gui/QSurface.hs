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

module Graphics.UI.Qtah.Generator.Interface.Gui.QSurface (
  aModule,
  c_QSurface,
  e_SurfaceType,
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
import Foreign.Hoppy.Generator.Types (boolT, enumT, objT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

minVersion = [5, 0]

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Gui", "QSurface"] minVersion $
  [ QtExport $ ExportClass c_QSurface
  , QtExport $ ExportEnum e_SurfaceClass
  , QtExport $ ExportEnum e_SurfaceType
  ]

c_QSurface =
  addReqIncludes [includeStd "QSurface"] $
  classSetEntityPrefix "" $
  makeClass (ident "QSurface") Nothing [] $
  collect
  [ -- TODO mkConstMethod "surfaceFormat" $ objT c_QSurfaceFormat
    just $ mkConstMethod "size" [] $ objT c_QSize
  , test (qtVersion >= [5, 3]) $ mkConstMethod "supportsOpenGL" [] boolT
  , just $ mkConstMethod "surfaceClass" [] $ enumT e_SurfaceClass
    -- TODO mkConstMethod "surfaceHandle" [] $ ptrT $ objT c_QPlatformSurface
  , just $ mkConstMethod "surfaceType" [] $ enumT e_SurfaceType
  ]

e_SurfaceClass =
  makeQtEnum (ident1 "QSurface" "SurfaceClass") [includeStd "QSurface"]
  [ (0, ["window"])
  , (1, ["offscreen"])
  ]

e_SurfaceType =
  makeQtEnum (ident1 "QSurface" "SurfaceType") [includeStd "QSurface"]
  [ (0, ["raster", "surface"])
  , (1, ["open", "g", "l", "surface"])
  , (2, ["raster", "g", "l", "surface"])
  ]
