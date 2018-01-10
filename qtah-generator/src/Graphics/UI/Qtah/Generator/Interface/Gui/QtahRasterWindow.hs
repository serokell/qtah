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

module Graphics.UI.Qtah.Generator.Interface.Gui.QtahRasterWindow (
  aModule,
  c_QtahRasterWindow,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident2,
  includeLocal,
  makeClass,
  mkCtor,
  mkMethod,
  )
import Foreign.Hoppy.Generator.Types (callbackT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Gui.QRasterWindow (c_QRasterWindow, minVersion)
import Graphics.UI.Qtah.Generator.Interface.Gui.QWindow (c_QWindow)
import Graphics.UI.Qtah.Generator.Interface.Internal.Callback (cb_PtrQPaintEventVoid)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Gui", "QtahRasterWindow"] minVersion $
  [ QtExport $ ExportClass c_QtahRasterWindow ]

c_QtahRasterWindow =
  addReqIncludes [includeLocal "qtahrasterwindow.hpp"] $
  classSetEntityPrefix "" $
  makeClass (ident2 "qtah" "qtahrasterwindow" "QtahRasterWindow") Nothing [c_QRasterWindow]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [ptrT $ objT c_QWindow]
  , mkMethod "onPaintEvent" [callbackT cb_PtrQPaintEventVoid] voidT
  ]
