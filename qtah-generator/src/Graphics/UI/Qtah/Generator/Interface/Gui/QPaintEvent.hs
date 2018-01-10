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

module Graphics.UI.Qtah.Generator.Interface.Gui.QPaintEvent (
  aModule,
  c_QPaintEvent,
  ) where

import Foreign.Hoppy.Generator.Spec (
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  )
import Foreign.Hoppy.Generator.Types (constT, objT, refT)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QEvent (c_QEvent)
import Graphics.UI.Qtah.Generator.Interface.Gui.QRegion (c_QRegion)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QPaintEvent"]
  [ QtExportEvent c_QPaintEvent
  ]

c_QPaintEvent =
  addReqIncludes [includeStd "QPaintEvent"] $
  classSetEntityPrefix "" $
  makeClass (ident "QPaintEvent") Nothing [c_QEvent]
  [ mkCtor "newWithRect" [objT c_QRect]
  , mkCtor "newWithRegion" [objT c_QRegion]
  , mkConstMethod "rect" [] $ refT $ constT $ objT c_QRect
  , mkConstMethod "region" [] $ refT $ constT $ objT c_QRegion
  ]
