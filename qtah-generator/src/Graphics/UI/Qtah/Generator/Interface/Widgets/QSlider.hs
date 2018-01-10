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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QSlider (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass, ExportEnum),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkCtor,
  mkProp,
  )
import Foreign.Hoppy.Generator.Types (enumT, intT, objT, ptrT)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_Orientation)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractSlider (c_QAbstractSlider)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QSlider"]
  [ QtExport $ ExportClass c_QSlider
  , QtExport $ ExportEnum e_TickPosition
  ]

c_QSlider =
  addReqIncludes [includeStd "QSlider"] $
  classSetEntityPrefix "" $
  makeClass (ident "QSlider") Nothing [c_QAbstractSlider]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , mkCtor "newWithOrientation" [enumT e_Orientation]
  , mkCtor "newWithOrientationAndParent" [enumT e_Orientation, ptrT $ objT c_QWidget]
  , mkProp "tickInterval" intT
  , mkProp "tickPosition" $ enumT e_TickPosition
  ]

e_TickPosition =
  makeQtEnum (ident1 "QSlider" "TickPosition") [includeStd "QSlider"]
  [ (0, ["no", "ticks"])
  , (1, ["ticks", "both", "sides"])
  , (2, ["ticks", "above"])
  , (3, ["ticks", "below"])
    -- TicksLeft = TicksAbove
    -- TicksRight = TicksBelow
    --
    -- We can't include these because duplicate enum values cause problems with
    -- Hoppy.
  ]
