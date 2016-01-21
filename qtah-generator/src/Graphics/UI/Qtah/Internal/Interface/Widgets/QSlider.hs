-- This file is part of Qtah.
--
-- Copyright 2015-2016 Bryan Gardiner <bog@khumba.net>
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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QSlider (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass, ExportEnum),
  Type (TEnum, TInt, TObj, TPtr),
  addReqIncludes,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkCtor,
  mkProp,
  mkProps,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.Types (e_Orientation)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractSlider (c_QAbstractSlider)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QSlider"]
  [ QtExport $ ExportClass c_QSlider
  , QtExport $ ExportEnum e_TickPosition
  ]

c_QSlider =
  addReqIncludes [includeStd "QSlider"] $
  makeClass (ident "QSlider") Nothing [c_QAbstractSlider]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  , mkCtor "newWithOrientation" [TEnum e_Orientation]
  , mkCtor "newWithOrientationAndParent" [TEnum e_Orientation, TPtr $ TObj c_QWidget]
  ] $
  mkProps
  [ mkProp "tickInterval" TInt
  , mkProp "tickPosition" $ TEnum e_TickPosition
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
