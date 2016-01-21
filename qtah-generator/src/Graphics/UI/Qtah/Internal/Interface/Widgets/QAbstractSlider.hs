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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractSlider (
  aModule,
  c_QAbstractSlider,
  e_SliderAction,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass, ExportEnum),
  Type (TBool, TEnum, TInt, TObj, TPtr, TVoid),
  addReqIncludes,
  ident,
  ident1,
  includeStd,
  makeClass,
  makeEnum,
  mkBoolIsProp,
  mkBoolHasProp,
  mkCtor,
  mkMethod,
  mkProp,
  mkProps,
  toExtName,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.Types (e_Orientation)
import Graphics.UI.Qtah.Internal.Interface.Listener (
  c_Listener,
  c_ListenerInt,
  c_ListenerIntInt,
  c_ListenerQAbstractSliderAction,
  )
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QAbstractSlider"] $
  QtExport (ExportClass c_QAbstractSlider) :
  map QtExportSignal signals ++
  [ QtExport $ ExportEnum e_SliderAction ]

c_QAbstractSlider =
  addReqIncludes [includeStd "QAbstractSlider"] $
  makeClass (ident "QAbstractSlider") Nothing [c_QWidget]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ] $
  [ mkMethod "triggerAction" [TEnum e_SliderAction] TVoid
  ] ++
  mkProps
  [ mkProp "invertedAppearance" TBool
  , mkProp "invertedControls" TBool
  , mkProp "maximum" TInt
  , mkProp "minimum" TInt
  , mkProp "orientation" $ TEnum e_Orientation
  , mkProp "pageStep" TInt
  , mkProp "singleStep" TInt
  , mkBoolIsProp "sliderDown"
  , mkProp "sliderPosition" TInt
  , mkBoolHasProp "tracking"
  , mkProp "value" TInt
  ]

signals =
  [ makeSignal c_QAbstractSlider "actionTriggered" c_ListenerQAbstractSliderAction
  , makeSignal c_QAbstractSlider "rangeChanged" c_ListenerIntInt
  , makeSignal c_QAbstractSlider "sliderMoved" c_ListenerInt
  , makeSignal c_QAbstractSlider "sliderPressed" c_Listener
  , makeSignal c_QAbstractSlider "sliderReleased" c_Listener
  , makeSignal c_QAbstractSlider "valueChanged" c_ListenerInt
  ]

-- This uses 'makeEnum' rather than 'makeQtEnum' and also drops a "Slider"
-- prefix off of the start of each value name, unlike most Qt enums, because
-- 'QAbstractSliderSliderAction_SliderNoAction' is a tad too repetetive.
e_SliderAction =
  addReqIncludes [includeStd "QAbstractSlider"] $
  makeEnum (ident1 "QAbstractSlider" "SliderAction") (Just $ toExtName "QAbstractSliderAction")
  [ (0, ["no", "action"])
  , (1, ["single", "step", "add"])
  , (2, ["single", "step", "sub"])
  , (3, ["page", "step", "add"])
  , (4, ["page", "step", "sub"])
  , (5, ["to", "minimum"])
  , (6, ["to", "maximum"])
  , (7, ["move"])
  ]
