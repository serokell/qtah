-- This file is part of Qtah.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QAbstractButton (
  aModule,
  c_QAbstractButton,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Type (TBool, TInt, TObj, TVoid),
  addReqIncludes,
  ident,
  includeStd,
  makeClass,
  mkBoolIsProp,
  mkMethod,
  mkProp,
  mkProps,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Listener (c_Listener, c_ListenerBool)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QAbstractButton"] $
  QtExport (ExportClass c_QAbstractButton) :
  map QtExportSignal signals

c_QAbstractButton =
  addReqIncludes [includeStd "QAbstractButton"] $
  makeClass (ident "QAbstractButton") Nothing
  [ c_QWidget ]
  [] $  -- Abstact.
  [ mkMethod "animateClick" [TInt] TVoid
  , mkMethod "click" [] TVoid
    -- TODO group
  , mkMethod "toggle" [] TVoid
  ] ++
  mkProps
  [ mkProp "autoExclusive" TBool
  , mkProp "autoRepeat" TBool
  , mkProp "autoRepeatDelay" TInt
  , mkProp "autoRepeatInterval" TInt
  , mkBoolIsProp "checkable"
  , mkBoolIsProp "checked"
  , mkBoolIsProp "down"
    -- TODO icon
  , mkProp "iconSize" $ TObj c_QSize
    -- TODO shortcut
  , mkProp "text" $ TObj c_QString
  ]

signals =
  [ makeSignal c_QAbstractButton "clicked" c_ListenerBool
  , makeSignal c_QAbstractButton "pressed" c_Listener
  , makeSignal c_QAbstractButton "released" c_Listener
  , makeSignal c_QAbstractButton "toggled" c_ListenerBool
  ]
