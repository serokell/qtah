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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QActionGroup (
  aModule,
  c_QActionGroup,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkBoolIsProp,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkMethod',
  mkProps,
  )
import Foreign.Hoppy.Generator.Types (boolT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Listener (c_ListenerPtrQAction)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QAction (c_QAction)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QActionGroup"] $
  QtExport (ExportClass c_QActionGroup) :
  map QtExportSignal signals

c_QActionGroup =
  addReqIncludes [includeStd "QActionGroup"] $
  classSetEntityPrefix "" $
  makeClass (ident "QActionGroup") Nothing
  [ c_QObject ]
  [ mkCtor "new" [ptrT $ objT c_QObject]
  ] $
  [ -- TODO actions
    mkMethod' "addAction" "addAction" [ptrT $ objT c_QAction] $ ptrT $ objT c_QAction
  , mkMethod' "addAction" "addNewAction" [objT c_QString] $ ptrT $ objT c_QAction
    -- TODO addNewActionWithIcon
  , mkConstMethod "checkedAction" [] $ ptrT $ objT c_QAction
  , mkMethod "removeAction" [ptrT $ objT c_QAction] voidT
  , mkMethod "setDisabled" [boolT] voidT
  ] ++
  mkProps
  [ mkBoolIsProp "enabled"
  , mkBoolIsProp "exclusive"
  , mkBoolIsProp "visible"
  ]

signals =
  [ makeSignal c_QActionGroup "hovered" c_ListenerPtrQAction
  , makeSignal c_QActionGroup "triggered" c_ListenerPtrQAction
  ]
