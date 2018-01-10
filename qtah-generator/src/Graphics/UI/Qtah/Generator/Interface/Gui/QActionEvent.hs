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

module Graphics.UI.Qtah.Generator.Interface.Gui.QActionEvent (
  aModule,
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
import Foreign.Hoppy.Generator.Types (enumT, objT, ptrT)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAction (c_QAction)
import Graphics.UI.Qtah.Generator.Interface.Core.QEvent (c_QEvent, e_Type)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QActionEvent"]
  [ QtExportEvent c_QActionEvent
  ]

c_QActionEvent =
  addReqIncludes [includeStd "QActionEvent"] $
  classSetEntityPrefix "" $
  makeClass (ident "QActionEvent") Nothing [c_QEvent]
  [ mkCtor "new" [enumT e_Type, ptrT $ objT c_QAction]
  , mkCtor "newBefore" [enumT e_Type, ptrT $ objT c_QAction]
  , mkConstMethod "action" [] $ ptrT $ objT c_QAction
  , mkConstMethod "before" [] $ ptrT $ objT c_QAction
  ]
