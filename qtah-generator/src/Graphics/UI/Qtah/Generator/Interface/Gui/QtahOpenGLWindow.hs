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

module Graphics.UI.Qtah.Generator.Interface.Gui.QtahOpenGLWindow (
  aModule,
  c_QtahOpenGLWindow,
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
import Foreign.Hoppy.Generator.Types (callbackT, enumT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Gui.QOpenGLWindow (
  c_QOpenGLWindow,
  e_UpdateBehavior,
  minVersion,
  )
import Graphics.UI.Qtah.Generator.Interface.Gui.QWindow (c_QWindow)
import Graphics.UI.Qtah.Generator.Interface.Internal.Callback (cb_Void, cb_IntIntVoid)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Gui", "QtahOpenGLWindow"] minVersion $
  [ QtExport $ ExportClass c_QtahOpenGLWindow ]

c_QtahOpenGLWindow =
  addReqIncludes [includeLocal "qtahopenglwindow.hpp"] $
  classSetEntityPrefix "" $
  makeClass (ident2 "qtah" "qtahopenglwindow" "QtahOpenGLWindow") Nothing [c_QOpenGLWindow]
  [ mkCtor "new" []
  , mkCtor "newWithUpdateBehavior" [enumT e_UpdateBehavior]
  , mkCtor "newWithUpdateBehaviorAndParent" [enumT e_UpdateBehavior, ptrT $ objT c_QWindow]
    -- TODO QtahOpenGLWindow(QOpenGLContext*, ...)
    -- TODO QOpenGLContext* context() const
  , mkMethod "onInitializeGL" [callbackT cb_Void] voidT
  , mkMethod "onPaintGL" [callbackT cb_Void] voidT
  , mkMethod "onPaintOverGL" [callbackT cb_Void] voidT
  , mkMethod "onPaintUnderGL" [callbackT cb_Void] voidT
  , mkMethod "onResizeGL" [callbackT cb_IntIntVoid] voidT
  ]
