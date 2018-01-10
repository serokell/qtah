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

module Graphics.UI.Qtah.Generator.Interface.Gui.QOpenGLWindow (
  minVersion,
  aModule,
  c_QOpenGLWindow,
  e_UpdateBehavior,
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
  mkCtor,
  mkMethod,
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (gluint)
import Graphics.UI.Qtah.Generator.Interface.Gui.QImage (c_QImage)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPaintDeviceWindow (c_QPaintDeviceWindow)
import Graphics.UI.Qtah.Generator.Interface.Gui.QWindow (c_QWindow)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (c_Listener)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

minVersion = [5, 4]

aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Gui", "QOpenGLWindow"] minVersion $
  [ QtExport $ ExportClass c_QOpenGLWindow ] ++
  map QtExportSignal signals ++
  [ QtExport $ ExportEnum e_UpdateBehavior ]

c_QOpenGLWindow =
  addReqIncludes [includeStd "QOpenGLWindow"] $
  classSetEntityPrefix "" $
  makeClass (ident "QOpenGLWindow") Nothing [c_QPaintDeviceWindow]
  [ mkCtor "new" []
  , mkCtor "newWithUpdateBehavior" [enumT e_UpdateBehavior]
  , mkCtor "newWithUpdateBehaviorAndParent" [enumT e_UpdateBehavior, ptrT $ objT c_QWindow]
    -- TODO QOpenGLWindow(QOpenGLContext*, ...)
    -- TODO QOpenGLContext* context() const
  , mkConstMethod "defaultFramebufferObject" [] gluint
  , mkMethod "doneCurrent" [] voidT
  , mkMethod "grabFramebuffer" [] $ objT c_QImage
  , mkConstMethod "isValid" [] boolT
  , mkMethod "makeCurrent" [] voidT
    -- TODO QOpenGLContext* shareContext() const
  , mkConstMethod "updateBehavior" [] $ enumT e_UpdateBehavior
  ]

signals =
  [ makeSignal c_QOpenGLWindow "frameSwapped" c_Listener
  ]

e_UpdateBehavior =
  makeQtEnum (ident1 "QOpenGLWindow" "UpdateBehavior") [includeStd "QOpenGLWindow"]
  [ (0, ["no", "partial", "update"])
  , (1, ["partial", "update", "blit"])
  , (2, ["partial", "update", "blend"])
  ]
