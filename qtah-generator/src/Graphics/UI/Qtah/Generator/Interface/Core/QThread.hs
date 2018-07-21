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

module Graphics.UI.Qtah.Generator.Interface.Core.QThread (
  aModule,
  c_QThread,
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
  mkMethod',
  mkProp,
  mkStaticMethod,
  )
import Foreign.Hoppy.Generator.Types (boolT, enumT, intT, objT, ptrT, uintT, ulongT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (c_Listener)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QThread"] $
  QtExport (ExportClass c_QThread) :
  map QtExportSignal signals ++
  [ QtExport $ ExportEnum e_Priority
  ]

c_QThread =
  addReqIncludes [includeStd "QThread"] $
  classSetEntityPrefix "" $
  makeClass (ident "QThread") Nothing [c_QObject] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QObject]
    -- TODO eventDispatcher property
  , just $ mkMethod' "exit" "exit" [] voidT
  , just $ mkMethod' "exit" "exitWithCode" [intT] voidT
  , just $ mkConstMethod "isFinished" [] boolT
  , test (qtVersion >= [5, 2]) $ mkConstMethod "isInterruptionRequested" [] boolT
  , just $ mkConstMethod "isRunning" [] boolT
  , test (qtVersion >= [5, 5]) $ mkConstMethod "loopLevel" [] intT
  , just $ mkProp "priority" $ enumT e_Priority
  , just $ mkMethod "quit" [] voidT
  , test (qtVersion >= [5, 2]) $ mkMethod "requestInterruption" [] voidT
  , just $ mkProp "stackSize" uintT
  , just $ mkMethod' "start" "start" [] voidT
  , just $ mkMethod' "start" "startWithPriority" [enumT e_Priority] voidT
  , just $ mkMethod "terminate" [] voidT
  , just $ mkMethod' "wait" "wait" [] voidT
  , just $ mkMethod' "wait" "waitWithMillis" [ulongT] voidT

    -- Static methods.
  , just $ mkStaticMethod "currentThread" [] $ ptrT $ objT c_QThread
    -- TODO currentThreadId
  , just $ mkStaticMethod "idealThreadCount" [] intT
  , just $ mkStaticMethod "msleep" [ulongT] voidT
  , just $ mkStaticMethod "sleep" [ulongT] voidT
  , just $ mkStaticMethod "usleep" [ulongT] voidT
  , just $ mkStaticMethod "yieldCurrentThread" [] voidT
  ]

signals =
  [ makeSignal c_QThread "finished" c_Listener
  , makeSignal c_QThread "started" c_Listener
  ]

e_Priority =
  makeQtEnum (ident1 "QThread" "Priority") [includeStd "QThread"]
  [ (0, ["idle", "priority"])
  , (1, ["lowest", "priority"])
  , (2, ["low", "priority"])
  , (3, ["normal", "priority"])
  , (4, ["high", "priority"])
  , (5, ["highest", "priority"])
  , (6, ["time", "critical", "priority"])
  , (7, ["inherit", "priority"])
  ]
