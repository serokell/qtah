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

module Graphics.UI.Qtah.Generator.Interface.Core.QTimer (
  aModule,
  c_QTimer
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export(ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkMethod,
  mkConstMethod,
  mkCtor,
  )
import Foreign.Hoppy.Generator.Types (boolT, intT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (c_Listener)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QTimer"]
  [ QtExport $ ExportClass c_QTimer
  , QtExportSignal s_timeout
  ]

c_QTimer =
  addReqIncludes [includeStd "QTimer"] $
  classSetEntityPrefix "" $
  makeClass (ident "QTimer") Nothing [c_QObject] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkConstMethod "interval" [] intT
  -- , just $ mkConstMethod "intervalAsDuration" [] $ objT c_std::chrono::milliseconds
  , just $ mkConstMethod "isActive" [] boolT
  , just $ mkConstMethod "isSingleShot" [] boolT
  , test (qtVersion >= [5, 0]) $ mkConstMethod "remainingTime" [] intT
  -- , just $ mkConstMethod "remainingTimeAsDuration" [] $ objT c_std::chrono::milliseconds
  , just $ mkMethod "setInterval" [intT] voidT
  -- , just $ mkMethod' "setInterval" "setInterval" [objT c_std::chrono::milliseconds] voidT
  , just $ mkMethod "setSingleShot" [boolT] voidT
  -- , just $ mkMethod "setTimerType" [objT c_Qt::TimerType] voidT
  , just $ mkMethod "start" [intT] voidT
  , just $ mkConstMethod "timerId" [] intT
  -- , just $ mkConstMethod "timerType" [] $ objT c_Qt::TimerType
  ]

s_timeout = makeSignal c_QTimer "timeout" c_Listener
