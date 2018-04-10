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

module Graphics.UI.Qtah.Generator.Interface.Core.QCoreApplication (
  aModule,
  c_QCoreApplication,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  MethodApplicability (MStatic),
  Purity (Nonpure),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident2,
  includeLocal,
  includeStd,
  makeFnMethod,
  makeClass,
  mkStaticMethod,
  mkStaticMethod',
  )
import Foreign.Hoppy.Generator.Types (boolT, intT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QEvent (c_QEvent)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QStringList (c_QStringList)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QCoreApplication"]
  [ QtExport $ ExportClass c_QCoreApplication ]

c_QCoreApplication =
  addReqIncludes [ includeStd "QCoreApplication"
                 , includeLocal "wrap_qcoreapplication.hpp"
                 ] $
  classSetEntityPrefix "" $
  makeClass (ident "QCoreApplication") Nothing [c_QObject] $
  collect
  [ just $ makeFnMethod (ident2 "qtah" "qcoreapplication" "create") "new" MStatic Nonpure
    [objT c_QStringList] $ ptrT $ objT c_QCoreApplication
  , just $ mkStaticMethod "applicationName" [] $ objT c_QString
  , just $ mkStaticMethod "applicationVersion" [] $ objT c_QString
  , test (qtVersion >= [4, 1]) $ mkStaticMethod "arguments" [] $ objT c_QStringList
  , just $ mkStaticMethod "exec" [] voidT
  , just $ mkStaticMethod "exit" [intT] voidT
  , just $ mkStaticMethod' "instance" "getInstance" [] $ ptrT $ objT c_QCoreApplication
  , test (qtVersion >= [5, 0]) $ mkStaticMethod "isQuitLockEnabled" [] boolT
  , just $ mkStaticMethod "organizationDomain" [] $ objT c_QString
  , just $ mkStaticMethod "organizationName" [] $ objT c_QString
  , test (qtVersion >= [4, 3]) $ mkStaticMethod' "postEvent" "postEvent"
    [ptrT $ objT c_QObject, ptrT $ objT c_QEvent] voidT
  , test (qtVersion >= [4, 3]) $ mkStaticMethod' "postEvent" "postEventWithPriority"
    [ptrT $ objT c_QObject, ptrT $ objT c_QEvent, intT] voidT
  , just $ mkStaticMethod "quit" [] voidT
  , just $ mkStaticMethod "sendEvent" [ptrT $ objT c_QObject, ptrT $ objT c_QEvent] boolT
  , just $ mkStaticMethod "setApplicationName" [objT c_QString] voidT
  , just $ mkStaticMethod "setApplicationVersion" [objT c_QString] voidT
  , just $ mkStaticMethod "setOrganizationDomain" [objT c_QString] voidT
  , just $ mkStaticMethod "setOrganizationName" [objT c_QString] voidT
  , test (qtVersion >= [5, 0]) $ mkStaticMethod "setQuitLockEnabled" [boolT] voidT
    -- TODO Other methods.
  ]
