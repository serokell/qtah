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

module Graphics.UI.Qtah.Internal.Interface.Core.QCoreApplication (
  aModule,
  c_QCoreApplication,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  MethodApplicability (MStatic),
  Purity (Nonpure),
  Type (TBool, TInt, TObj, TPtr, TVoid),
  addReqIncludes,
  ident,
  ident2,
  includeLocal,
  includeStd,
  makeFnMethod,
  makeClass,
  mkStaticMethod,
  mkStaticMethod',
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QEvent (c_QEvent)
import Graphics.UI.Qtah.Internal.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Internal.Interface.Core.QStringList (c_QStringList)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QCoreApplication"]
  [ QtExport $ ExportClass c_QCoreApplication ]

c_QCoreApplication =
  addReqIncludes [ includeStd "QCoreApplication"
                 , includeLocal "wrap_qcoreapplication.hpp"
                 ] $
  makeClass (ident "QCoreApplication") Nothing [c_QObject]
  [] $
  collect
  [ just $ makeFnMethod (ident2 "qtah" "qcoreapplication" "create") "new" MStatic Nonpure
    [TObj c_QStringList] $ TPtr $ TObj c_QCoreApplication
  , test (qtVersion >= [4, 1]) $ mkStaticMethod "arguments" [] $ TObj c_QStringList
  , just $ mkStaticMethod "exec" [] TVoid
  , just $ mkStaticMethod "exit" [TInt] TVoid
  , just $ mkStaticMethod' "instance" "getInstance" [] $ TPtr $ TObj c_QCoreApplication
  , test (qtVersion >= [4, 3]) $ mkStaticMethod' "postEvent" "postEvent"
    [TPtr $ TObj c_QObject, TPtr $ TObj c_QEvent] TVoid
  , test (qtVersion >= [4, 3]) $ mkStaticMethod' "postEvent" "postEventWithPriority"
    [TPtr $ TObj c_QObject, TPtr $ TObj c_QEvent, TInt] TVoid
  , just $ mkStaticMethod "quit" [] TVoid
  , just $ mkStaticMethod "sendEvent" [TPtr $ TObj c_QObject, TPtr $ TObj c_QEvent] TBool
    -- TODO Other methods.
  ]
