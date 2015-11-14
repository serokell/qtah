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

module Graphics.UI.Qtah.Internal.Interface.Core.QCoreApplication (
  aModule,
  c_QCoreApplication,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Type (TObj),
  addReqIncludes,
  ident,
  includeStd,
  makeClass,
  mkStaticMethod,
  )
import Foreign.Hoppy.Generator.Version (collect, test)
import Graphics.UI.Qtah.Internal.Flags (qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Internal.Interface.Core.QStringList (c_QStringList)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QCoreApplication"]
  [ QtExport $ ExportClass c_QCoreApplication ]

c_QCoreApplication =
  addReqIncludes [includeStd "QCoreApplication"] $
  makeClass (ident "QCoreApplication") Nothing [c_QObject]
  [] $
  collect
  [ test (qtVersion >= [4, 1]) $ mkStaticMethod "arguments" [] $ TObj c_QStringList
  ]
