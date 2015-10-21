-- This file is part of Qtah.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License version 3
-- as published by the Free Software Foundation.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Graphics.UI.Qtah.Internal.Interface.Core.QCoreApplication (
  hoppyModule,
  qtModule,
  c_QCoreApplication,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  ident,
  includeStd,
  makeClass,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QObject (c_QObject)

{-# ANN module "HLint: ignore Use camelCase" #-}

hoppyModule = makeHoppyModule "Core" "QCoreApplication" qtModule

qtModule =
  makeQtModule "Core.QCoreApplication"
  [ QtExport $ ExportClass c_QCoreApplication ]

c_QCoreApplication =
  addReqIncludes [includeStd "QCoreApplication"] $
  makeClass (ident "QCoreApplication") Nothing [c_QObject] [] []
