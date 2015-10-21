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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QVBoxLayout (
  hoppyModule,
  qtModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Type (TObj, TPtr),
  addReqIncludes,
  ident,
  includeStd,
  makeClass,
  mkCtor,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Widgets.QBoxLayout (c_QBoxLayout)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

hoppyModule = makeHoppyModule "Widgets" "QVBoxLayout" qtModule

qtModule =
  makeQtModule "Widgets.QVBoxLayout"
  [ QtExport $ ExportClass c_QVBoxLayout ]

c_QVBoxLayout =
  addReqIncludes [includeStd "QVBoxLayout"] $
  makeClass (ident "QVBoxLayout") Nothing [c_QBoxLayout]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  ]
  []
