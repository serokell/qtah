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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QSpacerItem (
  aModule,
  c_QSpacerItem,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod',
  )
import Foreign.Hoppy.Generator.Types (enumT, intT, objT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QLayoutItem (c_QLayoutItem)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QSizePolicy (c_QSizePolicy, e_Policy)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QSpacerItem"]
  [ QtExport $ ExportClass c_QSpacerItem ]

c_QSpacerItem =
  addReqIncludes [includeStd "QSpacerItem"] $
  classSetEntityPrefix "" $
  makeClass (ident "QSpacerItem") Nothing
  [ c_QLayoutItem ] $
  collect
  [ just $ mkCtor "new" [intT, intT]
  , just $ mkCtor "newWithOptions" [intT, intT, enumT e_Policy, enumT e_Policy]
  , just $ mkMethod' "changeSize" "changeSize" [intT, intT] voidT
  , just $ mkMethod' "changeSize" "changeSizeWithOptions"
    [intT, intT, enumT e_Policy, enumT e_Policy] voidT
  , test (qtVersion >= [5, 5]) $ mkConstMethod "sizePolicy" [] $ objT c_QSizePolicy
  ]
