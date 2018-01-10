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

module Graphics.UI.Qtah.Generator.Interface.Gui.QIntValidator (
  aModule,
  c_QIntValidator,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkCtor,
  mkMethod,
  mkProp,
  )
import Foreign.Hoppy.Generator.Types (intT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Gui.QValidator (c_QValidator)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QIntValidator"] $
  [ QtExport $ ExportClass c_QIntValidator ]

c_QIntValidator =
  addReqIncludes [includeStd "QIntValidator"] $
  classSetEntityPrefix "" $
  makeClass (ident "QIntValidator") Nothing [c_QValidator]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [ptrT $ objT c_QObject]
  , mkCtor "newWithOptions" [intT, intT]
  , mkCtor "newWithOptionsAndParent" [intT, intT, ptrT $ objT c_QObject]
  , mkProp "bottom" intT
  , mkMethod "setRange" [intT, intT] voidT
  , mkProp "top" intT
  ]
