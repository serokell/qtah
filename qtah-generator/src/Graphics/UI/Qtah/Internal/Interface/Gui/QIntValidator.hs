-- This file is part of Qtah.
--
-- Copyright 2016 Bryan Gardiner <bog@khumba.net>
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

module Graphics.UI.Qtah.Internal.Interface.Gui.QIntValidator (
  aModule,
  c_QIntValidator,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Type (TInt, TObj, TPtr, TVoid),
  addReqIncludes,
  ident,
  includeStd,
  makeClass,
  mkCtor,
  mkMethod,
  mkProp,
  mkProps,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Internal.Interface.Gui.QValidator (c_QValidator)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QIntValidator"] $
  [ QtExport $ ExportClass c_QIntValidator ]

c_QIntValidator =
  addReqIncludes [includeStd "QIntValidator"] $
  makeClass (ident "QIntValidator") Nothing [c_QValidator]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QObject]
  , mkCtor "newWithOptions" [TInt, TInt]
  , mkCtor "newWithOptionsAndParent" [TInt, TInt, TPtr $ TObj c_QObject]
  ] $
  [ mkMethod "setRange" [TInt, TInt] TVoid
  ] ++
  mkProps
  [ mkProp "bottom" TInt
  , mkProp "top" TInt
  ]
