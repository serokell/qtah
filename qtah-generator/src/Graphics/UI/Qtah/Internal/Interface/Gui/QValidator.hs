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

module Graphics.UI.Qtah.Internal.Interface.Gui.QValidator (
  aModule,
  c_QValidator,
  e_State,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportEnum, ExportClass),
  Type (TEnum, TInt, TObj, TRef, TVoid),
  addReqIncludes,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Listener (c_Listener)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QValidator"] $
  (QtExport $ ExportClass c_QValidator) :
  map QtExportSignal signals ++
  [ QtExport $ ExportEnum e_State ]

c_QValidator =
  addReqIncludes [includeStd "QValidator"] $
  makeClass (ident "QValidator") Nothing [c_QObject] [] $
  [ mkConstMethod "fixup" [TRef $ TObj c_QString] TVoid
    -- TODO locale
    -- TODO setLocale
  , mkConstMethod "validate" [TRef $ TObj c_QString, TRef TInt] $ TEnum e_State
  ]

signals =
  [ makeSignal c_QValidator "changed" c_Listener
  ]

e_State =
  makeQtEnum (ident1 "QValidator" "State") [includeStd "QValidator"]
  [ (0, ["invalid"])
  , (1, ["intermediate"])
  , (2, ["acceptable"])
  ]
