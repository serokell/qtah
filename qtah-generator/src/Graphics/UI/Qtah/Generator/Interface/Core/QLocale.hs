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

module Graphics.UI.Qtah.Generator.Interface.Core.QLocale (
  aModule,
  c_QLocale,
  e_NumberOption,
  bs_NumberOptions
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportBitspace, ExportClass, ExportEnum),
  addReqIncludes,
  classSetConversionToGc,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkCtor,
  mkProp
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (bitspaceT, objT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QLocale"]
  [ QtExport $ ExportClass c_QLocale
  , QtExport $ ExportEnum e_NumberOption
  , QtExport $ ExportBitspace bs_NumberOptions
  ]

c_QLocale =
  addReqIncludes [includeStd "QLocale"] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QLocale") Nothing [] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithName" [objT c_QString]
  , just $ mkProp "numberOptions" $ bitspaceT bs_NumberOptions
  -- TODO The rest of QLocale
  ]

(e_NumberOption, bs_NumberOptions) =
  makeQtEnumBitspace (ident1 "QLocale" "NumberOption") "NumberOptions" [includeStd "QLocale"]
  [ (0x00, ["default", "number", "options"])
  , (0x01, ["omit", "group", "separator"])
  , (0x02, ["reject", "group", "separator"])
  , (0x04, ["omit", "leading", "zero", "in", "exponent"])
  , (0x08, ["reject", "leading", "zero", "in", "exponent"])
  , (0x10, ["include", "trailing", "zeroes", "after", "dot"])
  , (0x20, ["reject", "trailing", "zeroes", "after", "dot"])
  ]
