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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QSizePolicy (
  aModule,
  c_QSizePolicy,
  e_Policy,
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
  mkBoolHasProp,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkProp,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (bitspaceT, boolT, enumT, intT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (bs_Orientations)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QSizePolicy"] $
  collect
  [ just $ QtExport $ ExportClass c_QSizePolicy
  , test (qtVersion >= [4, 3]) $ QtExport $ ExportEnum e_ControlType
  , test (qtVersion >= [4, 3]) $ QtExport $ ExportBitspace bs_ControlTypes
  , just $ QtExport $ ExportEnum e_Policy
  , just $ QtExport $ ExportEnum e_PolicyFlag
  ]

c_QSizePolicy =
  addReqIncludes [includeStd "QSizePolicy"] $
  classSetConversionToGc $
  classAddFeatures [Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QSizePolicy") Nothing [] $
  collect
  [ just $ mkCtor "new" []
  , test (qtVersion >= [4, 3]) $ mkCtor "newWithOptions"
    [enumT e_Policy, enumT e_Policy, enumT e_ControlType]
  , test (qtVersion >= [4, 3]) $ mkProp "controlType" $ enumT e_ControlType
  , just $ mkConstMethod "expandingDirections" [] $ bitspaceT bs_Orientations
  , just $ mkBoolHasProp "heightForWidth"
  , just $ mkBoolHasProp "widthForHeight"
  , just $ mkProp "horizontalPolicy" $ enumT e_Policy
  , just $ mkProp "horizontalStretch" intT
  , just $ mkProp "verticalPolicy" $ enumT e_Policy
  , just $ mkProp "verticalStretch" intT
  , test (qtVersion >= [5, 2]) $ mkProp "retainSizeWhenHidden" boolT
  , just $ mkMethod "transpose" [] voidT
  ]

(e_ControlType, bs_ControlTypes) =
  makeQtEnumBitspace (ident1 "QSizePolicy" "ControlType") "ControlTypes"
  [includeStd "QSizePolicy"]
  [ (0x00000001, ["default", "type"])
  , (0x00000002, ["button", "box"])
  , (0x00000004, ["check", "box"])
  , (0x00000008, ["combo", "box"])
  , (0x00000010, ["frame"])
  , (0x00000020, ["group", "box"])
  , (0x00000040, ["label"])
  , (0x00000080, ["line"])
  , (0x00000100, ["line", "edit"])
  , (0x00000200, ["push", "button"])
  , (0x00000400, ["radio", "button"])
  , (0x00000800, ["slider"])
  , (0x00001000, ["spin", "box"])
  , (0x00002000, ["tab", "widget"])
  , (0x00004000, ["tool", "button"])
  ]

e_Policy =
  makeQtEnum (ident1 "QSizePolicy" "Policy") [includeStd "QSizePolicy"]
  [ (0x0, ["fixed"])
  , (0x1, ["minimum"])  -- GrowFlag
  , (0x4, ["maximum"])  -- ShrinkFlag
  , (0x5, ["preferred"])  -- GrowFlag | ShrinkFlag
  , (0x7, ["expanding"])  -- GrowFlag | ShrinkFlag | ExpandFlag
  , (0x3, ["minimum", "expanding"])  -- GrowFlag | ExpandFlag
  , (0xd, ["ignored"])  -- ShrinkFlag | GrowFlag | IgnoreFlag
  ]

e_PolicyFlag =
  makeQtEnum (ident1 "QSizePolicy" "PolicyFlag") [includeStd "QSizePolicy"]
  [ (0x1, ["grow", "flag"])
  , (0x2, ["expand", "flag"])
  , (0x4, ["shrink", "flag"])
  , (0x8, ["ignore", "flag"])
  ]
