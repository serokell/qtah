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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QFrame (
  aModule,
  c_QFrame,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportEnum, ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkProp,
  )
import Foreign.Hoppy.Generator.Types (enumT, intT, objT, ptrT)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QFrame"]
  [ QtExport $ ExportClass c_QFrame
  , QtExport $ ExportEnum e_Shadow
  , QtExport $ ExportEnum e_Shape
  , QtExport $ ExportEnum e_StyleMask
  ]

c_QFrame =
  addReqIncludes [includeStd "QFrame"] $
  classSetEntityPrefix "" $
  makeClass (ident "QFrame") Nothing [c_QWidget]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
    -- TODO QFrame(QWidget*, Qt::WindowFlags)
  , mkProp "frameRect" $ objT c_QRect
  , mkProp "frameShadow" $ enumT e_Shadow
  , mkProp "frameShape" $ enumT e_Shape
  , mkProp "frameStyle" intT
  , mkConstMethod "frameWidth" [] intT
  , mkProp "lineWidth" intT
  , mkProp "midLineWidth" intT
  ]

e_Shadow =
  makeQtEnum (ident1 "QFrame" "Shadow") [includeStd "QFrame"]
  [ (0x0010, ["plain"])
  , (0x0020, ["raised"])
  , (0x0030, ["sunken"])
  ]

e_Shape =
  makeQtEnum (ident1 "QFrame" "Shape") [includeStd "QFrame"]
  [ (0x0000, ["no", "frame"])
  , (0x0001, ["box"])
  , (0x0002, ["panel"])
  , (0x0003, ["win", "panel"])
  , (0x0004, ["h", "line"])
  , (0x0005, ["v", "line"])
  , (0x0006, ["styled", "panel"])
  ]

e_StyleMask =
  makeQtEnum (ident1 "QFrame" "StyleMask") [includeStd "QFrame"]
  [ (0x000f, ["shape", "mask"])
  , (0x00f0, ["shadow", "mask"])
  ]
