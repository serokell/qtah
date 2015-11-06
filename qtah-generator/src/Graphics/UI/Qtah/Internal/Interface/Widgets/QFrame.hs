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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QFrame (
  aModule,
  c_QFrame,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportEnum, ExportClass),
  Type (TEnum, TInt, TObj, TPtr),
  addReqIncludes,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkProp,
  mkProps,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

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
  makeClass (ident "QFrame") Nothing [c_QWidget]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
    -- TODO QFrame(QWidget*, Qt::WindowFlags)
  ] $
  [ mkConstMethod "frameWidth" [] TInt
  ] ++
  mkProps
  [ mkProp "frameRect" $ TObj c_QRect
  , mkProp "frameShadow" $ TEnum e_Shadow
  , mkProp "frameShape" $ TEnum e_Shape
  , mkProp "frameStyle" TInt
  , mkProp "lineWidth" TInt
  , mkProp "midLineWidth" TInt
  ]

e_Shadow =
  makeQtEnum (ident1 "QFrame" "Shadow")
  [ (0x0010, ["plain"])
  , (0x0020, ["raised"])
  , (0x0030, ["sunken"])
  ]

e_Shape =
  makeQtEnum (ident1 "QFrame" "Shape")
  [ (0x0000, ["no", "frame"])
  , (0x0001, ["box"])
  , (0x0002, ["panel"])
  , (0x0003, ["win", "panel"])
  , (0x0004, ["h", "line"])
  , (0x0005, ["v", "line"])
  , (0x0006, ["styled", "panel"])
  ]

e_StyleMask =
  makeQtEnum (ident1 "QFrame" "StyleMask")
  [ (0x000f, ["shape", "mask"])
  , (0x00f0, ["shadow", "mask"])
  ]
