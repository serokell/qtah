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

module Graphics.UI.Qtah.Generator.Interface.Core.QVariant (
  aModule,
  c_QVariant,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportEnum, ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  classSetConversionToGc,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkMethod',
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Comparable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (
  boolT,
  doubleT,
  enumT,
  floatT,
  intT,
  llongT,
  objT,
  refT,
  uintT,
  ullongT,
  voidT,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Interface.Core.QChar (c_QChar)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QList (c_QListQVariant)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QPointF (c_QPointF)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QRectF (c_QRectF)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.QSizeF (c_QSizeF)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QStringList (c_QStringList)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QVariant"]
  [ QtExport $ ExportClass c_QVariant
  , QtExport $ ExportEnum e_Type
  ]

-- TODO Many more data types needed here.
c_QVariant =
  addReqIncludes [includeStd "QVariant"] $
  classSetConversionToGc $
  classAddFeatures (collect
                    [ just Assignable
                    , test (qtVersion >= [5, 0]) Comparable
                    , just Copyable
                    , just Equatable
                    ]) $
  classSetEntityPrefix "" $
  makeClass (ident "QVariant") Nothing [] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithBool" [boolT]
  , just $ mkCtor "newWithByteArray" [objT c_QByteArray]
  , just $ mkCtor "newWithChar" [objT c_QChar]
  , just $ mkCtor "newWithDouble" [doubleT]
  , test (qtVersion >= [4, 6]) $ mkCtor "newWithFloat" [floatT]
    -- OMIT Qt::GlobalColor support.  Per the Qt5 headers (qvariant.h as of Qt
    -- 5.6), it's been deleted because it unexpectedly creates a QVariant of
    -- type int instead of the enum type.
  , just $ mkCtor "newWithInt" [intT]
  , just $ mkCtor "newWithList" [objT c_QListQVariant]
  , just $ mkCtor "newWithLongLong" [llongT]
  , just $ mkCtor "newWithPointF" [objT c_QPointF]
  , just $ mkCtor "newWithPoint" [objT c_QPoint]
  , just $ mkCtor "newWithRectF" [objT c_QRectF]
  , just $ mkCtor "newWithRect" [objT c_QRect]
  , just $ mkCtor "newWithSizeF" [objT c_QSizeF]
  , just $ mkCtor "newWithSize" [objT c_QSize]
  , just $ mkCtor "newWithString" [objT c_QString]
  , just $ mkCtor "newWithStringList" [objT c_QStringList]
  , just $ mkCtor "newWithType" [enumT e_Type]
  , just $ mkCtor "newWithUInt" [uintT]
  , just $ mkCtor "newWithULongLong" [ullongT]
  , just $ mkConstMethod "canConvert" [enumT e_Type] boolT
  , just $ mkMethod "clear" [] voidT
  , just $ mkMethod "convert" [enumT e_Type] boolT
  , just $ mkConstMethod "isNull" [] boolT
  , just $ mkConstMethod "isValid" [] boolT
  , just $ mkMethod' "setValue" "setToBool" [boolT] voidT
  , just $ mkMethod' "setValue" "setToChar" [objT c_QChar] voidT
  , just $ mkMethod' "setValue" "setToDouble" [doubleT] voidT
  , test (qtVersion >= [4, 6]) $ mkMethod' "setValue" "setToFloat" [floatT] voidT
  , just $ mkMethod' "setValue" "setToInt" [intT] voidT
  , just $ mkMethod' "setValue" "setToList" [objT c_QListQVariant] voidT
  , just $ mkMethod' "setValue" "setToLongLong" [llongT] voidT
  , just $ mkMethod' "setValue" "setToPoint" [objT c_QPoint] voidT
  , just $ mkMethod' "setValue" "setToPointF" [objT c_QPointF] voidT
  , just $ mkMethod' "setValue" "setToRect" [objT c_QRect] voidT
  , just $ mkMethod' "setValue" "setToRectF" [objT c_QRectF] voidT
  , just $ mkMethod' "setValue" "setToSize" [objT c_QSize] voidT
  , just $ mkMethod' "setValue" "setToSizeF" [objT c_QSizeF] voidT
  , just $ mkMethod' "setValue" "setToStringList" [objT c_QStringList] voidT
  , just $ mkMethod' "setValue" "setToString" [objT c_QString] voidT
  , just $ mkMethod' "setValue" "setToUInt" [uintT] voidT
  , just $ mkMethod' "setValue" "setToULongLong" [ullongT] voidT
  , test (qtVersion >= [4, 8]) $ mkMethod' "setValue" "swap" [refT $ objT c_QVariant] voidT
  , just $ mkConstMethod "toBool" [] boolT
  , just $ mkConstMethod "toByteArray" [] (objT c_QByteArray)
  , just $ mkConstMethod "toChar" [] $ objT c_QChar
  , just $ mkConstMethod "toDouble" [] doubleT
  , test (qtVersion >= [4, 6]) $ mkConstMethod "toFloat" [] floatT
  , just $ mkConstMethod "toInt" [] intT
  , just $ mkConstMethod "toLongLong" [] llongT
  , just $ mkConstMethod "toPoint" [] $ objT c_QPoint
  , just $ mkConstMethod "toPointF" [] $ objT c_QPointF
  , just $ mkConstMethod "toRect" [] $ objT c_QRect
  , just $ mkConstMethod "toRectF" [] $ objT c_QRectF
  , just $ mkConstMethod "toSize" [] $ objT c_QSize
  , just $ mkConstMethod "toSizeF" [] $ objT c_QSizeF
  , just $ mkConstMethod "toStringList" [] $ objT c_QStringList
  , just $ mkConstMethod "toString" [] $ objT c_QString
  , just $ mkConstMethod "toUInt" [] uintT
  , just $ mkConstMethod "toULongLong" [] ullongT
  , -- Have to rename this, because "type" is reserved in Haskell.
    just $ mkConstMethod' "type" "getType" [] $ enumT e_Type
  , just $ mkConstMethod "userType" [] intT
  ]

e_Type =
  makeQtEnum (ident1 "QVariant" "Type") [includeStd "QVariant"]
  [ (0, ["invalid"])
  , (13, ["bit", "array"])
  , (73, ["bitmap"])
  , (1, ["bool"])
  , (66, ["brush"])
  , (12, ["byte", "array"])
  , (7, ["char"])
  , (67, ["color"])
  , (74, ["cursor"])
  , (14, ["date"])
  , (16, ["date", "time"])
  , (6, ["double"])
  , (29, ["easing", "curve"])
  , (64, ["font"])
  , (28, ["hash"])
  , (69, ["icon"])
  , (70, ["image"])
  , (2, ["int"])
  , (76, ["key", "sequence"])
  , (23, ["line"])
  , (24, ["line", "f"])
  , (9, ["list"])
  , (18, ["locale"])
  , (4, ["long", "long"])
  , (8, ["map"])
  , (80, ["matrix"])
  , (81, ["transform"])
  , (82, ["matrix", "4x4"])
  , (68, ["palette"])
  , (77, ["pen"])
  , (65, ["pixmap"])
  , (25, ["point"])
    -- PointArray omitted -- same value as Polygon.
  , (26, ["point", "f"])
  , (71, ["polygon"])
  , (86, ["quaternion"])
  , (19, ["rect"])
  , (20, ["rect", "f"])
  , (27, ["reg", "exp"])
  , (72, ["region"])
  , (21, ["size"])
  , (22, ["size", "f"])
  , (75, ["size", "policy"])
  , (10, ["string"])
  , (11, ["string", "list"])
  , (79, ["text", "format"])
  , (78, ["text", "length"])
  , (15, ["time"])
  , (3, ["u", "int"])
  , (5, ["u", "long", "long"])
  , (17, ["url"])
  , (83, ["vector", "2d"])
  , (84, ["vector", "3d"])
  , (85, ["vector", "4d"])
  , (127, ["user", "type"])
  ]
