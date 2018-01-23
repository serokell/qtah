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

module Graphics.UI.Qtah.Generator.Interface.Gui.QImage (
  aModule,
  c_QImage,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass, ExportEnum),
  MethodApplicability (MStatic),
  Purity (Nonpure),
  addReqIncludes,
  classSetConversionToGc,
  classSetEntityPrefix,
  ident,
  ident1,
  ident2,
  includeLocal,
  includeStd,
  makeClass,
  makeFnMethod,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  mkStaticMethod',
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (
  boolT,
  callbackT,
  constT,
  enumT,
  intT,
  objT,
  objToHeapT,
  ptrT,
  refT,
  toGcT,
  ucharT,
  uintT,
  voidT,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QStringList (c_QStringList)
import Graphics.UI.Qtah.Generator.Interface.Core.QVector (c_QVectorQRgb)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (
  e_AspectRatioMode,
  e_GlobalColor,
  e_MaskMode,
  e_TransformationMode,
  qreal,
  )
import Graphics.UI.Qtah.Generator.Interface.Gui.QColor (c_QColor, qrgb)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPaintDevice (c_QPaintDevice)
import Graphics.UI.Qtah.Generator.Interface.Gui.QTransform (c_QTransform)
import Graphics.UI.Qtah.Generator.Interface.Internal.Callback (cb_Void)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QImage"]
  [ QtExport $ ExportClass c_QImage
  , QtExport $ ExportEnum e_Format
  , QtExport $ ExportEnum e_InvertMode
  ]

c_QImage =
  addReqIncludes [ includeStd "QImage"
                 , includeLocal "wrap_qimage.hpp"
                 ] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QImage") Nothing [c_QPaintDevice] $
  collect
  [ -- Lots of constructors here!
    just $ mkCtor "new" []
  , just $ mkCtor "newWithSize" [objT c_QSize, enumT e_Format]
  , just $ mkCtor "newWithSizeRaw" [intT, intT, enumT e_Format]
  , just $ mkCtor "newWithData"
    [ptrT ucharT, intT, intT, enumT e_Format]
  , test (qtVersion >= [5, 0]) $
    makeFnMethod (ident2 "qtah" "qimage" "create") "newWithDataAndCleanup" MStatic Nonpure
    [ptrT ucharT, intT, intT, enumT e_Format, callbackT cb_Void] $ ptrT $ objT c_QImage
  , just $ mkCtor "newWithConstData"
    [ptrT $ constT ucharT, intT, intT, enumT e_Format]
  , test (qtVersion >= [5, 0]) $
    makeFnMethod (ident2 "qtah" "qimage" "create") "newWithConstDataAndCleanup" MStatic Nonpure
    [ptrT $ constT ucharT, intT, intT, enumT e_Format, callbackT cb_Void] $ ptrT $ objT c_QImage
  , just $ mkCtor "newWithDataAndBytesPerLine"
    [ptrT ucharT, intT, intT, enumT e_Format]
  , test (qtVersion >= [5, 0]) $
    makeFnMethod (ident2 "qtah" "qimage" "create") "newWithDataAndBytesPerLineAndCleanup"
    MStatic Nonpure [ptrT ucharT, intT, intT, intT, enumT e_Format, callbackT cb_Void] $
    ptrT $ objT c_QImage
  , just $ mkCtor "newWithConstDataAndBytesPerLine"
    [ptrT $ constT ucharT, intT, intT, enumT e_Format]
  , test (qtVersion >= [5, 0]) $
    makeFnMethod (ident2 "qtah" "qimage" "create") "newWithConstDataAndBytesPerLineAndCleanup"
    MStatic Nonpure [ptrT $ constT ucharT, intT, intT, intT, enumT e_Format, callbackT cb_Void] $
    ptrT $ objT c_QImage
    -- TODO newWithXpm (but nicely)
  , just $ mkCtor "newWithFile" [objT c_QString]
  , just $ makeFnMethod (ident2 "qtah" "qimage" "create") "newWithFileAndFormat" MStatic Nonpure
    [objT c_QString, objT c_QString] $ ptrT $ objT c_QImage

    -- Finally methods.
  , just $ mkConstMethod "allGray" [] boolT
  , just $ mkConstMethod "bitPlaneCount" [] intT
  , just $ mkMethod "bits" [] $ ptrT ucharT
  , just $ mkConstMethod' "bits" "bitsConst" [] $ ptrT $ constT ucharT
  , just $ mkConstMethod "byteCount" [] intT
  , just $ mkConstMethod "bytesPerLine" [] intT
    -- TODO cacheKey
  , just $ mkConstMethod "color" [intT] qrgb
  , just $ mkProp "colorCount" intT
  , just $ mkConstMethod "colorTable" [] $ toGcT $ objT c_QVectorQRgb
    -- OMIT constBits, this is provided by the const version of bits().
  , just $ mkConstMethod "constScanLine" [intT] $ ptrT $ constT ucharT
    -- TODO convertToFormat
  , just $ mkConstMethod' "copy" "copyRect" [objT c_QRect] $ objT c_QImage
  , just $ mkConstMethod' "copy" "copyRaw" [intT, intT, intT, intT] $ objT c_QImage
  , just $ mkConstMethod "createAlphaMask" [] $ objT c_QImage  -- TODO Overload.
  , just $ mkConstMethod' "createHeuristicMask" "createHeuristicMask" [] $ objT c_QImage
  , just $ mkConstMethod' "createHeuristicMask" "createHeuristicMaskWithClipTight"
    [boolT] $ objT c_QImage
  , just $ mkConstMethod' "createMaskFromColor" "createMaskFromColor" [qrgb] $ objT c_QImage
  , just $ mkConstMethod' "createMaskFromColor" "createMaskFromColorWithMode"
    [qrgb, enumT e_MaskMode] $ objT c_QImage
    -- TODO createMaskFromColor
  , just $ mkConstMethod "depth" [] intT
  , test (qtVersion >= [5, 0]) $ mkProp "devicePixelRatio" qreal
  , just $ mkProp "dotsPerMeterX" intT
  , just $ mkProp "dotsPerMeterY" intT
  , just $ mkMethod' "fill" "fillWithPixel" [uintT] voidT
  , just $ mkMethod' "fill" "fillWithColor" [objT c_QColor] voidT
  , just $ mkMethod' "fill" "fillWithGlobalColor" [enumT e_GlobalColor] voidT
  , just $ mkConstMethod "format" [] $ enumT e_Format
  , just $ mkStaticMethod' "fromData" "fromDataByteArray" [objT c_QByteArray] $ objToHeapT c_QImage
  , just $ makeFnMethod (ident2 "qtah" "qimage" "fromData") "fromDataByteArrayWithFormat"
    MStatic Nonpure [objT c_QByteArray, objT c_QString] $ ptrT $ objT c_QImage
  , just $ mkStaticMethod' "fromData" "fromDataRaw" [ptrT $ constT ucharT, intT] $
    objToHeapT c_QImage
  , just $ makeFnMethod (ident2 "qtah" "qimage" "fromData") "fromDataRawWithFormat" MStatic Nonpure
    [ptrT $ constT ucharT, intT, objT c_QString] $ ptrT $ objT c_QImage
  , just $ mkConstMethod "hasAlphaChannel" [] boolT
  , just $ mkConstMethod "height" [] intT
  , just $ mkMethod' "invertPixels" "invertPixels" [] voidT
  , just $ mkMethod' "invertPixels" "invertPixelsWithMode" [enumT e_InvertMode] voidT
  , just $ mkConstMethod "isGrayscale" [] boolT
  , just $ mkConstMethod "isNull" [] boolT
  , just $ mkMethod' "load" "load" [objT c_QString] boolT
  , just $ makeFnMethod (ident2 "qtah" "qimage" "load") "loadWithFormat" MStatic Nonpure
    [refT $ objT c_QImage, objT c_QString, objT c_QString] boolT
    -- TODO load(QIODevice*, ...)
  , just $ mkMethod' "loadFromData" "loadFromDataByteArray" [objT c_QByteArray] boolT
  , just $ makeFnMethod (ident2 "qtah" "qimage" "loadFromData") "loadFromDataByteArrayWithFormat"
    MStatic Nonpure [refT $ objT c_QImage, objT c_QByteArray, objT c_QString] boolT
  , just $ mkMethod' "loadFromData" "loadFromDataRaw" [ptrT $ constT ucharT, intT] boolT
  , just $ makeFnMethod (ident2 "qtah" "qimage" "loadFromData") "loadFromDataRawWithFormat"
    MStatic Nonpure [refT $ objT c_QImage, ptrT $ constT ucharT, intT, objT c_QString] boolT
  , just $ mkConstMethod "mirrored" [boolT, boolT] $ objT c_QImage  -- TODO Args optional?
  , just $ mkProp "offset" $ objT c_QPoint
  , just $ mkConstMethod' "pixel" "pixelAtPoint" [objT c_QPoint] qrgb
  , just $ mkConstMethod' "pixel" "pixelAtRaw" [intT, intT] qrgb
  , test (qtVersion >= [5, 6]) $ mkConstMethod' "pixelColor" "pixelColorAtPoint"
    [objT c_QPoint] $ objT c_QColor
  , test (qtVersion >= [5, 6]) $ mkConstMethod' "pixelColor" "pixelColorAtRaw"
    [intT, intT] $ objT c_QColor
    -- TODO QPixelFormat pixelFormat() const
  , just $ mkConstMethod' "pixelIndex" "pixelIndexAtPoint" [objT c_QPoint] intT
  , just $ mkConstMethod' "pixelIndex" "pixelIndexAtRaw" [intT, intT] intT
  , just $ mkConstMethod "rect" [] $ objT c_QRect
  , just $ mkConstMethod "rgbSwapped" [] $ objT c_QImage
  , just $ mkMethod' "save" "save" [objT c_QString] boolT
  , just $ makeFnMethod (ident2 "qtah" "qimage" "save") "saveAll" MStatic Nonpure
    [refT $ objT c_QImage, objT c_QString, objT c_QString, intT] boolT
    -- TODO save(QIODevice*, ...)
  , just $ mkConstMethod' "scaled" "scaled" [objT c_QSize] $ objT c_QImage
  , just $ mkConstMethod' "scaled" "scaledAll"
    [objT c_QSize, enumT e_AspectRatioMode, enumT e_TransformationMode] $ objT c_QImage
  , just $ mkConstMethod' "scaled" "scaledWithRaw" [intT, intT] $ objT c_QImage
  , just $ mkConstMethod' "scaled" "scaledWithRawAll"
    [intT, intT, enumT e_AspectRatioMode, enumT e_TransformationMode] $ objT c_QImage
  , just $ mkConstMethod' "scaledToHeight" "scaledToHeight" [intT] $ objT c_QImage
  , just $ mkConstMethod' "scaledToHeight" "scaledToHeightAll"
    [intT, enumT e_TransformationMode] $ objT c_QImage
  , just $ mkConstMethod' "scaledToWidth" "scaledToWidth" [intT] $ objT c_QImage
  , just $ mkConstMethod' "scaledToWidth" "scaledToWidthAll"
    [intT, enumT e_TransformationMode] $ objT c_QImage
  , just $ mkMethod' "scanLine" "scanLine" [intT] $ ptrT ucharT
  , just $ mkConstMethod' "scanLine" "scanLineConst" [intT] $ ptrT $ constT ucharT
  , just $ mkMethod "setColor" [intT, qrgb] voidT
  , just $ mkMethod "setColorTable" [objT c_QVectorQRgb] voidT
  , just $ mkMethod' "setPixel" "setPixelAtPoint" [objT c_QPoint, uintT] voidT
  , just $ mkMethod' "setPixel" "setPixelAtRaw" [intT, intT, uintT] voidT
  , test (qtVersion >= [5, 6]) $ mkMethod' "setPixelColor" "setPixelColorAtPoint"
    [objT c_QPoint, objT c_QColor] voidT
  , test (qtVersion >= [5, 6]) $ mkMethod' "setPixelColor" "setPixelColorAtRaw"
    [intT, intT, objT c_QColor] voidT
  , just $ mkMethod "setText" [objT c_QString, objT c_QString] voidT
  , just $ mkConstMethod "size" [] $ objT c_QSize
  , just $ mkMethod "swap" [refT $ objT c_QImage] voidT
  , just $ mkConstMethod "text" [objT c_QString] $ objT c_QString
  , just $ mkConstMethod "textKeys" [] $ objT c_QStringList
    -- TODO toImageFormat
    -- TODO toPixelFormat
  , just $ mkConstMethod' "transformed" "transformedWithTransform" [objT c_QTransform] $
    objT c_QImage
  , just $ mkConstMethod' "transformed" "transformedWithTransformAll"
    [objT c_QTransform, enumT e_TransformationMode] $ objT c_QImage
    -- TODO transformed(const QMatrix&, ...)
  , just $ mkStaticMethod' "trueMatrix" "trueMatrixWithTransform"
    [objT c_QTransform, intT, intT] $ objT c_QTransform
    -- TODO trueMatrix(const QMatrix&, ...)
  , just $ mkConstMethod' "valid" "validAtPoint" [objT c_QPoint] boolT
  , just $ mkConstMethod' "valid" "validAtRaw" [intT, intT] boolT
  , just $ mkConstMethod "width" [] intT
  ]

e_Format =
  makeQtEnum (ident1 "QImage" "Format") [includeStd "QImage"] $
  [ (0, ["format", "invalid"])
  , (1, ["format", "mono"])
  , (2, ["format", "mono", "l", "s", "b"])
  , (3, ["format", "indexed", "8"])
  , (4, ["format", "r", "g", "b", "32"])
  , (5, ["format", "a", "r", "g", "b", "32"])
  , (6, ["format", "a", "r", "g", "b", "32", "premultiplied"])
  , (7, ["format", "r", "g", "b", "16"])
  , (8, ["format", "a", "r", "g", "b", "8565", "premultiplied"])
  , (9, ["format", "r", "g", "b", "666"])
  , (10, ["format", "a", "r", "g", "b", "666", "premultiplied"])
  , (11, ["format", "r", "g", "b", "555"])
  , (12, ["format", "a", "r", "g", "b", "8555", "premultiplied"])
  , (13, ["format", "r", "g", "b", "888"])
  , (14, ["format", "r", "g", "b", "444"])
  , (15, ["format", "a", "r", "g", "b", "4444", "premultiplied"])
  , (16, ["format", "r", "g", "b", "x", "8888"])
  , (17, ["format", "r", "g", "b", "a", "8888"])
  , (18, ["format", "r", "g", "b", "a", "8888", "premultiplied"])
  , (19, ["format", "b", "g", "r", "30"])
  , (20, ["format", "a", "2", "b", "g", "r", "30", "premultiplied"])
  , (21, ["format", "r", "g", "b", "30"])
  , (22, ["format", "a", "2", "r", "g", "b", "30", "premultiplied"])
  , (23, ["format", "alpha", "8"])
  , (24, ["format", "grayscale", "8"])
  ]

e_InvertMode =
  makeQtEnum (ident1 "QImage" "InvertMode") [includeStd "QImage"] $
  [ (0, ["invert", "rgb"])
  , (1, ["invert", "rgba"])
  ]
