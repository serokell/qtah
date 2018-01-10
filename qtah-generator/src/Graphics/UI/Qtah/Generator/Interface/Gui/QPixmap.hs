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

module Graphics.UI.Qtah.Generator.Interface.Gui.QPixmap (
  aModule,
  c_QPixmap,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetConversionToGc,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  mkStaticMethod,
  mkStaticMethod',
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (
  bitspaceT,
  boolT,
  enumT,
  intT,
  int64T,
  objT,
  ptrT,
  refT,
  voidT,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (
  bs_ImageConversionFlags,
  e_AspectRatioMode,
  e_TransformationMode,
  qreal,
  )
import Graphics.UI.Qtah.Generator.Interface.Gui.QColor (c_QColor)
import Graphics.UI.Qtah.Generator.Interface.Gui.QImage (c_QImage)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPaintDevice (c_QPaintDevice)
import Graphics.UI.Qtah.Generator.Interface.Gui.QRegion (c_QRegion)
import Graphics.UI.Qtah.Generator.Interface.Gui.QTransform (c_QTransform)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QPixmap"]
  [ QtExport $ ExportClass c_QPixmap
  ]

c_QPixmap =
  addReqIncludes [includeStd "QPixmap"] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Copyable] $
  classSetEntityPrefix "" $
  makeClass (ident "QPixmap") Nothing [c_QPaintDevice] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithSize" [objT c_QSize]
  , just $ mkCtor "newWithSizeRaw" [intT, intT]
  , just $ mkCtor "newWithFile" [objT c_QString]
    -- TODO QPixmap(const QString&, const char*, Qt::ImageConversionFlags)
    -- TODO QPixmap(const char* const[] xpm)  (Does this copy the data?)
  , just $ mkConstMethod "cacheKey" [] int64T
  , just $ mkMethod' "convertFromImage" "convertFromImage" [objT c_QImage] boolT
  , just $ mkMethod' "convertFromImage" "convertFromImageAll"
    [objT c_QImage, bitspaceT bs_ImageConversionFlags] boolT
  , just $ mkConstMethod' "copy" "copyRect" [objT c_QRect] $ objT c_QPixmap
  , just $ mkConstMethod' "copy" "copyRaw" [intT, intT, intT, intT] $ objT c_QPixmap
    -- TODO QBitmap createHeuristicMask(bool =) const
    -- TODO QBitmap createMaskFromColor(const QColor&, Qt::MaskMode =) const
  , just $ mkStaticMethod "defaultDepth" [] intT
  , just $ mkConstMethod "depth" [] intT
  , just $ mkMethod "detach" [] voidT
  , test (qtVersion >= [5, 0]) $ mkProp "devicePixelRatio" qreal
  , just $ mkMethod' "fill" "fill" [] voidT
  , just $ mkMethod' "fill" "fillWithColor" [objT c_QColor] voidT
  , just $ mkStaticMethod' "fromImage" "fromImage" [objT c_QImage] $ objT c_QPixmap
  , just $ mkStaticMethod' "fromImage" "fromImageAll"
    [objT c_QImage, bitspaceT bs_ImageConversionFlags] $ objT c_QPixmap
    -- TODO QPixmap fromImageReader(QImageReader*, ...)
  , just $ mkConstMethod "hasAlpha" [] boolT
  , just $ mkConstMethod "hasAlphaChannel" [] boolT
  , just $ mkConstMethod "height" [] intT
  , just $ mkConstMethod "isNull" [] boolT
  , just $ mkConstMethod "isQBitmap" [] boolT
  , just $ mkMethod' "load" "load" [objT c_QString] boolT
    -- TODO load(const QString&, const char*, Qt::ImageConversionFlags)
    -- TODO loadFromData
    -- TODO mask
  , just $ mkConstMethod "rect" [] $ objT c_QRect
  , just $ mkConstMethod "save" [objT c_QString] boolT
    -- TODO bool save(const QString&, const char*, int)
    -- TODO save(QIODevice*, ...)
  , just $ mkConstMethod' "scaled" "scaledRaw" [intT, intT] $ objT c_QPixmap
  , just $ mkConstMethod' "scaled" "scaledRawAll"
    [intT, intT, enumT e_AspectRatioMode, enumT e_TransformationMode] $ objT c_QPixmap
  , just $ mkConstMethod' "scaled" "scaledSize" [objT c_QSize] $ objT c_QPixmap
  , just $ mkConstMethod' "scaled" "scaledSizeAll"
    [objT c_QSize, enumT e_AspectRatioMode, enumT e_TransformationMode] $ objT c_QPixmap
  , just $ mkConstMethod' "scaledToHeight" "scaledToHeight" [intT] $ objT c_QPixmap
  , just $ mkConstMethod' "scaledToHeight" "scaledToHeightAll" [intT, enumT e_TransformationMode] $
    objT c_QPixmap
  , just $ mkConstMethod' "scaledToWidth" "scaledToWidth" [intT] $ objT c_QPixmap
  , just $ mkConstMethod' "scaledToWidth" "scaledToWidthAll" [intT, enumT e_TransformationMode] $
    objT c_QPixmap
  , just $ mkMethod' "scroll" "scrollRaw" [intT, intT, intT, intT, intT, intT] voidT
  , just $ mkMethod' "scroll" "scrollRawAll"
    [intT, intT, intT, intT, intT, intT, ptrT $ objT c_QRegion] voidT
  , just $ mkMethod' "scroll" "scrollRect" [intT, intT, objT c_QRect] voidT
  , just $ mkMethod' "scroll" "scrollRectAll" [intT, intT, objT c_QRect, ptrT $ objT c_QRegion]
    voidT
  , just $ mkConstMethod "size" [] $ objT c_QSize
  , just $ mkMethod "swap" [refT $ objT c_QPixmap] voidT
  , just $ mkConstMethod "toImage" [] $ objT c_QImage
  , just $ mkConstMethod' "transformed" "transformed" [objT c_QTransform] $ objT c_QPixmap
  , just $ mkConstMethod' "transformed" "transformedAll"
    [objT c_QTransform, enumT e_TransformationMode] $ objT c_QPixmap
    -- TODO QPixmap transformed(const QMatrix&, ...)
  , just $ mkStaticMethod' "trueMatrix" "trueMatrixTransform" [objT c_QTransform, intT, intT] $
    objT c_QTransform
    -- TODO QPixmap trueMatrix(const QMatrix&, ...)
  , just $ mkConstMethod "width" [] intT
    -- OMIT bool operator!() (It's just isNull).
  ]
