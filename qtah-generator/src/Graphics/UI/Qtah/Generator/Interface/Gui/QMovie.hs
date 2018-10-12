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

module Graphics.UI.Qtah.Generator.Interface.Gui.QMovie(
  aModule,
  c_QMovie,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor, mkMethod
  )

import Foreign.Hoppy.Generator.Types (boolT, intT, objT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QByteArray (c_QByteArray)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Gui.QColor (c_QColor)
import Graphics.UI.Qtah.Generator.Interface.Gui.QImage (c_QImage)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPixmap (c_QPixmap)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  c_Listener,
  c_ListenerInt,
  )
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QMovie"] $
  QtExport (ExportClass c_QMovie) :
  map QtExportSignal signals

c_QMovie =
  addReqIncludes [includeStd "QMovie"] $
  classSetEntityPrefix "" $
  makeClass (ident "QMovie") Nothing [c_QObject] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithFile" [objT c_QString]
  , just $ mkConstMethod "backgroundColor" [] $ objT c_QColor
  , just $ mkConstMethod "currentFrameNumber" [] intT
  , just $ mkConstMethod "currentImage" [] $ objT c_QImage
  , just $ mkConstMethod "currentPixmap" [] $ objT c_QPixmap
  , just $ mkConstMethod "fileName" [] $ objT c_QString
  , just $ mkConstMethod "format" [] $ objT c_QByteArray
  , just $ mkConstMethod "frameCount" [] intT
  , just $ mkConstMethod "frameRect" [] $ objT c_QRect
  , just $ mkConstMethod "isValid" []  boolT
  , just $ mkMethod "jumpToFrame" [intT] boolT
  , just $ mkConstMethod "loopCount" [] intT
  , just $ mkConstMethod "nextFrameDelay" [] intT
  , just $ mkMethod "scaledSize" [] $ objT c_QSize
  , just $ mkMethod "setBackgroundColor" [objT c_QColor] voidT
  , just $ mkMethod "setFileName" [objT c_QString] voidT
  , just $ mkMethod "setFormat" [objT c_QByteArray] voidT
  , just $ mkMethod "setScaledSize" [objT c_QSize] voidT
  , just $ mkConstMethod "speed" [] intT
  , just $ mkMethod "start" [] voidT
  , just $ mkMethod "stop" [] voidT
  ]

signals =
  [ makeSignal c_QMovie "finished" c_Listener
  , makeSignal c_QMovie "frameChanged" c_ListenerInt
  , makeSignal c_QMovie "started" c_Listener
  ]
