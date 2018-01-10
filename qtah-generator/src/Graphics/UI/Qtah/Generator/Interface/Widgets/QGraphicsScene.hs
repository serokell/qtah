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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsScene (
  aModule,
  c_QGraphicsScene,
  e_ItemIndexMethod,
  e_SceneLayer,
  bs_SceneLayers,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass, ExportEnum, ExportBitspace),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkMethod,
  mkMethod',
  mkCtor,
  mkConstMethod,
  mkConstMethod',
  mkProp,
  )
import Foreign.Hoppy.Generator.Types (voidT, objT, ptrT, intT, boolT, enumT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qreal)
import Graphics.UI.Qtah.Generator.Interface.Core.QEvent (c_QEvent)
-- import Graphics.UI.Qtah.Generator.Interface.Core.QLineF (c_QLineF)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QPointF (c_QPointF)
import Graphics.UI.Qtah.Generator.Interface.Core.QRectF (c_QRectF)
-- import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types hiding (aModule)
import Graphics.UI.Qtah.Generator.Interface.Gui.QBrush (c_QBrush)
import Graphics.UI.Qtah.Generator.Interface.Gui.QFont (c_QFont)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPainterPath (c_QPainterPath)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPen (c_QPen)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPolygonF (c_QPolygonF)
import Graphics.UI.Qtah.Generator.Interface.Gui.QTransform (c_QTransform)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsEllipseItem (c_QGraphicsEllipseItem)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsItem (c_QGraphicsItem)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsPolygonItem (c_QGraphicsPolygonItem)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsRectItem (c_QGraphicsRectItem)
-- import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QGraphicsScene"] $
  (QtExport $ ExportClass c_QGraphicsScene) :
  map (QtExport . ExportEnum)
    [ e_ItemIndexMethod
    , e_SceneLayer
    ] ++
  map (QtExport . ExportBitspace)
    [ bs_SceneLayers
    ]

-- Due to a parsing bug types of the form `T<S*>` were generated incorrectly.
c_QGraphicsScene =
  addReqIncludes [includeStd "QGraphicsScene"] $
  classSetEntityPrefix "" $
  makeClass (ident "QGraphicsScene") Nothing [c_QObject] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QObject]
  , just $ mkCtor "newWithRect" [objT c_QRectF]
  , just $ mkCtor "newWithRaw" [qreal, qreal, qreal, qreal]
  , just $ mkConstMethod "activePanel" [] $ ptrT $ objT c_QGraphicsItem
  -- TODO mkConstMethod "activeWindow" [] $ ptrT $ objT c_QGraphicsWidget
  , just $ mkMethod' "addEllipse" "addEllipseRectF"
      [objT c_QRectF] $ ptrT $ objT c_QGraphicsEllipseItem
  , just $ mkMethod' "addEllipse" "addEllipseRectFAll"
      [objT c_QRectF, objT c_QPen, objT c_QBrush] $ ptrT $ objT c_QGraphicsEllipseItem
  , just $ mkMethod' "addEllipse" "addEllipseRaw"
      [qreal, qreal, qreal, qreal] $ ptrT $ objT c_QGraphicsEllipseItem
  , just $ mkMethod' "addEllipse" "addEllipseRawAll"
      [qreal, qreal, qreal, qreal, objT c_QPen, objT c_QBrush] $
        ptrT $ objT c_QGraphicsEllipseItem
  , just $ mkMethod "addItem" [ptrT $ objT c_QGraphicsItem] voidT
  -- TODO mkMethod' "addLine" "addLine" [objT c_QLineF] $
  --   ptrT $ objT c_QGraphicsLineItem
  -- TODO mkMethod' "addLine" "addLineAll" [objT c_QLineF, objT c_QPen] $
  --   ptrT $ objT c_QGraphicsLineItem
  -- TODO mkMethod' "addLine" "addLine" [qreal, qreal, qreal, qreal] $
  --   ptrT $ objT c_QGraphicsLineItem
  -- TODO mkMethod' "addLine" "addLineAll" [qreal, qreal, qreal, qreal, objT c_QPen] $
  --   ptrT $ objT c_QGraphicsLineItem
  -- TODO mkMethod "addPath" [objT c_QPainterPath] $ ptrT $ objT c_QGraphicsPathItem
  -- TODO mkMethod' "addPath" "addPathAll" [objT c_QPainterPath, objT c_QPen, objT c_QBrush] $
  --   ptrT $ objT c_QGraphicsPathItem
  -- TODO mkMethod "addPixmap" [objT c_QPixmap] $ ptrT $ objT c_QGraphicsPixmapItem
  , just $ mkMethod "addPolygon" [objT c_QPolygonF] $ ptrT $ objT c_QGraphicsPolygonItem
  , just $ mkMethod' "addPolygon" "addPolygonAll" [objT c_QPolygonF, objT c_QPen, objT c_QBrush] $
      ptrT $ objT c_QGraphicsPolygonItem
  , just $ mkMethod "addRect" [objT c_QRectF] $ ptrT $ objT c_QGraphicsRectItem
  , just $ mkMethod' "addRect" "addRectAll" [objT c_QRectF, objT c_QPen, objT c_QBrush] $
      ptrT $ objT c_QGraphicsRectItem
  , just $ mkMethod' "addRect" "addRectRaw" [qreal, qreal, qreal, qreal] $
      ptrT $ objT c_QGraphicsRectItem
  , just $ mkMethod' "addRect" "addRectRawAll"
      [qreal, qreal, qreal, qreal, objT c_QPen, objT c_QBrush] $
        ptrT $ objT c_QGraphicsRectItem
  -- TODO mkMethod "addSimpleText" [objT c_QString] $ ptrT $ objT c_QGraphicsSimpleTextItem
  -- TODO mkMethod' "addSimpleText" "addSimpleTextAll" [objT c_QString, objT c_QFont] $
  --   ptrT $ objT c_QGraphicsSimpleTextItem
  -- TODO mkMethod "addText" [objT c_QString] $ ptrT $ objT c_QGraphicsTextItem
  -- TODO mkMethod' "addText" "addTextAll" [objT c_QString, objT c_QFont] $
  --   ptrT $ objT c_QGraphicsTextItem
  -- TODO mkMethod "addWidget" [ptrT $ objT c_QWidget] $ ptrT $ objT c_QGraphicsProxyWidget
  -- TODO mkMethod' "addWidget" "addWidgetAll" [ptrT $ objT c_QWidget, bitspaceT bs_WindowFlags] $
  --   ptrT $ objT c_QGraphicsProxyWidget
  , just $ mkConstMethod "backgroundBrush" [] $ objT c_QBrush
  , just $ mkConstMethod "bspTreeDepth" [] intT
  , just $ mkMethod "clearFocus" [] voidT
  -- TODO mkConstMethod "collidingItems" [ptrT $ objT c_QGraphicsItem] $
  --   objT c_QList<QGraphicsItem $ objT c_*>
  -- TODO mkConstMethod' "collidingItems" "collidingItemsAll"
  --   [ptrT $ objT c_QGraphicsItem, objT c_Qt::ItemSelectionMode] $
  --     objT c_QList<QGraphicsItem $ objT c_*>
  -- TODO mkMethod "createItemGroup" [objT c_QList<QGraphicsItem] $ ptrT $ objT c_QGraphicsItemGroup
  -- TODO mkMethod "destroyItemGroup" [ptrT $ objT c_QGraphicsItemGroup] voidT
  , just $ mkConstMethod "focusItem" [] $ ptrT $ objT c_QGraphicsItem
  , just $ mkProp "font" $ objT c_QFont
  , just $ mkConstMethod "foregroundBrush" [] $ objT c_QBrush
  , just $ mkConstMethod "hasFocus" [] boolT
  , just $ mkConstMethod "height" [] qreal
  -- TODO mkConstMethod "inputMethodQuery" [objT c_Qt::InputMethodQuery] $ objT c_QVariant
  , just $ mkMethod "invalidate" [qreal, qreal, qreal, qreal] voidT
  -- TODO mkMethod' "invalidate" "invalidateAll"
  --   [qreal, qreal, qreal, qreal, objT c_SceneLayers] voidT
  , just $ mkConstMethod "isActive" [] boolT
  , just $ mkConstMethod' "itemAt" "itemAtPointF" [objT c_QPointF, objT c_QTransform] $
      ptrT $ objT c_QGraphicsItem
  , just $ mkConstMethod' "itemAt" "itemAtRaw" [qreal, qreal, objT c_QTransform] $
      ptrT $ objT c_QGraphicsItem
  -- TODO mkConstMethod "itemIndexMethod" [] $ objT c_ItemIndexMethod
  -- TODO mkConstMethod' "items" "items" [] $ objT c_QList<QGraphicsItem $ objT c_*>
  -- TODO mkConstMethod' "items" "itemsAll" [enumT e_SortOrder] $
  --   objT c_QList<QGraphicsItem $ objT c_*>
  -- TODO mkConstMethod' "items" "items" [objT c_QPointF] $
  --   objT c_QList<QGraphicsItem $ objT c_*>
  -- TODO mkConstMethod' "items" "itemsAll"
  --   [objT c_QPointF, objT c_Qt::ItemSelectionMode, objT c_Qt::SortOrder, objT c_QTransform] $
  --     objT c_QList<QGraphicsItem $ objT c_*>
  -- TODO mkConstMethod' "items" "items" [objT c_QRectF] $
  --   objT c_QList<QGraphicsItem $ objT c_*>
  -- TODO mkConstMethod' "items" "itemsAll"
  --   [objT c_QRectF, objT c_Qt::ItemSelectionMode, objT c_Qt::SortOrder, objT c_QTransform] $
  --     objT c_QList<QGraphicsItem $ objT c_*>
  -- TODO mkConstMethod' "items" "items" [objT c_QPolygonF] $
  --   objT c_QList<QGraphicsItem $ objT c_*>
  -- TODO mkConstMethod' "items" "itemsAll"
  --   [objT c_QPolygonF, objT c_Qt::ItemSelectionMode, objT c_Qt::SortOrder, objT c_QTransform] $
  --     objT c_QList<QGraphicsItem $ objT c_*>
  -- TODO mkConstMethod' "items" "items" [objT c_QPainterPath] $
  --   objT c_QList<QGraphicsItem $ objT c_*>
  -- TODO mkConstMethod' "items" "itemsAll"
  --   [objT c_QPainterPath, objT c_Qt::ItemSelectionMode
  --   , objT c_Qt::SortOrder, objT c_QTransform
  --   ] $ objT c_QList<QGraphicsItem $ objT c_*>
  -- TODO mkConstMethod' "items" "items"
  --   [qreal, qreal, qreal, qreal, objT c_Qt::ItemSelectionMode, objT c_Qt::SortOrder] $
  --     objT c_QList<QGraphicsItem $ objT c_*>
  -- TODO mkConstMethod' "items" "itemsAll"
  --   [ qreal, qreal, qreal, qreal, objT c_Qt::ItemSelectionMode
  --   , objT c_Qt::SortOrder, objT c_QTransform
  --   ] $ objT c_QList<QGraphicsItem $ objT c_*>
  , just $ mkConstMethod "itemsBoundingRect" [] $ objT c_QRectF
  , test (qtVersion >= [5, 4]) $ mkConstMethod "minimumRenderSize" [] qreal
  , just $ mkConstMethod "mouseGrabberItem" [] $ ptrT $ objT c_QGraphicsItem
  -- TODO mkConstMethod "palette" [] $ objT c_QPalette
  , just $ mkMethod "removeItem" [ptrT $ objT c_QGraphicsItem] voidT
  -- TODO mkMethod "render" [ptrT $ objT c_QPainter] voidT
  -- TODO mkMethod' "render" "renderAll"
  --   [ptrT $ objT c_QPainter, objT c_QRectF, objT c_QRectF, enumT e_AspectRatioMode] voidT
  , just $ mkConstMethod "sceneRect" [] $ objT c_QRectF
  -- TODO mkConstMethod "selectedItems" [] $ objT c_QList<QGraphicsItem $ objT c_*>
  , just $ mkConstMethod "selectionArea" [] $ objT c_QPainterPath
  , just $ mkMethod "sendEvent" [ptrT $ objT c_QGraphicsItem, ptrT $ objT c_QEvent] boolT
  , just $ mkMethod "setActivePanel" [ptrT $ objT c_QGraphicsItem] voidT
  -- TODO mkMethod "setActiveWindow" [ptrT $ objT c_QGraphicsWidget] voidT
  , just $ mkMethod "setBackgroundBrush" [objT c_QBrush] voidT
  , just $ mkMethod "setBspTreeDepth" [intT] voidT
  , just $ mkMethod "setFocus" [] voidT
  , just $ mkMethod' "setFocus" "setFocusAll" [enumT e_FocusReason] voidT
  , just $ mkMethod "setFocusItem" [ptrT $ objT c_QGraphicsItem] voidT
  , just $ mkMethod' "setFocusItem" "setFocusItemAll"
      [ptrT $ objT c_QGraphicsItem, enumT e_FocusReason] voidT
  , just $ mkMethod "setForegroundBrush" [objT c_QBrush] voidT
  -- TODO mkMethod "setItemIndexMethod" [objT c_ItemIndexMethod] voidT
  , test (qtVersion >= [5, 4]) $ mkMethod "setMinimumRenderSize" [qreal] voidT
  -- TODO mkMethod "setPalette" [objT c_QPalette] voidT
  , just $ mkMethod "setSceneRect" [objT c_QRectF] voidT
  , just $ mkMethod' "setSceneRect" "setSceneRectRaw" [qreal, qreal, qreal, qreal] voidT
  , test (qtVersion >= [5, 5]) $ mkMethod' "setSelectionArea" "setSelectionAreaTransform"
      [objT c_QPainterPath, objT c_QTransform] voidT
  , test (qtVersion >= [5, 5]) $ mkMethod "setSelectionArea" [objT c_QPainterPath] voidT
  -- TODO mkMethod' "setSelectionArea" "setSelectionAreaAll"
  --   [objT c_QPainterPath, objT c_Qt::ItemSelectionMode, objT c_QTransform] voidT
  -- TODO mkMethod' "setSelectionArea" "setSelectionArea"
  --   [objT c_QPainterPath, objT c_Qt::ItemSelectionOperation] voidT
  -- TODO mkMethod' "setSelectionArea" "setSelectionAreaAll"
  --   [ objT c_QPainterPath, objT c_Qt::ItemSelectionOperation
  --   , objT c_Qt::ItemSelectionMode, objT c_QTransform
  --   ] voidT
  , just $ mkMethod "setStickyFocus" [boolT] voidT
  -- TODO mkMethod "setStyle" [ptrT $ objT c_QStyle] voidT
  , just $ mkConstMethod "stickyFocus" [] boolT
  -- TODO mkConstMethod "style" [] $ ptrT $ objT c_QStyle
  , just $ mkMethod "update" [qreal, qreal, qreal, qreal] voidT
  -- TODO mkConstMethod "views" [] $ objT c_QList<QGraphicsView $ objT c_*>
  , just $ mkConstMethod "width" [] qreal
  ]

e_ItemIndexMethod =
  makeQtEnum (ident1 "QGraphicsScene" "ItemIndexMethod")
    [includeStd "QGraphicsScene"]
  [ (0, ["bsp","tree","index"])
  , (-1, ["no","index"])
  ]

(e_SceneLayer, bs_SceneLayers) =
  makeQtEnumBitspace (ident1 "QGraphicsView" "SceneLayer") "SceneLayers"
    [includeStd "QGraphicsView"]
  [ (0x1, ["item","layer"])
  , (0x2, ["background","layer"])
  , (0x4, ["foreground","layer"])
  , (0xffff, ["all","layers"])
  ]
