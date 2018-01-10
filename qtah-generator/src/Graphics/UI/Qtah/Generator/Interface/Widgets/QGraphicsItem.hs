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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsItem (
  aModule,
  c_QGraphicsItem,
  e_CacheMode,
  e_GraphicsItemChange,
  e_PanelModality,
  bs_GraphicsItemFlags
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
  mkConstMethod,
  mkMethod',
  mkConstMethod'
  )
import Foreign.Hoppy.Generator.Types (voidT, objT, ptrT, boolT, constT, intT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (qreal)
import Graphics.UI.Qtah.Generator.Interface.Core.QPointF (c_QPointF)
import Graphics.UI.Qtah.Generator.Interface.Core.QRectF (c_QRectF)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
-- import Graphics.UI.Qtah.Generator.Interface.Gui.QPolygonF (c_QPolygonF)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPainterPath (c_QPainterPath)
-- import Graphics.UI.Qtah.Generator.Interface.Gui.QTransform (c_QTransform)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsScene (c_QGraphicsScene)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QGraphicsItem"] $
  map QtExport $
    ExportClass c_QGraphicsItem :
    map ExportEnum
      [ e_CacheMode
      , e_GraphicsItemChange
      , e_GraphicsItemFlag
      , e_PanelModality
      ] ++
    map ExportBitspace [bs_GraphicsItemFlags]

c_QGraphicsItem =
  addReqIncludes [includeStd "QGraphicsItem"] $
  classSetEntityPrefix "" $
  makeClass (ident "QGraphicsItem") Nothing []
  [ mkConstMethod "acceptDrops" [] boolT
  , mkConstMethod "acceptHoverEvents" [] boolT
  , mkConstMethod "acceptTouchEvents" [] boolT
  -- TODO mkConstMethod "acceptedMouseButtons" [] $ objT c_Qt::MouseButtons
  , mkMethod "advance" [intT] voidT
  , mkConstMethod "boundingRect" [] $ objT c_QRectF
  -- TODO mkConstMethod "boundingRegion" [objT c_QTransform] $ objT c_QRegion
  , mkConstMethod "boundingRegionGranularity" [] qreal
  -- TODO mkConstMethod "cacheMode" [] $ objT bs_CacheMode
  -- TODO mkConstMethod "childItems" [] $ objT c_QList<QGraphicsItem $ objT c_*>
  , mkConstMethod "childrenBoundingRect" [] $ objT c_QRectF
  , mkMethod "clearFocus" [] voidT
  , mkConstMethod "clipPath" [] $ objT c_QPainterPath
  , mkConstMethod "collidesWithItem" [ptrT $ constT $ objT c_QGraphicsItem] boolT
  -- TODO mkConstMethod' "collidesWithItem" "collidesWithItemAll"
  --   [ptrT $ constT $ objT c_QGraphicsItem, objT c_Qt::ItemSelectionMode] boolT
  , mkConstMethod "collidesWithPath" [objT c_QPainterPath] boolT
  -- TODO mkConstMethod' "collidesWithPath" "collidesWithPathAll"
  --   [objT c_QPainterPath, objT c_Qt::ItemSelectionMode] boolT
  -- TODO mkConstMethod "collidingItems" [] $ objT c_QList<QGraphicsItem $ objT c_*>
  -- TODO mkConstMethod' "collidingItems" "collidingItemsAll"
  --   [objT c_Qt::ItemSelectionMode] $ objT c_QList<QGraphicsItem $ objT c_*>
  , mkConstMethod "commonAncestorItem" [ptrT $ constT $ objT c_QGraphicsItem] $
      ptrT $ objT c_QGraphicsItem
  , mkConstMethod "contains" [objT c_QPointF] boolT
  -- TODO mkConstMethod "cursor" [] $ objT c_QCursor
  -- TODO mkConstMethod "data" [intT] $ objT c_QVariant
  -- TODO mkConstMethod "deviceTransform" [objT c_QTransform] $ objT c_QTransform
  , mkConstMethod "effectiveOpacity" [] qreal
  , mkMethod "ensureVisible" [] voidT
  , mkMethod' "ensureVisible" "ensureVisibleRectFAll" [objT c_QRectF, intT, intT] voidT
  , mkMethod' "ensureVisible" "ensureVisibleRaw"
      [qreal, qreal, qreal, qreal] voidT
  , mkMethod' "ensureVisible" "ensureVisibleRawAll"
      [qreal, qreal, qreal, qreal, intT, intT] voidT
  , mkConstMethod "filtersChildEvents" [] boolT
  -- TODO mkConstMethod "flags" [] $ objT c_GraphicsItemFlags
  , mkConstMethod "focusItem" [] $ ptrT $ objT c_QGraphicsItem
  , mkConstMethod "focusProxy" [] $ ptrT $ objT c_QGraphicsItem
  , mkMethod "grabKeyboard" [] voidT
  , mkMethod "grabMouse" [] voidT
  -- TODO mkConstMethod "graphicsEffect" [] $ ptrT $ objT c_QGraphicsEffect
  -- TODO mkConstMethod "group" [] $ ptrT $ objT c_QGraphicsItemGroup
  , mkConstMethod "hasCursor" [] boolT
  , mkConstMethod "hasFocus" [] boolT
  , mkMethod "hide" [] voidT
  -- TODO mkConstMethod "inputMethodHints" [] $ objT c_Qt::InputMethodHints
  , mkMethod "installSceneEventFilter" [ptrT $ objT c_QGraphicsItem] voidT
  , mkConstMethod "isActive" [] boolT
  , mkConstMethod "isAncestorOf" [ptrT $ constT $ objT c_QGraphicsItem] boolT
  , mkConstMethod "isBlockedByModalPanel" [] boolT
  , mkConstMethod' "isBlockedByModalPanel" "isBlockedByModalPanelAll"
      [ptrT $ ptrT $ objT c_QGraphicsItem] boolT
  , mkConstMethod "isClipped" [] boolT
  , mkConstMethod "isEnabled" [] boolT
  , mkConstMethod "isObscured" [] boolT
  , mkConstMethod' "isObscured" "isObscuredRaw" [qreal, qreal, qreal, qreal] boolT
  , mkConstMethod' "isObscured" "isObscuredRectF" [objT c_QRectF] boolT
  , mkConstMethod "isObscuredBy" [ptrT $ constT $ objT c_QGraphicsItem] boolT
  , mkConstMethod "isPanel" [] boolT
  , mkConstMethod "isSelected" [] boolT
  , mkConstMethod "isUnderMouse" [] boolT
  , mkConstMethod "isVisible" [] boolT
  , mkConstMethod "isVisibleTo" [ptrT $ constT $ objT c_QGraphicsItem] boolT
  , mkConstMethod "isWidget" [] boolT
  , mkConstMethod "isWindow" [] boolT
  -- TODO mkConstMethod "itemTransform" [ptrT $ constT $ objT c_QGraphicsItem] $
  --   objT c_QTransform
  -- TODO mkConstMethod' "itemTransform" "itemTransformAll"
  --   [ptrT $ constT $ objT c_QGraphicsItem, ptrT $ boolT] $ objT c_QTransform
  -- TODO mkConstMethod' "mapFromItem" "mapFromItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, objT c_QPointF] $ objT c_QPointF
  -- TODO mkConstMethod' "mapFromItem" "mapFromItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, objT c_QRectF] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapFromItem" "mapFromItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, objT c_QPolygonF] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapFromItem" "mapFromItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, objT c_QPainterPath] $ objT c_QPainterPath
  -- TODO mkConstMethod' "mapFromItem" "mapFromItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, qreal, qreal, qreal, qreal] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapFromItem" "mapFromItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, qreal, qreal] $ objT c_QPointF
  -- TODO mkConstMethod' "mapFromParent" "mapFromParent"
  --   [objT c_QPointF] $ objT c_QPointF
  -- TODO mkConstMethod' "mapFromParent" "mapFromParent"
  --   [objT c_QRectF] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapFromParent" "mapFromParent"
  --   [objT c_QPolygonF] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapFromParent" "mapFromParent"
  --   [objT c_QPainterPath] $ objT c_QPainterPath
  -- TODO mkConstMethod' "mapFromParent" "mapFromParent"
  --   [qreal, qreal, qreal, qreal] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapFromParent" "mapFromParent"
  --   [qreal, qreal] $ objT c_QPointF
  -- TODO mkConstMethod' "mapFromScene" "mapFromScene"
  --   [objT c_QPointF] $ objT c_QPointF
  -- TODO mkConstMethod' "mapFromScene" "mapFromScene"
  --   [objT c_QRectF] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapFromScene" "mapFromScene"
  --   [objT c_QPolygonF] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapFromScene" "mapFromScene"
  --   [objT c_QPainterPath] $ objT c_QPainterPath
  -- TODO mkConstMethod' "mapFromScene" "mapFromScene"
  --   [qreal, qreal, qreal, qreal] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapFromScene" "mapFromScene"
  --   [qreal, qreal] $ objT c_QPointF
  -- TODO mkConstMethod' "mapRectFromItem" "mapRectFromItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, objT c_QRectF] $ objT c_QRectF
  -- TODO mkConstMethod' "mapRectFromItem" "mapRectFromItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, qreal, qreal, qreal, qreal] $ objT c_QRectF
  -- TODO mkConstMethod' "mapRectFromParent" "mapRectFromParent"
  --   [objT c_QRectF] $ objT c_QRectF
  -- TODO mkConstMethod' "mapRectFromParent" "mapRectFromParent"
  --   [qreal, qreal, qreal, qreal] $ objT c_QRectF
  -- TODO mkConstMethod' "mapRectFromScene" "mapRectFromScene"
  --   [objT c_QRectF] $ objT c_QRectF
  -- TODO mkConstMethod' "mapRectFromScene" "mapRectFromScene"
  --   [qreal, qreal, qreal, qreal] $ objT c_QRectF
  -- TODO mkConstMethod' "mapRectToItem" "mapRectToItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, objT c_QRectF] $ objT c_QRectF
  -- TODO mkConstMethod' "mapRectToItem" "mapRectToItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, qreal, qreal, qreal, qreal] $ objT c_QRectF
  -- TODO mkConstMethod' "mapRectToParent" "mapRectToParent"
  --   [objT c_QRectF] $ objT c_QRectF
  -- TODO mkConstMethod' "mapRectToParent" "mapRectToParent"
  --   [qreal, qreal, qreal, qreal] $ objT c_QRectF
  -- TODO mkConstMethod' "mapRectToScene" "mapRectToScene"
  --   [objT c_QRectF] $ objT c_QRectF
  -- TODO mkConstMethod' "mapRectToScene" "mapRectToScene"
  --   [qreal, qreal, qreal, qreal] $ objT c_QRectF
  -- TODO mkConstMethod' "mapToItem" "mapToItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, objT c_QPointF] $ objT c_QPointF
  -- TODO mkConstMethod' "mapToItem" "mapToItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, objT c_QRectF] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapToItem" "mapToItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, objT c_QPolygonF] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapToItem" "mapToItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, objT c_QPainterPath] $ objT c_QPainterPath
  -- TODO mkConstMethod' "mapToItem" "mapToItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, qreal, qreal, qreal, qreal] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapToItem" "mapToItem"
  --   [ptrT $ constT $ objT c_QGraphicsItem, qreal, qreal] $ objT c_QPointF
  -- TODO mkConstMethod' "mapToParent" "mapToParent"
  --   [objT c_QPointF] $ objT c_QPointF
  -- TODO mkConstMethod' "mapToParent" "mapToParent"
  --   [objT c_QRectF] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapToParent" "mapToParent"
  --   [objT c_QPolygonF] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapToParent" "mapToParent"
  --   [objT c_QPainterPath] $ objT c_QPainterPath
  -- TODO mkConstMethod' "mapToParent" "mapToParent"
  --   [qreal, qreal, qreal, qreal] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapToParent" "mapToParent"
  --   [qreal, qreal] $ objT c_QPointF
  -- TODO mkConstMethod' "mapToScene" "mapToScene"
  --   [objT c_QPointF] $ objT c_QPointF
  -- TODO mkConstMethod' "mapToScene" "mapToScene"
  --   [objT c_QRectF] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapToScene" "mapToScene"
  --   [objT c_QPolygonF] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapToScene" "mapToScene"
  --   [objT c_QPainterPath] $ objT c_QPainterPath
  -- TODO mkConstMethod' "mapToScene" "mapToScene"
  --   [qreal, qreal, qreal, qreal] $ objT c_QPolygonF
  -- TODO mkConstMethod' "mapToScene" "mapToScene"
  --   [qreal, qreal] $ objT c_QPointF
  , mkMethod "moveBy" [qreal, qreal] voidT
  , mkConstMethod "opacity" [] qreal
  , mkConstMethod "opaqueArea" [] $ objT c_QPainterPath
  -- TODO mkMethod "paint"
  --   [ptrT $ objT c_QPainter, ptrT $ constT $ objT c_QStyleOptionGraphicsItem] voidT
  -- TODO mkMethod' "paint" "paintAll"
  --   [ ptrT $ objT c_QPainter
  --   , ptrT $ constT $ objT c_QStyleOptionGraphicsItem
  --   , ptrT $ objT c_QWidget
  --   ] voidT
  , mkConstMethod "panel" [] $ ptrT $ objT c_QGraphicsItem
  -- TODO mkConstMethod "panelModality" [] $ objT c_PanelModality
  , mkConstMethod "parentItem" [] $ ptrT $ objT c_QGraphicsItem
  -- TODO mkConstMethod "parentObject" [] $ ptrT $ objT c_QGraphicsObject
  -- TODO mkConstMethod "parentWidget" [] $ ptrT $ objT c_QGraphicsWidget
  , mkConstMethod "pos" [] $ objT c_QPointF
  , mkMethod "removeSceneEventFilter" [ptrT $ objT c_QGraphicsItem] voidT
  , mkMethod "resetTransform" [] voidT
  , mkConstMethod "rotation" [] qreal
  , mkConstMethod "scale" [] qreal
  , mkConstMethod "scene" [] $ ptrT $ objT c_QGraphicsScene
  , mkConstMethod "sceneBoundingRect" [] $ objT c_QRectF
  , mkConstMethod "scenePos" [] $ objT c_QPointF
  -- TODO mkConstMethod "sceneTransform" [] $ objT c_QTransform
  , mkMethod "scroll" [qreal, qreal] voidT
  , mkMethod' "scroll" "scrollAll" [qreal, qreal, objT c_QRectF] voidT
  , mkMethod "setAcceptDrops" [boolT] voidT
  , mkMethod "setAcceptHoverEvents" [boolT] voidT
  , mkMethod "setAcceptTouchEvents" [boolT] voidT
  -- TODO mkMethod "setAcceptedMouseButtons" [objT c_Qt::MouseButtons] voidT
  , mkMethod "setActive" [boolT] voidT
  , mkMethod "setBoundingRegionGranularity" [qreal] voidT
  -- TODO mkMethod "setCacheMode" [objT c_CacheMode] voidT
  -- TODO mkMethod' "setCacheMode" "setCacheModeAll" [objT c_CacheMode, objT c_QSize] voidT
  -- TODO mkMethod "setCursor" [objT c_QCursor] voidT
  -- TODO mkMethod "setData" [intT, objT c_QVariant] voidT
  , mkMethod "setEnabled" [boolT] voidT
  , mkMethod "setFiltersChildEvents" [boolT] voidT
  -- TODO mkMethod "setFlag" [objT c_GraphicsItemFlag] voidT
  -- TODO mkMethod' "setFlag" "setFlagAll" [objT c_GraphicsItemFlag, boolT] voidT
  -- TODO mkMethod "setFlags" [objT c_GraphicsItemFlags] voidT
  , mkMethod "setFocus" [] voidT
 --  , mkMethod' "setFocus" "setFocusAll" [objT c_Qt::FocusReason] voidT
  , mkMethod "setFocusProxy" [ptrT $ objT c_QGraphicsItem] voidT
  -- TODO mkMethod "setGraphicsEffect" [ptrT $ objT c_QGraphicsEffect] voidT
  -- TODO mkMethod "setGroup" [ptrT $ objT c_QGraphicsItemGroup] voidT
  -- TODO mkMethod "setInputMethodHints" [objT c_Qt::InputMethodHints] voidT
  , mkMethod "setOpacity" [qreal] voidT
  -- TODO mkMethod "setPanelModality" [objT c_PanelModality] voidT
  , mkMethod "setParentItem" [ptrT $ objT c_QGraphicsItem] voidT
  , mkMethod' "setPos" "setPosPointF" [objT c_QPointF] voidT
  , mkMethod' "setPos" "setPosRaw" [qreal, qreal] voidT
  , mkMethod "setRotation" [qreal] voidT
  , mkMethod "setScale" [qreal] voidT
  , mkMethod "setSelected" [boolT] voidT
  , mkMethod "setToolTip" [objT c_QString] voidT
  -- TODO mkMethod "setTransform" [objT c_QTransform] voidT
  -- TODO mkMethod' "setTransform" "setTransformAll" [objT c_QTransform, boolT] voidT
  , mkMethod' "setTransformOriginPoint" "setTransformOriginPointF" [objT c_QPointF] voidT
  , mkMethod' "setTransformOriginPoint" "setTransformOriginPointRaw" [qreal, qreal] voidT
  -- TODO mkMethod "setTransformations" [objT c_QList<QGraphicsTransform] voidT
  , mkMethod "setVisible" [boolT] voidT
  , mkMethod "setX" [qreal] voidT
  , mkMethod "setY" [qreal] voidT
  , mkMethod "setZValue" [qreal] voidT
  , mkConstMethod "shape" [] $ objT c_QPainterPath
  , mkMethod "show" [] voidT
  , mkMethod "stackBefore" [ptrT $ constT $ objT c_QGraphicsItem] voidT
  -- TODO mkMethod' "toGraphicsObject" "toGraphicsObject" [] $
  --   ptrT $ objT c_QGraphicsObject
  -- TODO mkConstMethod' "toGraphicsObject" "toGraphicsObject" [] $
  --   ptrT $ constT $ objT c_QGraphicsObject
  , mkConstMethod "toolTip" [] $ objT c_QString
  , mkConstMethod "topLevelItem" [] $ ptrT $ objT c_QGraphicsItem
  -- TODO mkConstMethod "topLevelWidget" [] $ ptrT $ objT c_QGraphicsWidget
  -- TODO mkConstMethod "transform" [] $ objT c_QTransform
  , mkConstMethod "transformOriginPoint" [] $ objT c_QPointF
  -- TODO mkConstMethod "transformations" [] $ objT c_QList<QGraphicsTransform $ objT c_*>
  , mkConstMethod' "type" "itemType" [] intT
  , mkMethod "ungrabKeyboard" [] voidT
  , mkMethod "ungrabMouse" [] voidT
  , mkMethod "unsetCursor" [] voidT
  , mkMethod "update" [] voidT
  , mkMethod' "update" "updateRectF" [objT c_QRectF] voidT
  , mkMethod' "update" "updateRaw" [qreal, qreal, qreal, qreal] voidT
  -- TODO mkConstMethod "window" [] $ ptrT $ objT c_QGraphicsWidget
  , mkConstMethod "x" [] qreal
  , mkConstMethod "y" [] qreal
  , mkConstMethod "zValue" [] qreal
  ]

e_CacheMode =
  makeQtEnum (ident1 "QGraphicsItem" "CacheMode") [includeStd "QGraphicsItem"]
  [ (0, ["no","cache"])
  , (1, ["item","coordinate","cache"])
  , (2, ["device","coordinate","cache"])
  ]

e_GraphicsItemChange =
  makeQtEnum (ident1 "QGraphicsItem" "GraphicsItemChange") [includeStd "QGraphicsItem"]
  [ (3, ["item","enabled","change"])
  , (13, ["item","enabled","has","changed"])
  , (1, ["item","matrix","change"])
  , (0, ["item","position","change"])
  , (9, ["item","position","has","changed"])
  , (8, ["item","transform","change"])
  , (10, ["item","transform","has","changed"])
  , (28, ["item","rotation","change"])
  , (29, ["item","rotation","has","changed"])
  , (30, ["item","scale","change"])
  , (31, ["item","scale","has","changed"])
  , (32, ["item","transform","origin","point","change"])
  , (33, ["item","transform","origin","point","has","changed"])
  , (4, ["item","selected","change"])
  , (14, ["item","selected","has","changed"])
  , (2, ["item","visible","change"])
  , (12, ["item","visible","has","changed"])
  , (5, ["item","parent","change"])
  , (15, ["item","parent","has","changed"])
  , (6, ["item","child","added","change"])
  , (7, ["item","child","removed","change"])
  , (11, ["item","scene","change"])
  , (16, ["item","scene","has","changed"])
  , (17, ["item","cursor","change"])
  , (18, ["item","cursor","has","changed"])
  , (19, ["item","tool","tip","change"])
  , (20, ["item","tool","tip","has","changed"])
  , (21, ["item","flags","change"])
  , (22, ["item","flags","have","changed"])
  , (23, ["item","z","value","change"])
  , (24, ["item","z","value","has","changed"])
  , (25, ["item","opacity","change"])
  , (26, ["item","opacity","has","changed"])
  , (27, ["item","scene","position","has","changed"])
  ]

(e_GraphicsItemFlag, bs_GraphicsItemFlags) =
  makeQtEnumBitspace (ident1 "QGraphicsItem" "GraphicsItemFlag") "GraphicsItemFlags"
    [includeStd "QGraphicsItem"] $
  collect
  [ just $ (0x1, ["item","is","movable"])
  , just $ (0x2, ["item","is","selectable"])
  , just $ (0x4, ["item","is","focusable"])
  , just $ (0x8, ["item","clips","to","shape"])
  , just $ (0x10, ["item","clips","children","to","shape"])
  , just $ (0x20, ["item","ignores","transformations"])
  , just $ (0x40, ["item","ignores","parent","opacity"])
  , just $ (0x80, ["item","doesnt","propagate","opacity","to","children"])
  , just $ (0x100, ["item","stacks","behind","parent"])
  , just $ (0x200, ["item","uses","extended","style","option"])
  , just $ (0x400, ["item","has","no","contents"])
  , just $ (0x800, ["item","sends","geometry","changes"])
  , just $ (0x1000, ["item","accepts","input","method"])
  , just $ (0x2000, ["item","negative","z","stacks","behind","parent"])
  , just $ (0x4000, ["item","is","panel"])
  , just $ (0x10000, ["item","sends","scene","position","changes"])
  , test (qtVersion >= [5, 4]) $ (0x80000, ["item","contains","children","in","shape"])
  ]

e_PanelModality =
  makeQtEnum (ident1 "QGraphicsItem" "PanelModality") [includeStd "QGraphicsItem"]
  [ (0, ["non","modal"])
  , (1, ["panel","modal"])
  , (2, ["scene","modal"])
  ]
