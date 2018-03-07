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

module Graphics.UI.Qtah.Generator.Interface.Internal.Callback where

import Foreign.Hoppy.Generator.Spec (
  Callback,
  Export (ExportCallback),
  makeCallback,
  makeModule,
  moduleAddExports,
  moduleAddHaskellName,
  moduleModify',
  toExtName,
  )
import Foreign.Hoppy.Generator.Types (
  bitspaceT,
  boolT,
  constT,
  doubleT,
  enumT,
  intT,
  objT,
  ptrT,
  refT,
  toGcT,
  voidT,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QAbstractItemModel (c_QAbstractItemModel)
import Graphics.UI.Qtah.Generator.Interface.Core.QDate (c_QDate)
import Graphics.UI.Qtah.Generator.Interface.Core.QEvent (c_QEvent)
import Graphics.UI.Qtah.Generator.Interface.Core.QItemSelection (c_QItemSelection)
import Graphics.UI.Qtah.Generator.Interface.Core.QModelIndex (c_QModelIndex)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Core.QVector (c_QVectorInt)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (
  bs_ToolBarAreas,
  e_Orientation,
  e_ScreenOrientation,
  e_ScreenOrientation_minVersion,
  e_ToolButtonStyle,
  e_WindowModality,
  e_WindowState,
  qreal,
  )
import {-# SOURCE #-} qualified Graphics.UI.Qtah.Generator.Interface.Gui.QClipboard as QClipboard
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Gui.QIcon (c_QIcon)
import Graphics.UI.Qtah.Generator.Interface.Gui.QPaintEvent (c_QPaintEvent)
import {-# SOURCE #-} qualified Graphics.UI.Qtah.Generator.Interface.Gui.QWindow as QWindow
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractButton
  (c_QAbstractButton)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractSlider (e_SliderAction)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QAction (c_QAction)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QGraphicsItem (c_QGraphicsItem)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QSystemTrayIcon (
  e_ActivationReason,
  )
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QTreeWidget (c_QTreeWidgetItem)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AHoppyModule))

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AHoppyModule $
  moduleModify' (makeModule "callback" "b_callback.hpp" "b_callback.cpp") $ do
    moduleAddHaskellName ["Internal", "Callback"]
    moduleAddExports $ collect
      [ just $ ExportCallback cb_BoolVoid
      , just $ ExportCallback cb_DoubleVoid
      , just $ ExportCallback cb_IntVoid
      , just $ ExportCallback cb_IntBoolVoid
      , just $ ExportCallback cb_IntIntVoid
      , just $ ExportCallback cb_OrientationVoid
      , just $ ExportCallback cb_PtrQAbstractButtonVoid
      , just $ ExportCallback cb_PtrQAbstractButtonBoolVoid
      , just $ ExportCallback cb_PtrQAbstractItemModelVoid
      , just $ ExportCallback cb_PtrQActionVoid
      , just $ ExportCallback cb_PtrQGraphicsItemPtrQEventBool
      , just $ ExportCallback cb_PtrQObjectPtrQEventBool
      , just $ ExportCallback cb_PtrQObjectVoid
      , just $ ExportCallback cb_PtrQPaintEventVoid
      , just $ ExportCallback cb_PtrQTreeWidgetItemVoid
      , just $ ExportCallback cb_PtrQTreeWidgetItemIntVoid
      , just $ ExportCallback cb_PtrQTreeWidgetItemPtrQTreeWidgetItemVoid
      , just $ ExportCallback cb_PtrQWidgetPtrQWidgetVoid
      , just $ ExportCallback cb_QAbstractSliderActionVoid
      , just $ ExportCallback cb_QClipboardModeVoid
      , just $ ExportCallback cb_QDateVoid
      , just $ ExportCallback cb_QModelIndexVoid
      , just $ ExportCallback cb_QModelIndexIntIntVoid
      , just $ ExportCallback cb_QModelIndexIntIntQModelIndexIntVoid
      , just $ ExportCallback cb_QModelIndexQModelIndexVoid
      , just $ ExportCallback cb_QModelIndexQModelIndexQVectorIntVoid
      , test (qtVersion >= QWindow.minVersion) $ ExportCallback cb_QWindowVisibilityVoid
      , just $ ExportCallback cb_QPointVoid
      , just $ ExportCallback cb_QrealVoid
      , just $ ExportCallback cb_QSizeVoid
      , just $ ExportCallback cb_QStringVoid
      , just $ ExportCallback cb_QSystemTrayIconActivationReasonVoid
      , just $ ExportCallback cb_RefConstQIconVoid
      , just $ ExportCallback cb_RefConstQItemSelectionRefConstQItemSelectionVoid
      , test (qtVersion >= e_ScreenOrientation_minVersion) $ ExportCallback cb_ScreenOrientationVoid
      , just $ ExportCallback cb_ToolBarAreasVoid
      , just $ ExportCallback cb_ToolButtonStyleVoid
      , just $ ExportCallback cb_WindowModalityVoid
      , just $ ExportCallback cb_WindowStateVoid
      , just $ ExportCallback cb_Void
      ]

cb_BoolVoid =
  makeCallback (toExtName "CallbackBoolVoid")
  [boolT] voidT

cb_DoubleVoid =
  makeCallback (toExtName "CallbackDoubleVoid")
  [doubleT] voidT

cb_IntVoid =
  makeCallback (toExtName "CallbackIntVoid")
  [intT] voidT

cb_IntBoolVoid =
  makeCallback (toExtName "CallbackIntBoolVoid")
  [intT, boolT] voidT

cb_IntIntVoid =
  makeCallback (toExtName "CallbackIntIntVoid")
  [intT, intT] voidT

cb_OrientationVoid =
  makeCallback (toExtName "CallbackOrientationVoid")
  [enumT e_Orientation] voidT

cb_PtrQAbstractButtonVoid =
  makeCallback (toExtName "CallbackPtrQAbstractButtonVoid")
  [ptrT $ objT c_QAbstractButton] voidT

cb_PtrQAbstractButtonBoolVoid =
  makeCallback (toExtName "CallbackPtrQAbstractButtonBoolVoid")
  [ptrT $ objT c_QAbstractButton, boolT] voidT

cb_PtrQAbstractItemModelVoid =
  makeCallback (toExtName "CallbackPtrQAbstractItemModelVoid")
  [ptrT $ objT c_QAbstractItemModel] voidT

cb_PtrQActionVoid =
  makeCallback (toExtName "CallbackPtrQActionVoid")
  [ptrT $ objT c_QAction] voidT

cb_PtrQGraphicsItemPtrQEventBool =
  makeCallback (toExtName "CallbackPtrQGraphicsItemPtrQEventBool")
  [ptrT $ objT c_QGraphicsItem, ptrT $ objT c_QEvent] boolT

cb_PtrQObjectPtrQEventBool =
  makeCallback (toExtName "CallbackPtrQObjectPtrQEventBool")
  [ptrT $ objT c_QObject, ptrT $ objT c_QEvent] boolT

cb_PtrQObjectVoid =
  makeCallback (toExtName "CallbackPtrQObjectVoid")
  [ptrT $ objT c_QObject] voidT

cb_PtrQPaintEventVoid =
  makeCallback (toExtName "CallbackPtrQPaintEventVoid")
  [ptrT $ objT c_QPaintEvent] voidT

cb_PtrQTreeWidgetItemVoid :: Callback
cb_PtrQTreeWidgetItemVoid =
  makeCallback (toExtName "CallbackPtrQTreeWidgetItemVoid")
  [ptrT $ objT c_QTreeWidgetItem] voidT

cb_PtrQTreeWidgetItemIntVoid :: Callback
cb_PtrQTreeWidgetItemIntVoid =
  makeCallback (toExtName "CallbackPtrQTreeWidgetItemIntVoid")
  [ptrT $ objT c_QTreeWidgetItem, intT] voidT

cb_PtrQTreeWidgetItemPtrQTreeWidgetItemVoid :: Callback
cb_PtrQTreeWidgetItemPtrQTreeWidgetItemVoid =
  makeCallback (toExtName "CallbackPtrQTreeWidgetItemPtrQTreeWidgetItemVoid")
  [ptrT $ objT c_QTreeWidgetItem, ptrT $ objT c_QTreeWidgetItem] voidT

cb_PtrQWidgetPtrQWidgetVoid =
  makeCallback (toExtName "CallbackPtrQWidgetPtrQWidgetVoid")
  [ptrT $ objT c_QWidget, ptrT $ objT c_QWidget] voidT

cb_QAbstractSliderActionVoid =
  makeCallback (toExtName "CallbackQAbstractSliderActionVoid")
  [enumT e_SliderAction] voidT

cb_QClipboardModeVoid =
  makeCallback (toExtName "CallbackQClipboardModeVoid")
  [enumT QClipboard.e_Mode] voidT

cb_QDateVoid =
  makeCallback (toExtName "CallbackQDateVoid")
  [objT c_QDate] voidT

cb_QModelIndexVoid =
  makeCallback (toExtName "CallbackQModelIndexVoid")
  [objT c_QModelIndex] voidT

cb_QModelIndexIntIntVoid =
  makeCallback (toExtName "CallbackQModelIndexIntIntVoid")
  [objT c_QModelIndex, intT, intT] voidT

cb_QModelIndexIntIntQModelIndexIntVoid =
  makeCallback (toExtName "CallbackQModelIndexIntIntQModelIndexIntVoid")
  [objT c_QModelIndex, intT, intT, objT c_QModelIndex, intT] voidT

cb_QModelIndexQModelIndexVoid =
  makeCallback (toExtName "CallbackQModelIndexQModelIndexVoid")
  [objT c_QModelIndex, objT c_QModelIndex] voidT

cb_QModelIndexQModelIndexQVectorIntVoid =
  makeCallback (toExtName "CallbackQModelIndexQModelIndexQVectorIntVoid")
  [objT c_QModelIndex, objT c_QModelIndex, toGcT $ objT c_QVectorInt] voidT

cb_QWindowVisibilityVoid =
  makeCallback (toExtName "CallbackQWindowVisibilityVoid")
  [enumT QWindow.e_Visibility] voidT

cb_QPointVoid =
  makeCallback (toExtName "CallbackQPointVoid")
  [objT c_QPoint] voidT

cb_QrealVoid =
  makeCallback (toExtName "CallbackQrealVoid")
  [qreal] voidT

cb_QSizeVoid =
  makeCallback (toExtName "CallbackQSizeVoid")
  [objT c_QSize] voidT

cb_QStringVoid =
  makeCallback (toExtName "CallbackQStringVoid")
  [objT c_QString] voidT

cb_QSystemTrayIconActivationReasonVoid =
  makeCallback (toExtName "CallbackQSystemTrayIconActivationReasonVoid")
  [enumT e_ActivationReason] voidT

cb_RefConstQIconVoid =
  makeCallback (toExtName "CallbackRefConstQIconVoid")
  [refT $ constT $ objT c_QIcon] voidT

cb_RefConstQItemSelectionRefConstQItemSelectionVoid =
  makeCallback (toExtName "CallbackRefConstQItemSelectionRefConstQItemSelectionVoid")
  [refT $ constT $ objT c_QItemSelection, refT $ constT $ objT c_QItemSelection] voidT

cb_ScreenOrientationVoid =
  makeCallback (toExtName "CallbackScreenOrientationVoid")
  [enumT e_ScreenOrientation] voidT

cb_ToolBarAreasVoid =
  makeCallback (toExtName "CallbackToolBarAreasVoid")
  [bitspaceT bs_ToolBarAreas] voidT

cb_ToolButtonStyleVoid =
  makeCallback (toExtName "CallbackToolButtonStyleVoid")
  [enumT e_ToolButtonStyle] voidT

cb_WindowModalityVoid =
  makeCallback (toExtName "CallbackWindowModalityVoid")
  [enumT e_WindowModality] voidT

cb_WindowStateVoid =
  makeCallback (toExtName "CallbackWindowStateVoid")
  [enumT e_WindowState] voidT

cb_Void =
  makeCallback (toExtName "CallbackVoid")
  [] voidT
