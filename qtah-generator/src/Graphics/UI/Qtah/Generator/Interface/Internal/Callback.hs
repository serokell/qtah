-- This file is part of Qtah.
--
-- Copyright 2015-2017 Bryan Gardiner <bog@khumba.net>
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
  Export (ExportCallback),
  makeCallback,
  makeModule,
  moduleAddExports,
  moduleAddHaskellName,
  moduleModify',
  toExtName,
  )
import Foreign.Hoppy.Generator.Types (boolT, doubleT, enumT, intT, objT, ptrT, toGcT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Core.QEvent (c_QEvent)
import Graphics.UI.Qtah.Generator.Interface.Core.QModelIndex (c_QModelIndex)
import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QVector (c_QVectorInt)
import {-# SOURCE #-} qualified Graphics.UI.Qtah.Generator.Interface.Gui.QClipboard as QClipboard
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractButton (
  c_QAbstractButton,
  )
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractSlider (
  e_SliderAction,
  )
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QAction (c_QAction)
import {-# SOURCE #-} Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AHoppyModule))

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AHoppyModule $
  moduleModify' (makeModule "callback" "b_callback.hpp" "b_callback.cpp") $ do
    moduleAddHaskellName ["Internal", "Callback"]
    moduleAddExports
      [ ExportCallback cb_BoolVoid
      , ExportCallback cb_DoubleVoid
      , ExportCallback cb_IntVoid
      , ExportCallback cb_IntBoolVoid
      , ExportCallback cb_IntIntVoid
      , ExportCallback cb_PtrQAbstractButtonVoid
      , ExportCallback cb_PtrQAbstractButtonBoolVoid
      , ExportCallback cb_PtrQActionVoid
      , ExportCallback cb_PtrQObjectPtrQEventBool
      , ExportCallback cb_PtrQObjectVoid
      , ExportCallback cb_PtrQWidgetPtrQWidgetVoid
      , ExportCallback cb_QAbstractSliderActionVoid
      , ExportCallback cb_QClipboardModeVoid
      , ExportCallback cb_QModelIndexIntIntVoid
      , ExportCallback cb_QModelIndexIntIntQModelIndexIntVoid
      , ExportCallback cb_QModelIndexQModelIndexQVectorIntVoid
      , ExportCallback cb_QPointVoid
      , ExportCallback cb_QSizeVoid
      , ExportCallback cb_QStringVoid
      , ExportCallback cb_Void
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

cb_PtrQAbstractButtonVoid =
  makeCallback (toExtName "CallbackPtrQAbstractButtonVoid")
  [ptrT $ objT c_QAbstractButton] voidT

cb_PtrQAbstractButtonBoolVoid =
  makeCallback (toExtName "CallbackPtrQAbstractButtonBoolVoid")
  [ptrT $ objT c_QAbstractButton, boolT] voidT

cb_PtrQActionVoid =
  makeCallback (toExtName "CallbackPtrQActionVoid")
  [ptrT $ objT c_QAction] voidT

cb_PtrQObjectPtrQEventBool =
  makeCallback (toExtName "CallbackPtrQObjectPtrQEventBool")
  [ptrT $ objT c_QObject, ptrT $ objT c_QEvent] boolT

cb_PtrQObjectVoid =
  makeCallback (toExtName "CallbackPtrQObjectVoid")
  [ptrT $ objT c_QObject] voidT

cb_PtrQWidgetPtrQWidgetVoid =
  makeCallback (toExtName "CallbackPtrQWidgetPtrQWidgetVoid")
  [ptrT $ objT c_QWidget, ptrT $ objT c_QWidget] voidT

cb_QAbstractSliderActionVoid =
  makeCallback (toExtName "CallbackQAbstractSliderActionVoid")
  [enumT e_SliderAction] voidT

cb_QClipboardModeVoid =
  makeCallback (toExtName "CallbackQClipboardModeVoid")
  [enumT QClipboard.e_Mode] voidT

cb_QModelIndexIntIntVoid =
  makeCallback (toExtName "CallbackQModelIndexIntIntVoid")
  [objT c_QModelIndex, intT, intT] voidT

cb_QModelIndexIntIntQModelIndexIntVoid =
  makeCallback (toExtName "CallbackQModelIndexIntIntQModelIndexIntVoid")
  [objT c_QModelIndex, intT, intT, objT c_QModelIndex, intT] voidT

cb_QModelIndexQModelIndexQVectorIntVoid =
  makeCallback (toExtName "CallbackQModelIndexQModelIndexQVectorIntVoid")
  [objT c_QModelIndex, objT c_QModelIndex, toGcT $ objT c_QVectorInt] voidT

cb_QPointVoid =
  makeCallback (toExtName "CallbackQPointVoid")
  [objT c_QPoint] voidT

cb_QSizeVoid =
  makeCallback (toExtName "CallbackQSizeVoid")
  [objT c_QSize] voidT

cb_QStringVoid =
  makeCallback (toExtName "CallbackQStringVoid")
  [objT c_QString] voidT

cb_Void =
  makeCallback (toExtName "CallbackVoid")
  [] voidT
