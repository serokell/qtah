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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractItemView (
  aModule,
  c_QAbstractItemView,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportBitspace, ExportEnum, ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkBoolHasProp,
  mkConstMethod,
  mkConstMethod',
  mkMethod,
  mkMethod',
  mkProp,
  )
import Foreign.Hoppy.Generator.Types (
  bitspaceT,
  boolT,
  enumT,
  intT,
  objT,
  ptrT,
  voidT,
  )
import Graphics.UI.Qtah.Generator.Interface.Core.QAbstractItemModel (c_QAbstractItemModel)
import Graphics.UI.Qtah.Generator.Interface.Core.QItemSelectionModel (c_QItemSelectionModel)
import Graphics.UI.Qtah.Generator.Interface.Core.QModelIndex (c_QModelIndex)
import Graphics.UI.Qtah.Generator.Interface.Core.QPoint (c_QPoint)
import Graphics.UI.Qtah.Generator.Interface.Core.QRect (c_QRect)
import Graphics.UI.Qtah.Generator.Interface.Core.QSize (c_QSize)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (
  e_DropAction,
  e_TextElideMode,
  )
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (
  c_Listener,
  c_ListenerQModelIndex,
  c_ListenerQSize,
  )
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractItemDelegate (c_QAbstractItemDelegate)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractScrollArea (c_QAbstractScrollArea)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QAbstractItemView"] $
  QtExport (ExportClass c_QAbstractItemView) :
  map QtExportSignal signals ++
  [ QtExport $ ExportEnum e_DragDropMode
  , QtExport $ ExportEnum e_EditTrigger
  , QtExport $ ExportBitspace bs_EditTriggers
  , QtExport $ ExportEnum e_ScrollHint
  , QtExport $ ExportEnum e_ScrollMode
  , QtExport $ ExportEnum e_SelectionBehavior
  , QtExport $ ExportEnum e_SelectionMode
  ]

c_QAbstractItemView =
  addReqIncludes [includeStd "QAbstractItemView"] $
  classSetEntityPrefix "" $
  makeClass (ident "QAbstractItemView") Nothing [c_QAbstractScrollArea] $
  [ mkProp "alternatingRowColors" boolT
  , mkBoolHasProp "autoScroll"
  , mkProp "autoScrollMargin" intT
  , mkMethod "clearSelection" [] voidT
  , mkMethod "closePersistentEditor" [objT c_QModelIndex] voidT
  , mkProp "currentIndex" $ objT c_QModelIndex
  , mkProp "defaultDropAction" $ enumT e_DropAction
  , mkProp "dragDropMode" $ enumT e_DragDropMode
  , mkProp "dragDropOverwriteMode" boolT
  , mkProp "dragEnabled" boolT
  , mkMethod "edit" [objT c_QModelIndex] voidT
  , mkProp "editTriggers" $ bitspaceT bs_EditTriggers
  , mkProp "horizontalScrollMode" $ enumT e_ScrollMode
  , mkProp "iconSize" $ objT c_QSize
  , mkConstMethod "indexAt" [objT c_QPoint] $ objT c_QModelIndex
  , mkConstMethod "indexWidget" [objT c_QModelIndex] $ ptrT $ objT c_QWidget
  , mkConstMethod' "itemDelegate" "itemDelegate" [] $
    ptrT $ objT c_QAbstractItemDelegate
  , mkConstMethod' "itemDelegate" "itemDelegateAt" [objT c_QModelIndex] $
    ptrT $ objT c_QAbstractItemDelegate
  , mkConstMethod "itemDelegateForColumn" [intT] $
    ptrT $ objT c_QAbstractItemDelegate
  , mkConstMethod "itemDelegateForRow" [intT] $
    ptrT $ objT c_QAbstractItemDelegate
  , mkMethod "keyboardSearch" [objT c_QString] voidT
  , mkProp "model" $ ptrT $ objT c_QAbstractItemModel
  , mkMethod "openPersistentEditor" [objT c_QModelIndex] voidT
  , mkMethod "reset" [] voidT
  , mkProp "rootIndex" $ objT c_QModelIndex
  , mkMethod' "scrollTo" "scrollTo" [objT c_QModelIndex] voidT
  , mkMethod' "scrollTo" "scrollToWithHint" [objT c_QModelIndex, enumT e_ScrollHint] voidT
  , mkMethod "scrollToBottom" [] voidT
  , mkMethod "scrollToTop" [] voidT
  , mkMethod "selectAll" [] voidT
  , mkProp "selectionBehavior" $ enumT e_SelectionBehavior
  , mkProp "selectionMode" $ enumT e_SelectionMode
  , mkProp "selectionModel" $ ptrT $ objT c_QItemSelectionModel
  , mkMethod "setDropIndicatorShown" [boolT] voidT
  , mkMethod "setIndexWidget" [objT c_QModelIndex, ptrT $ objT c_QWidget] voidT
  , mkMethod "setItemDelegate" [ptrT $ objT c_QAbstractItemDelegate] voidT
  , mkMethod "setItemDelegateForColumn" [intT, ptrT $ objT c_QAbstractItemDelegate] voidT
  , mkMethod "setItemDelegateForRow" [intT, ptrT $ objT c_QAbstractItemDelegate] voidT
  , mkConstMethod "showDropIndicator" [] boolT
  , mkConstMethod "sizeHintForColumn" [intT] intT
  , mkConstMethod "sizeHintForIndex" [objT c_QModelIndex] $ objT c_QSize
  , mkConstMethod "sizeHintForRow" [intT] intT
  , mkProp "tabKeyNavigation" boolT
  , mkProp "textElideMode" $ enumT e_TextElideMode
  , mkMethod "update" [objT c_QModelIndex] voidT
  , mkProp "verticalScrollMode" $ enumT e_ScrollMode
  , mkConstMethod "visualRect" [objT c_QModelIndex] $ objT c_QRect
  ]

signals =
  [ makeSignal c_QAbstractItemView "activated" c_ListenerQModelIndex
  , makeSignal c_QAbstractItemView "clicked" c_ListenerQModelIndex
  , makeSignal c_QAbstractItemView "doubleClicked" c_ListenerQModelIndex
  , makeSignal c_QAbstractItemView "entered" c_ListenerQModelIndex
  , makeSignal c_QAbstractItemView "iconSizeChanged" c_ListenerQSize
  , makeSignal c_QAbstractItemView "pressed" c_ListenerQModelIndex
  , makeSignal c_QAbstractItemView "viewportEntered" c_Listener
  ]

e_DragDropMode =
  makeQtEnum (ident1 "QAbstractItemView" "DragDropMode") [includeStd "QAbstractItemView"]
  [ (0, ["no", "drag", "drop"])
  , (1, ["drag", "only"])
  , (2, ["drop", "only"])
  , (3, ["drag", "drop"])
  , (4, ["internal", "move"])
  ]

(e_EditTrigger, bs_EditTriggers) =
  makeQtEnumBitspace (ident1 "QAbstractItemView" "EditTrigger") "EditTriggers"
  [includeStd "QAbstractItemView"]
  [ (0, ["no", "edit", "triggers"])
  , (1, ["current", "changed"])
  , (2, ["double", "clicked"])
  , (4, ["selected", "clicked"])
  , (8, ["edit", "key", "pressed"])
  , (16, ["any", "key", "pressed"])
  , (31, ["all", "edit", "triggers"])
  ]

e_ScrollHint =
  makeQtEnum (ident1 "QAbstractItemView" "ScrollHint") [includeStd "QAbstractItemView"]
  [ (0, ["ensure", "visible"])
  , (1, ["position", "at", "top"])
  , (2, ["position", "at", "bottom"])
  , (3, ["position", "at", "center"])
  ]

e_ScrollMode =
  makeQtEnum (ident1 "QAbstractItemView" "ScrollMode") [includeStd "QAbstractItemView"]
  [ (0, ["scroll", "per", "item"])
  , (1, ["scroll", "per", "pixel"])
  ]

e_SelectionBehavior =
  makeQtEnum (ident1 "QAbstractItemView" "SelectionBehavior") [includeStd "QAbstractItemView"]
  [ (0, ["select", "items"])
  , (1, ["select", "rows"])
  , (2, ["select", "columns"])
  ]

e_SelectionMode =
  makeQtEnum (ident1 "QAbstractItemView" "SelectionMode") [includeStd "QAbstractItemView"]
  [ (0, ["no", "selection"])
  , (1, ["single", "selection"])
  , (2, ["multi", "selection"])
  , (3, ["extended", "selection"])
  , (4, ["contiguous", "selection"])
  ]
