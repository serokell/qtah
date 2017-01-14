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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QLabel (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  includeStd,
  makeClass,
  mkBoolHasProp,
  mkConstMethod,
  mkCtor,
  mkMethod',
  mkMethod,
  mkProp,
  )
import Foreign.Hoppy.Generator.Types (bitspaceT, boolT, doubleT, enumT, intT, objT, ptrT, voidT)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (bs_Alignment, e_TextFormat)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (c_ListenerQString)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QFrame (c_QFrame)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QLabel"] $
  QtExport (ExportClass c_QLabel) :
  map QtExportSignal signals

c_QLabel =
  addReqIncludes [includeStd "QLabel"] $
  classSetEntityPrefix "" $
  makeClass (ident "QLabel") Nothing [c_QFrame]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , mkCtor "newWithText" [objT c_QString]
  , mkCtor "newWithTextAndParent" [objT c_QString, ptrT $ objT c_QWidget]
    -- TODO Ctors taking Qt::WindowFlags.
  , mkProp "alignment" $ bitspaceT bs_Alignment
  , mkProp "buddy" $ ptrT $ objT c_QWidget
  , mkMethod "clear" [] voidT
  , mkConstMethod "hasSelectedText" [] boolT
  , mkProp "indent" intT
  , mkProp "margin" intT
    -- TODO movie
  , mkProp "openExternalLinks" boolT
    -- TODO picture
    -- TODO pixmap
  , mkBoolHasProp "scaledContents"
  , mkConstMethod "selectedText" [] $ objT c_QString
  , mkConstMethod "selectionStart" [] intT
  , mkMethod' "setNum" "setInt" [intT] voidT
  , mkMethod' "setNum" "setDouble" [doubleT] voidT
  , mkMethod "setSelection" [intT, intT] voidT
  , mkProp "text" $ objT c_QString
  , mkProp "textFormat" $ enumT e_TextFormat
    -- TODO textInteractionFlags
  , mkProp "wordWrap" boolT
  ]

signals =
  [ makeSignal c_QLabel "linkActivated" c_ListenerQString
  , makeSignal c_QLabel "linkHovered" c_ListenerQString
  ]
