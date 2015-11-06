-- This file is part of Qtah.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License version 3
-- as published by the Free Software Foundation.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Graphics.UI.Qtah.Internal.Interface.Widgets.QLabel (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Type (TBitspace, TBool, TDouble, TEnum, TInt, TObj, TPtr, TVoid),
  addReqIncludes,
  ident,
  includeStd,
  makeClass,
  mkBoolHasProp,
  mkConstMethod,
  mkCtor,
  mkMethod',
  mkMethod,
  mkProp,
  mkProps,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (bs_Alignment, e_TextFormat)
import Graphics.UI.Qtah.Internal.Interface.Listener (c_ListenerQString)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QFrame (c_QFrame)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QLabel"] $
  QtExport (ExportClass c_QLabel) :
  map QtExportSignal signals

c_QLabel =
  addReqIncludes [includeStd "QLabel"] $
  makeClass (ident "QLabel") Nothing [c_QFrame]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  , mkCtor "newWithText" [TObj c_QString]
  , mkCtor "newWithTextAndParent" [TObj c_QString, TPtr $ TObj c_QWidget]
    -- Ctors taking Qt::WindowFlags.
  ] $
  [ mkMethod "clear" [] TVoid
  , mkConstMethod "hasSelectedText" [] TBool
  , mkConstMethod "selectedText" [] $ TObj c_QString
  , mkConstMethod "selectionStart" [] TInt
  , mkMethod' "setNum" "setInt" [TInt] TVoid
  , mkMethod' "setNum" "setDouble" [TDouble] TVoid
  , mkMethod "setSelection" [TInt, TInt] TVoid
  ] ++
  mkProps
  [ mkProp "alignment" $ TBitspace bs_Alignment
  , mkProp "buddy" $ TPtr $ TObj c_QWidget
  , mkProp "indent" TInt
  , mkProp "margin" TInt
    -- TODO movie
  , mkProp "openExternalLinks" TBool
    -- TODO picture
    -- TODO pixmap
  , mkBoolHasProp "scaledContents"
  , mkProp "text" $ TObj c_QString
  , mkProp "textFormat" $ TEnum e_TextFormat
    -- TODO textInteractionFlags
  , mkProp "wordWrap" TBool
  ]

signals =
  [ makeSignal c_QLabel "linkActivated" c_ListenerQString
  , makeSignal c_QLabel "linkHovered" c_ListenerQString
  ]
