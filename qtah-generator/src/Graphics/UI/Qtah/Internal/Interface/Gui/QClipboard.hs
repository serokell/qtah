-- This file is part of Qtah.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
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

module Graphics.UI.Qtah.Internal.Interface.Gui.QClipboard (
  aModule,
  c_QClipboard,
  e_Mode,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportEnum, ExportClass),
  Type (TBool, TEnum, TObj, TRef, TVoid),
  addReqIncludes,
  classSetDtorPrivate,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkMethod',
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QObject (c_QObject)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Listener (
  c_Listener,
  c_ListenerQClipboardMode,
  )

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QClipboard"] $
  (QtExport $ ExportClass c_QClipboard) :
  map QtExportSignal signals ++
  [ QtExport $ ExportEnum e_Mode ]

c_QClipboard =
  addReqIncludes [includeStd "QClipboard"] $
  classSetDtorPrivate $
  makeClass (ident "QClipboard") Nothing [c_QObject] [] $
  collect
  [ just $ mkMethod' "clear" "clear" [] TVoid
  , just $ mkMethod' "clear" "clearWithMode" [TEnum e_Mode] TVoid
    -- TODO image
    -- TODO mimeData
  , just $ mkConstMethod "ownsClipboard" [] TBool
  , test (qtVersion >= [4, 2]) $ mkConstMethod "ownsFindBuffer" [] TBool
  , just $ mkConstMethod "ownsSelection" [] TBool
    -- TODO pixmap
    -- TODO setImage
    -- TODO setMimeData
    -- TODO setPixmap
  , just $ mkMethod' "setText" "setText" [TObj c_QString] TVoid
  , just $ mkMethod' "setText" "setTextWithMode" [TObj c_QString, TEnum e_Mode] TVoid
  , just $ mkConstMethod "supportsFindBuffer" [] TBool
  , just $ mkConstMethod "supportsSelection" [] TBool
  , just $ mkConstMethod' "text" "text" [] $ TObj c_QString
  , just $ mkConstMethod' "text" "textWithMode" [TEnum e_Mode] $ TObj c_QString
  , just $ mkConstMethod' "text" "textSubtype" [TRef $ TObj c_QString] $ TObj c_QString
  , just $ mkConstMethod' "text" "textSubtypeWithMode" [TRef $ TObj c_QString, TEnum e_Mode] $
    TObj c_QString
  ]

signals =
  collect
  [ test (qtVersion >= [4, 2]) $ makeSignal c_QClipboard "changed" c_ListenerQClipboardMode
  , just $ makeSignal c_QClipboard "dataChanged" c_Listener
  , test (qtVersion >= [4, 2]) $ makeSignal c_QClipboard "findBufferChanged" c_Listener
  , just $ makeSignal c_QClipboard "selectionChanged" c_Listener
  ]

e_Mode =
  makeQtEnum (ident1 "QClipboard" "Mode") [includeStd "QClipboard"]
  [ (0, ["clipboard"])
  , (1, ["selection"])
  , (2, ["find", "buffer"])
  ]
