-- This file is part of Qtah.
--
-- Copyright 2016 Bryan Gardiner <bog@khumba.net>
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

module Graphics.UI.Qtah.Internal.Interface.Gui.QKeyEvent (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Type (TBitspace, TBool, TEnum, TInt, TObj, TUShort, TWord32),
  addReqIncludes,
  ident,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  )
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QEvent (e_Type)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (bs_KeyboardModifiers)
import Graphics.UI.Qtah.Internal.Interface.Gui.QInputEvent (c_QInputEvent)

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Gui", "QKeyEvent"]
  [ QtExportEvent c_QKeyEvent
  ]

c_QKeyEvent =
  addReqIncludes [includeStd "QKeyEvent"] $
  makeClass (ident "QKeyEvent") Nothing [c_QInputEvent]
  (collect
   [ just $ mkCtor "new" [TEnum e_Type, TInt, TBitspace bs_KeyboardModifiers]
   , just $ mkCtor "newWithText"
     [TEnum e_Type, TInt, TBitspace bs_KeyboardModifiers, TObj c_QString, TBool, TUShort]
   , test (qtVersion >= [5, 0]) $ mkCtor "newNative"
     [TEnum e_Type, TInt, TBitspace bs_KeyboardModifiers, TWord32, TWord32, TWord32]
   , test (qtVersion >= [5, 0]) $ mkCtor "newNativeWithText"
     [TEnum e_Type, TInt, TBitspace bs_KeyboardModifiers, TWord32, TWord32, TWord32,
      TObj c_QString, TBool, TUShort]
   ]) $
  collect
  [ just $ mkConstMethod "count" [] TInt
  , just $ mkConstMethod "isAutoRepeat" [] TBool
  , just $ mkConstMethod "key" [] TInt
    -- TODO matches (>=4.2)
  , just $ mkConstMethod "modifiers" [] $ TBitspace bs_KeyboardModifiers
  , test (qtVersion >= [4, 2]) $ mkConstMethod "nativeModifiers" [] TWord32
  , test (qtVersion >= [4, 2]) $ mkConstMethod "nativeScanCode" [] TWord32
  , test (qtVersion >= [4, 2]) $ mkConstMethod "nativeVirtualKey" [] TWord32
  , just $ mkConstMethod "text" [] $ TObj c_QString
  ]
