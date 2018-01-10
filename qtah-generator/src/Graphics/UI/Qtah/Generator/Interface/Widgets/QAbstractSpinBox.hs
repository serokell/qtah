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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractSpinBox (
  aModule,
  c_QAbstractSpinBox,
  e_ButtonSymbols,
  e_CorrectionMode,
  e_StepEnabledFlag,
  bs_StepEnabled,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportBitspace, ExportClass, ExportEnum),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkBoolIsProp,
  mkBoolHasProp,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkProp,
  )
import Foreign.Hoppy.Generator.Types (bitspaceT, boolT, enumT, intT, objT, ptrT, refT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (bs_Alignment)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (c_Listener)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QAbstractSpinBox"] $
  QtExport (ExportClass c_QAbstractSpinBox) :
  map QtExportSignal signals ++
  (map QtExport . collect)
  [ test (qtVersion >= [4, 2]) $ ExportEnum e_ButtonSymbols
  , just $ ExportEnum e_CorrectionMode
  , just $ ExportEnum e_StepEnabledFlag
  , just $ ExportBitspace bs_StepEnabled
  ]

c_QAbstractSpinBox =
  addReqIncludes [includeStd "QAbstractSpinBox"] $
  classSetEntityPrefix "" $
  makeClass (ident "QAbstractSpinBox") Nothing [c_QWidget] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , test (qtVersion >= [4, 2]) $ mkBoolIsProp "accelerated"
  , just $ mkProp "alignment" $ bitspaceT bs_Alignment
  , test (qtVersion >= [4, 2]) $ mkProp "buttonSymbols" $ enumT e_ButtonSymbols
  , just $ mkMethod "clear" [] voidT
  , just $ mkProp "correctionMode" $ enumT e_CorrectionMode
  , test (qtVersion >= [4, 2]) $ mkConstMethod "hasAcceptableInput" [] boolT
  , just $ mkConstMethod "fixup" [refT $ objT c_QString] voidT
  , test (qtVersion >= [4, 3]) $ mkBoolHasProp "frame"
  , test (qtVersion >= [5, 3]) $ mkBoolIsProp "groupSeparatorShown"
  , just $ mkMethod "interpretText" [] voidT
  , just $ mkProp "keyboardTracking" boolT
  , just $ mkBoolIsProp "readOnly"
  , just $ mkMethod "selectAll" [] voidT
  , just $ mkProp "specialValueText" $ objT c_QString
  , just $ mkMethod "stepBy" [intT] voidT
  , just $ mkMethod "stepDown" [] voidT
  , just $ mkMethod "stepUp" [] voidT
  , just $ mkConstMethod "text" [] $ objT c_QString
    -- TODO validate
  , just $ mkProp "wrapping" boolT
  ]

signals =
  [ makeSignal c_QAbstractSpinBox "editingFinished" c_Listener
  ]

e_ButtonSymbols =
  addReqIncludes [includeStd "QAbstractSpinBox"] $
  makeQtEnum (ident1 "QAbstractSpinBox" "ButtonSymbols") [includeStd "QAbstractSpinBox"]
  [ (0, ["up", "down", "arrows"])
  , (1, ["plus", "minus"])
  , (2, ["no", "buttons"])
  ]

e_CorrectionMode =
  makeQtEnum (ident1 "QAbstractSpinBox" "CorrectionMode") [includeStd "QAbstractSpinBox"]
  [ (0, ["correct", "to", "previous", "value"])
  , (1, ["correct", "to", "nearest", "value"])
  ]

(e_StepEnabledFlag, bs_StepEnabled) =
  makeQtEnumBitspace (ident1 "QAbstractSpinBox" "StepEnabledFlag") "StepEnabled"
  [includeStd "QAbstractSpinBox"]
  [ (0x0, ["step", "none"])
  , (0x1, ["step", "up", "enabled"])
  , (0x2, ["step", "down", "enabled"])
  ]
