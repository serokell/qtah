-- This file is part of Qtah.
--
-- Copyright 2018 The Qtah Authors.
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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QDateTimeEdit (
  aModule,
  c_QDateTimeEdit,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportBitspace, ExportClass, ExportEnum),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkProp,
  )
import Foreign.Hoppy.Generator.Types (bitspaceT, boolT, enumT, intT, objT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QDate (c_QDate)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (c_ListenerQDate)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QAbstractSpinBox (
  c_QAbstractSpinBox,
  )
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QDateTimeEdit"] $
  QtExport (ExportClass c_QDateTimeEdit) :
  map QtExportSignal signals ++
  (map QtExport . collect)
  [ just $ ExportEnum e_Section
  , just $ ExportBitspace bs_Sections
  ]

c_QDateTimeEdit =
  addReqIncludes [includeStd "QDateTimeEdit"] $
  classSetEntityPrefix "" $
  makeClass (ident "QDateTimeEdit") Nothing [c_QAbstractSpinBox] $
  collect
  [
  -- Properties
    test (qtVersion >= [4, 2]) $ mkProp "calendarPopup" boolT
  , just $ mkProp "currentSection" (enumT e_Section)
  , test (qtVersion >= [4, 3]) $ mkProp "currentSectionIndex" intT
  , just $ mkProp "date" (objT c_QDate)
  -- TODO just $ mkProp "dateTime" (objT c_QDateTime)
  , just $ mkProp "displayFormat" (objT c_QString)
  , just $ mkConstMethod "displayedSections" [] (bitspaceT bs_Sections)
  , just $ mkProp "maximumDate" (objT c_QDate)
  -- TODO test (qtVersion >= [4, 4]) $
  --      mkProp "maximumDateTime" (objT c_QDateTime)
  -- TODO just $ mkProp "maximumTime" c_QTime
  , just $ mkProp "minimumDate" (objT c_QDate)
  -- TODO test (qtVersion >= [4, 4]) $
  --      mkProp "minimumDateTime" (objT c_QDateTime)
  -- TODO just $ mkProp "minimumTime" c_QTime
  , test (qtVersion >= [4, 3]) $ mkConstMethod "sectionCount" [] intT
  -- TODO just $ mkProp "time" c_QTime
  -- TODO test (qtVersion >= [4, 4]) $ mkProp "timeSpec" Qt.TimeSpec
  -- Public Functions
  , just $ mkCtor "new" []
  -- TODO Other methods.
  ]

signals =
  [ makeSignal c_QDateTimeEdit "dateChanged" c_ListenerQDate
  -- TODO void dateTimeChanged(const QDateTime &datetime)
  -- TODO void timeChanged(const QTime &time)
  ]

(e_Section, bs_Sections) = makeQtEnumBitspace
  (ident1 "QDateTimeEdit" "Section")
  "Sections"
  [includeStd "QDateTimeEdit"]
  [ (0x0000, ["no", "section"])
  , (0x0001, ["am", "pm", "section"])
  , (0x0002, ["m", "sec", "section"])
  , (0x0004, ["second", "section"])
  , (0x0008, ["minute", "section"])
  , (0x0010, ["hour", "section"])
  , (0x0100, ["day", "section"])
  , (0x0200, ["month", "section"])
  , (0x0400, ["year", "section"])
  ]
