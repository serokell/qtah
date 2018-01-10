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

module Graphics.UI.Qtah.Generator.Interface.Widgets.QFileDialog (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportBitspace, ExportEnum, ExportClass),
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  mkStaticMethod',
  )
import Foreign.Hoppy.Generator.Types (bitspaceT, boolT, enumT, objT, ptrT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QDir (bs_Filters, c_QDir)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QStringList (c_QStringList)
import Graphics.UI.Qtah.Generator.Interface.Core.Types (bs_WindowFlags)
import Graphics.UI.Qtah.Generator.Interface.Internal.Listener (c_ListenerQString)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QDialog (c_QDialog)
import Graphics.UI.Qtah.Generator.Interface.Widgets.QWidget (c_QWidget)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Widgets", "QFileDialog"] $
  [ QtExport $ ExportClass c_QFileDialog
  , QtExport $ ExportEnum e_AcceptMode
  , QtExport $ ExportEnum e_DialogLabel
  , QtExport $ ExportEnum e_FileMode
  , QtExport $ ExportEnum e_Option
  , QtExport $ ExportBitspace bs_Options
  , QtExport $ ExportEnum e_ViewMode
  ] ++ map QtExportSignal signals

c_QFileDialog =
  addReqIncludes [includeStd "QFileDialog"] $
  classSetEntityPrefix "" $
  makeClass (ident "QFileDialog") Nothing [c_QDialog] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QWidget]
  , just $ mkCtor "newWithParentAndFlags" [ptrT $ objT c_QWidget, bitspaceT bs_WindowFlags]
  , just $ mkCtor "newWithParentAndCaption" [ptrT $ objT c_QWidget, objT c_QString]
  , just $ mkCtor "newWithParentAndCaptionAndDirectory"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString]
  , just $ mkCtor "newWithParentAndCaptionAndDirectoryAndFilter"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString, objT c_QString]
  , just $ mkProp "acceptMode" $ enumT e_AcceptMode
  , just $ mkProp "defaultSuffix" $ objT c_QString
  , just $ mkConstMethod "directory" [] $ objT c_QDir
    -- TODO directoryUrl (>=5.2)
  , just $ mkProp "fileMode" $ enumT e_FileMode
  , test (qtVersion >= [4, 4]) $ mkProp "filter" $ bitspaceT bs_Filters
  , just $ mkStaticMethod' "getExistingDirectory" "getExistingDirectory"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString] $ objT c_QString
  , just $ mkStaticMethod' "getExistingDirectory" "getExistingDirectoryWithOptions"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString, bitspaceT bs_Options] $ objT c_QString
    -- TODO getExistingDirectoryUrl (>=5.2)
  , just $ mkStaticMethod' "getOpenFileName" "getOpenFileName"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString, objT c_QString] $ objT c_QString
  , just $ mkStaticMethod' "getOpenFileName" "getOpenFileNameWithOptions"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString,
     objT c_QString, ptrT $ objT c_QString, bitspaceT bs_Options] $
    objT c_QString
  , just $ mkStaticMethod' "getOpenFileNames" "getOpenFileNames"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString, objT c_QString] $ objT c_QStringList
  , just $ mkStaticMethod' "getOpenFileNames" "getOpenFileNamesWithOptions"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString,
     objT c_QString, ptrT $ objT c_QString, bitspaceT bs_Options] $
    objT c_QStringList
    -- TODO getOpenFileUrl (>=5.2)
    -- TODO getOpenFileUrls (>=5.2)
  , just $ mkStaticMethod' "getSaveFileName" "getSaveFileName"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString, objT c_QString] $ objT c_QString
  , just $ mkStaticMethod' "getSaveFileName" "getSaveFileNameWithOptions"
    [ptrT $ objT c_QWidget, objT c_QString, objT c_QString,
     objT c_QString, ptrT $ objT c_QString, bitspaceT bs_Options] $
    objT c_QString
    -- TODO getSaveFileUrl (>=5.2)
  , just $ mkProp "history" $ objT c_QStringList
    -- TODO iconProvider
    -- TODO itemDelegate
  , just $ mkConstMethod "labelText" [enumT e_DialogLabel] $ objT c_QString
  , test (qtVersion >= [5, 2]) $ mkProp "mimeTypeFilters" $ objT c_QStringList
  , test (qtVersion >= [4, 4]) $ mkProp "nameFilters" $ objT c_QStringList
    -- TODO open (>=4.5)
  , test (qtVersion >= [4, 5]) $ mkProp "options" $ bitspaceT bs_Options
    -- TODO proxyModel
    -- TODO restoreState (>=4.3)
    -- TODO saveState (>=4.3)
  , just $ mkMethod "selectFile" [objT c_QString] voidT
    -- TODO selectMimeTypeFilter (>=5.2)
  , just $ mkMethod "selectNameFilter" [objT c_QString] voidT
    -- TODO selectUrl (>=5.2)
  , just $ mkConstMethod "selectedFiles" [] $ objT c_QStringList
  , test (qtVersion >= [4, 4]) $ mkConstMethod "selectedNameFilter" [] $ objT c_QString
    -- TODO selectedUrls (>=5.2)
  , just $ mkMethod' "setDirectory" "setDirectory" [objT c_QDir] voidT
  , just $ mkMethod' "setDirectory" "setDirectoryPath" [objT c_QString] voidT
    -- TODO setDirectoryUrl(QUrl) (>=5.2)
    -- TODO setHistory
    -- TODO setIconProvider
    -- TODO setItemDelegate
  , just $ mkMethod "setLabelText" [enumT e_DialogLabel, objT c_QString] voidT
  , test (qtVersion >= [4, 4]) $ mkMethod "setNameFilter" [objT c_QString] voidT
  , test (qtVersion >= [4, 5]) $ mkMethod "setOption" [enumT e_Option, boolT] voidT
    -- TODO setProxyModel (>=4.3)
    -- TODO sidebarUrls (>=4.3)
    -- TODO testOption (>=4.5)
  , just $ mkProp "viewMode" $ enumT e_ViewMode
  ]

signals =
  collect
  [ just $ makeSignal c_QFileDialog "currentChanged" c_ListenerQString
    -- TODO currentUrlChanged (>=5.2)
  , just $ makeSignal c_QFileDialog "directoryEntered" c_ListenerQString
    -- TODO directoryUrlEntered (>=5.2)
  , just $ makeSignal c_QFileDialog "fileSelected" c_ListenerQString
    -- TODO filesSelected
  , test (qtVersion >= [4, 3]) $ makeSignal c_QFileDialog "filterSelected" c_ListenerQString
    -- TODO urlSelected (>=5.2)
    -- TODO urlsSelected (>=5.2)
  ]

e_AcceptMode =
  makeQtEnum (ident1 "QFileDialog" "AcceptMode") [includeStd "QFileDialog"]
  [ (0, ["accept", "open"])
  , (1, ["accept", "save"])
  ]

e_DialogLabel =
  makeQtEnum (ident1 "QFileDialog" "DialogLabel") [includeStd "QFileDialog"]
  [ (0, ["look", "in"])
  , (1, ["file", "name"])
  , (2, ["file", "type"])
  , (3, ["accept"])
  , (4, ["reject"])
  ]

e_FileMode =
  makeQtEnum (ident1 "QFileDialog" "FileMode") [includeStd "QFileDialog"] $
  collect
  [ just (0, ["any", "file"])
  , just (1, ["existing", "file"])
  , just (2, ["directory"])
  , just (3, ["existing", "files"])
  , test (qtVersion < [4, 5]) $ (4, ["directory", "only"])
  ]

(e_Option, bs_Options) =
  makeQtEnumBitspace (ident1 "QFileDialog" "Option") "Options" [includeStd "QFileDialog"]
  [ (0x1, ["show", "dirs", "only"])
  , (0x2, ["dont", "resolve", "symlinks"])
  , (0x4, ["dont", "confirm", "overwrite"])
  , (0x8, ["dont", "use", "sheet"])
  , (0x10, ["dont", "use", "native", "dialog"])
  , (0x20, ["read", "only"])
  , (0x40, ["hide", "name", "filter", "details"])
  , (0x80, ["dont", "use", "custom", "directory", "icons"])
  ]

e_ViewMode =
  makeQtEnum (ident1 "QFileDialog" "ViewMode") [includeStd "QFileDialog"]
  [ (0, ["detail"])
  , (1, ["list"])
  ]
