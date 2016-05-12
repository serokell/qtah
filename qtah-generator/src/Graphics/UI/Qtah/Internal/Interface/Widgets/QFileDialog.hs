-- This file is part of Qtah.
--
-- Copyright 2015-2016 Bryan Gardiner <bog@khumba.net>
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

module Graphics.UI.Qtah.Internal.Interface.Widgets.QFileDialog (
  aModule,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Export (ExportBitspace, ExportEnum, ExportClass),
  Type (TBitspace, TBool, TEnum, TObj, TPtr, TVoid),
  addReqIncludes,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkCtor,
  mkMethod,
  mkMethod',
  mkProp,
  mkProps,
  mkStaticMethod',
  )
import Graphics.UI.Qtah.Internal.Flag (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QDir (bs_Filters, c_QDir)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Internal.Interface.Core.QStringList (c_QStringList)
import Graphics.UI.Qtah.Internal.Interface.Core.Types (bs_WindowFlags)
import Graphics.UI.Qtah.Internal.Interface.Listener (c_ListenerQString)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QDialog (c_QDialog)
import Graphics.UI.Qtah.Internal.Interface.Widgets.QWidget (c_QWidget)

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
  makeClass (ident "QFileDialog") Nothing [c_QDialog]
  [ mkCtor "new" []
  , mkCtor "newWithParent" [TPtr $ TObj c_QWidget]
  , mkCtor "newWithParentAndFlags" [TPtr $ TObj c_QWidget, TBitspace bs_WindowFlags]
  , mkCtor "newWithParentAndCaption" [TPtr $ TObj c_QWidget, TObj c_QString]
  , mkCtor "newWithParentAndCaptionAndDirectory"
    [TPtr $ TObj c_QWidget, TObj c_QString, TObj c_QString]
  , mkCtor "newWithParentAndCaptionAndDirectoryAndFilter"
    [TPtr $ TObj c_QWidget, TObj c_QString, TObj c_QString, TObj c_QString]
  ] $
  collect
  [ just $ mkConstMethod "directory" [] $ TObj c_QDir
    -- TODO directoryUrl (>=5.2)
  , just $ mkStaticMethod' "getExistingDirectory" "getExistingDirectory"
    [TPtr $ TObj c_QWidget, TObj c_QString, TObj c_QString] $ TObj c_QString
  , just $ mkStaticMethod' "getExistingDirectory" "getExistingDirectoryWithOptions"
    [TPtr $ TObj c_QWidget, TObj c_QString, TObj c_QString, TBitspace bs_Options] $ TObj c_QString
    -- TODO getExistingDirectoryUrl (>=5.2)
  , just $ mkStaticMethod' "getOpenFileName" "getOpenFileName"
    [TPtr $ TObj c_QWidget, TObj c_QString, TObj c_QString, TObj c_QString] $ TObj c_QString
  , just $ mkStaticMethod' "getOpenFileName" "getOpenFileNameWithOptions"
    [TPtr $ TObj c_QWidget, TObj c_QString, TObj c_QString,
     TObj c_QString, TPtr $ TObj c_QString, TBitspace bs_Options] $
    TObj c_QString
  , just $ mkStaticMethod' "getOpenFileNames" "getOpenFileNames"
    [TPtr $ TObj c_QWidget, TObj c_QString, TObj c_QString, TObj c_QString] $ TObj c_QStringList
  , just $ mkStaticMethod' "getOpenFileNames" "getOpenFileNamesWithOptions"
    [TPtr $ TObj c_QWidget, TObj c_QString, TObj c_QString,
     TObj c_QString, TPtr $ TObj c_QString, TBitspace bs_Options] $
    TObj c_QStringList
    -- TODO getOpenFileUrl (>=5.2)
    -- TODO getOpenFileUrls (>=5.2)
  , just $ mkStaticMethod' "getSaveFileName" "getSaveFileName"
    [TPtr $ TObj c_QWidget, TObj c_QString, TObj c_QString, TObj c_QString] $ TObj c_QString
  , just $ mkStaticMethod' "getSaveFileName" "getSaveFileNameWithOptions"
    [TPtr $ TObj c_QWidget, TObj c_QString, TObj c_QString,
     TObj c_QString, TPtr $ TObj c_QString, TBitspace bs_Options] $
    TObj c_QString
    -- TODO getSaveFileUrl (>=5.2)

    -- TODO iconProvider
    -- TODO itemDelegate
  , just $ mkConstMethod "labelText" [TEnum e_DialogLabel] $ TObj c_QString
    -- TODO open (>=4.5)
    -- TODO proxyModel
    -- TODO restoreState (>=4.3)
    -- TODO saveState (>=4.3)
  , just $ mkMethod "selectFile" [TObj c_QString] TVoid
    -- TODO selectMimeTypeFilter (>=5.2)
  , just $ mkMethod "selectNameFilter" [TObj c_QString] TVoid
    -- TODO selectUrl (>=5.2)
  , just $ mkConstMethod "selectedFiles" [] $ TObj c_QStringList
  , test (qtVersion >= [4, 4]) $ mkConstMethod "selectedNameFilter" [] $ TObj c_QString
    -- TODO selectedUrls (>=5.2)
  , just $ mkMethod' "setDirectory" "setDirectory" [TObj c_QDir] TVoid
  , just $ mkMethod' "setDirectory" "setDirectoryPath" [TObj c_QString] TVoid
    -- TODO setDirectoryUrl(QUrl) (>=5.2)
    -- TODO setHistory
    -- TODO setIconProvider
    -- TODO setItemDelegate
  , just $ mkMethod "setLabelText" [TEnum e_DialogLabel, TObj c_QString] TVoid
  , test (qtVersion >= [4, 4]) $ mkMethod "setNameFilter" [TObj c_QString] TVoid
  , test (qtVersion >= [4, 5]) $ mkMethod "setOption" [TEnum e_Option, TBool] TVoid
    -- TODO setProxyModel (>=4.3)
    -- TODO testOption (>=4.5)
  ] ++
  (mkProps . collect)
  [ just $ mkProp "acceptMode" $ TEnum e_AcceptMode
  , just $ mkProp "defaultSuffix" $ TObj c_QString
  , just $ mkProp "fileMode" $ TEnum e_FileMode
  , test (qtVersion >= [4, 4]) $ mkProp "filter" $ TBitspace bs_Filters
  , just $ mkProp "history" $ TObj c_QStringList
  , test (qtVersion >= [5, 2]) $ mkProp "mimeTypeFilters" $ TObj c_QStringList
  , test (qtVersion >= [4, 4]) $ mkProp "nameFilters" $ TObj c_QStringList
  , test (qtVersion >= [4, 5]) $ mkProp "options" $ TBitspace bs_Options
    -- TODO sidebarUrls (>=4.3)
  , just $ mkProp "viewMode" $ TEnum e_ViewMode
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
