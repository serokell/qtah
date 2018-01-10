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

module Graphics.UI.Qtah.Generator.Interface.Core.QDir (
  aModule,
  c_QDir,
  e_Filter,
  bs_Filters,
  e_SortFlag,
  bs_SortFlags,
  ) where

import Data.Bits ((.|.))
import Foreign.Hoppy.Generator.Spec (
  Export (ExportBitspace, ExportEnum, ExportClass),
  Operator (OpArray),
  addReqIncludes,
  classSetConversionToGc,
  classSetEntityPrefix,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkProp,
  mkStaticMethod,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Foreign.Hoppy.Generator.Types (bitspaceT, boolT, intT, objT, refT, voidT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QChar (c_QChar)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModule)
import Graphics.UI.Qtah.Generator.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

aModule =
  AQtModule $
  makeQtModule ["Core", "QDir"]
  [ QtExport $ ExportClass c_QDir
  , QtExport $ ExportEnum e_Filter
  , QtExport $ ExportBitspace bs_Filters
  , QtExport $ ExportEnum e_SortFlag
  , QtExport $ ExportBitspace bs_SortFlags
  ]

c_QDir =
  addReqIncludes [includeStd "QDir"] $
  classSetConversionToGc $
  classAddFeatures [Assignable, Copyable, Equatable] $
  classSetEntityPrefix "" $
  makeClass (ident "QDir") Nothing [] $
  collect
  [ just $ mkCtor "new" [objT c_QString]
  , just $ mkConstMethod "absoluteFilePath" [objT c_QString] $ objT c_QString
  , just $ mkConstMethod "absolutePath" [] $ objT c_QString
  , test (qtVersion >= [4, 3]) $
    mkStaticMethod "addSearchPath" [objT c_QString, objT c_QString] voidT
  , just $ mkConstMethod "canonicalPath" [] $ objT c_QString
  , just $ mkMethod "cd" [objT c_QString] boolT
  , just $ mkMethod "cdUp" [] boolT
  , just $ mkStaticMethod "cleanPath" [objT c_QString] $ objT c_QString
  , just $ mkConstMethod "count" [] intT
  , just $ mkStaticMethod "current" [] $ objT c_QDir
  , just $ mkStaticMethod "currentPath" [] $ objT c_QString
  , just $ mkConstMethod "dirName" [] $ objT c_QString
    -- TODO drives
    -- TODO entryInfoList
    -- TODO entryList
  , just $ mkConstMethod' "exists" "exists" [] boolT
  , just $ mkConstMethod' "exists" "entryExists" [objT c_QString] boolT
  , just $ mkConstMethod "filePath" [objT c_QString] $ objT c_QString
  , just $ mkProp "filter" $ bitspaceT bs_Filters
  , test (qtVersion >= [4, 2]) $
    mkStaticMethod "fromNativeSeparators" [objT c_QString] $ objT c_QString
  , just $ mkStaticMethod "home" [] $ objT c_QDir
  , just $ mkStaticMethod "homePath" [] $ objT c_QString
  , just $ mkConstMethod "isAbsolute" [] boolT
  , just $ mkStaticMethod "isAbsolutePath" [objT c_QString] boolT
  , just $ mkConstMethod "isReadable" [] boolT
  , just $ mkConstMethod "isRelative" [] boolT
  , just $ mkStaticMethod "isRelativePath" [objT c_QString] boolT
  , just $ mkConstMethod "isRoot" [] boolT
  , just $ mkMethod "makeAbsolute" [] boolT
  , just $ mkStaticMethod "match" [objT c_QString, objT c_QString] boolT
    -- TODO match(QStringList, QString)
  , just $ mkConstMethod "mkdir" [objT c_QString] boolT
  , just $ mkConstMethod "mkpath" [objT c_QString] boolT
    -- TODO nameFilters
  , just $ mkProp "path" $ objT c_QString
  , just $ mkMethod "refresh" [] voidT
  , just $ mkConstMethod "relativeFilePath" [objT c_QString] $ objT c_QString
  , just $ mkMethod "remove" [objT c_QString] boolT
  , test (qtVersion >= [5, 0]) $ mkMethod "removeRecursively" [] boolT
  , just $ mkMethod "rename" [objT c_QString, objT c_QString] boolT
  , just $ mkConstMethod "rmdir" [objT c_QString] boolT
  , just $ mkConstMethod "rmpath" [objT c_QString] boolT
  , just $ mkStaticMethod "root" [] $ objT c_QDir
  , just $ mkStaticMethod "rootPath" [] $ objT c_QString
    -- TODO searchPaths (>=4.3)
  , just $ mkStaticMethod "separator" [] $ objT c_QChar
  , just $ mkStaticMethod "setCurrent" [objT c_QString] boolT
  , just $ mkProp "sorting" $ bitspaceT bs_SortFlags
  , test (qtVersion >= [5, 0]) $ mkMethod "swap" [refT $ objT c_QDir] voidT
  , just $ mkStaticMethod "temp" [] $ objT c_QDir
  , just $ mkStaticMethod "tempPath" [] $ objT c_QString
  , test (qtVersion >= [4, 2]) $
    mkStaticMethod "toNativeSeparators" [objT c_QString] $ objT c_QString
  , just $ mkConstMethod OpArray [intT] $ objT c_QString
  ]

(e_Filter, bs_Filters) =
  makeQtEnumBitspace (ident1 "QDir" "Filter") "Filters" [includeStd "QDir"] $
  let dirs = 0x1
      allDirs = 0x400
      files = 0x2
      drives = 0x4
      noSymLinks = 0x8
      noDotAndDotDot = noDot .|. noDotDot
      noDot = 0x2000
      noDotDot = 0x4000
      allEntries = dirs .|. files .|. drives
      readable = 0x10
      writable = 0x20
      executable = 0x40
      modified = 0x80
      hidden = 0x100
      system = 0x200
      caseSensitive = 0x800
  in [ (dirs, ["dirs"])
     , (allDirs, ["all", "dirs"])
     , (files, ["files"])
     , (drives, ["drives"])
     , (noSymLinks, ["no", "sym", "links"])
     , (noDotAndDotDot, ["no", "dot", "and", "dot", "dot"])
     , (noDot, ["no", "dot"])
     , (noDotDot, ["no", "dot", "dot"])
     , (allEntries, ["all", "entries"])
     , (readable, ["readable"])
     , (writable, ["writable"])
     , (executable, ["executable"])
     , (modified, ["modified"])
     , (hidden, ["hidden"])
     , (system, ["system"])
     , (caseSensitive, ["case", "sensitive"])
     ]

(e_SortFlag, bs_SortFlags) =
  makeQtEnumBitspace (ident1 "QDir" "SortFlag") "SortFlags" [includeStd "QDir"]
  [ (0x00, ["name"])
  , (0x01, ["time"])
  , (0x02, ["size"])
  , (0x80, ["typ"])  -- "type" is a Haskell keyword.
  , (0x03, ["unsorted"])
    -- QDir::NoSort = -1.  Not sure this is needed (it's used for parameter
    -- defaults).  Would need to check if negative values work as expected.
  , (0x04, ["dirs", "first"])
  , (0x20, ["dirs", "last"])
  , (0x08, ["reversed"])
  , (0x10, ["ignore", "case"])
  , (0x40, ["locale", "aware"])
  ]
