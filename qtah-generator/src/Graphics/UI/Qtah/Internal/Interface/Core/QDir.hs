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

module Graphics.UI.Qtah.Internal.Interface.Core.QDir (
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
  Type (TBitspace, TBool, TInt, TObj, TObjToHeap, TRef, TVoid),
  addReqIncludes,
  ident,
  ident1,
  includeStd,
  makeClass,
  mkConstMethod,
  mkConstMethod',
  mkCtor,
  mkMethod,
  mkProp,
  mkProps,
  mkStaticMethod,
  )
import Foreign.Hoppy.Generator.Spec.ClassFeature (
  ClassFeature (Assignable, Copyable, Equatable),
  classAddFeatures,
  )
import Graphics.UI.Qtah.Internal.Flag (collect, just, test)
import Graphics.UI.Qtah.Internal.Flags (qtVersion)
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Interface.Core.QChar (c_QChar)
import Graphics.UI.Qtah.Internal.Interface.Core.QString (c_QString)

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
  classAddFeatures [Assignable, Copyable, Equatable] $
  makeClass (ident "QDir") Nothing []
  [ mkCtor "new" [TObj c_QString]
  ] $
  collect
  [ just $ mkConstMethod "absoluteFilePath" [TObj c_QString] $ TObj c_QString
  , just $ mkConstMethod "absolutePath" [] $ TObj c_QString
  , test (qtVersion >= [4, 3]) $
    mkStaticMethod "addSearchPath" [TObj c_QString, TObj c_QString] TVoid
  , just $ mkConstMethod "canonicalPath" [] $ TObj c_QString
  , just $ mkMethod "cd" [TObj c_QString] TBool
  , just $ mkMethod "cdUp" [] TBool
  , just $ mkStaticMethod "cleanPath" [TObj c_QString] $ TObj c_QString
  , just $ mkConstMethod "count" [] TInt
  , just $ mkStaticMethod "current" [] $ TObjToHeap c_QDir
  , just $ mkStaticMethod "currentPath" [] $ TObj c_QString
  , just $ mkConstMethod "dirName" [] $ TObj c_QString
    -- TODO drives
    -- TODO entryInfoList
    -- TODO entryList
  , just $ mkConstMethod' "exists" "exists" [] TBool
  , just $ mkConstMethod' "exists" "entryExists" [TObj c_QString] TBool
  , just $ mkConstMethod "filePath" [TObj c_QString] $ TObj c_QString
  , test (qtVersion >= [4, 2]) $
    mkStaticMethod "fromNativeSeparators" [TObj c_QString] $ TObj c_QString
  , just $ mkStaticMethod "home" [] $ TObjToHeap c_QDir
  , just $ mkStaticMethod "homePath" [] $ TObj c_QString
  , just $ mkConstMethod "isAbsolute" [] TBool
  , just $ mkStaticMethod "isAbsolutePath" [TObj c_QString] TBool
  , just $ mkConstMethod "isReadable" [] TBool
  , just $ mkConstMethod "isRelative" [] TBool
  , just $ mkStaticMethod "isRelativePath" [TObj c_QString] TBool
  , just $ mkConstMethod "isRoot" [] TBool
  , just $ mkMethod "makeAbsolute" [] TBool
  , just $ mkStaticMethod "match" [TObj c_QString, TObj c_QString] TBool
    -- TODO match(QStringList, QString)
  , just $ mkConstMethod "mkdir" [TObj c_QString] TBool
  , just $ mkConstMethod "mkpath" [TObj c_QString] TBool
  , just $ mkMethod "refresh" [] TVoid
  , just $ mkConstMethod "relativeFilePath" [TObj c_QString] $ TObj c_QString
  , just $ mkMethod "remove" [TObj c_QString] TBool
  , test (qtVersion >= [5, 0]) $ mkMethod "removeRecursively" [] TBool
  , just $ mkMethod "rename" [TObj c_QString, TObj c_QString] TBool
  , just $ mkConstMethod "rmdir" [TObj c_QString] TBool
  , just $ mkConstMethod "rmpath" [TObj c_QString] TBool
  , just $ mkStaticMethod "root" [] $ TObjToHeap c_QDir
  , just $ mkStaticMethod "rootPath" [] $ TObj c_QString
  , just $ mkStaticMethod "separator" [] $ TObj c_QChar
  , just $ mkStaticMethod "setCurrent" [TObj c_QString] TBool
  , test (qtVersion >= [5, 0]) $ mkMethod "swap" [TRef $ TObj c_QDir] TVoid
  , just $ mkStaticMethod "temp" [] $ TObjToHeap c_QDir
  , just $ mkStaticMethod "tempPath" [] $ TObj c_QString
  , test (qtVersion >= [4, 2]) $
    mkStaticMethod "toNativeSeparators" [TObj c_QString] $ TObj c_QString
  , just $ mkConstMethod OpArray [TInt] $ TObj c_QString
  ] ++
  mkProps
  [ mkProp "filter" $ TBitspace bs_Filters
    -- TODO nameFilters
  , mkProp "path" $ TObj c_QString
    -- TODO searchPaths (>=4.3)
  , mkProp "sorting" $ TBitspace bs_SortFlags
  ]

(e_Filter, bs_Filters) =
  makeQtEnumBitspace (ident1 "QDir" "Filter") "Filters" $
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
  makeQtEnumBitspace (ident1 "QDir" "SortFlag") "SortFlags"
  [ (0x00, ["name"])
  , (0x01, ["time"])
  , (0x02, ["size"])
  , (0x80, ["type"])
  , (0x03, ["unsorted"])
    -- QDir::NoSort = -1.  Not sure this is needed (it's used for parameter
    -- defaults).  Would need to check if negative values work as expected.
  , (0x04, ["dirs", "first"])
  , (0x20, ["dirs", "last"])
  , (0x08, ["reversed"])
  , (0x10, ["ignore", "case"])
  , (0x40, ["locale", "aware"])
  ]
