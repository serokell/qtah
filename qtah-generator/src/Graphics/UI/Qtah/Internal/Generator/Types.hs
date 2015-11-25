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

module Graphics.UI.Qtah.Internal.Generator.Types (
  moduleNameAppend,
  AModule (..),
  aModuleHoppy,
  QtModule,
  makeQtModule,
  qtModulePath,
  qtModuleQtExports,
  qtModuleHoppy,
  QtExport (..),
  makeQtEnum,
  makeQtEnumBitspace,
  Signal, makeSignal, signalCName, signalClass, signalListenerClass,
  ) where

import Data.Char (toLower)
import Data.Maybe (mapMaybe)
import Foreign.Hoppy.Generator.Spec (
  Bitspace,
  Class,
  CppEnum,
  Export (ExportFn),
  Function,
  Identifier,
  Include,
  Module,
  Type (TEnum, TInt),
  addReqIncludes,
  bitspaceAddCppType,
  bitspaceAddEnum,
  identifierParts,
  identT,
  idPartBase,
  includeStd,
  makeBitspace,
  makeEnum,
  makeModule,
  moduleAddExports,
  moduleAddHaskellName,
  moduleModify',
  toExtName,
  )

moduleNameAppend :: String -> String -> String
moduleNameAppend "" y = y
moduleNameAppend x "" = x
moduleNameAppend x y = concat [x, ".", y]

-- | A union of Hoppy and Qt modules.
data AModule = AHoppyModule Module | AQtModule QtModule

-- | Extracts the Hoppy 'Module' for an 'AModule'.
aModuleHoppy :: AModule -> Module
aModuleHoppy (AHoppyModule m) = m
aModuleHoppy (AQtModule qm) = qtModuleHoppy qm

-- | A @QtModule@ (distinct from a Hoppy 'Module'), is a description of a
-- Haskell module in the @Graphics.UI.Qtah.Q@ namespace that:
--
--     1. reexports 'Export's from a Hoppy module, dropping @ClassName_@
--        prefixes from the reexported names.
--     2. generates Signal definitions for Qt signals.
data QtModule = QtModule
  { qtModulePath :: [String]
  , qtModuleQtExports :: [QtExport]
    -- ^ A list of exports whose generated Hoppy bindings will be re-exported in
    -- this module.
  , qtModuleHoppy :: Module
  }

makeQtModule :: [String] -> [QtExport] -> QtModule
makeQtModule [] _ = error "makeQtModule: Module path must be nonempty."
makeQtModule modulePath@(_:moduleNameParts) qtExports =
  let lowerName = map toLower $ concat moduleNameParts
  in QtModule
     { qtModulePath = modulePath
     , qtModuleQtExports = qtExports
     , qtModuleHoppy =
       moduleModify' (makeModule lowerName
                     (concat ["b_", lowerName, ".hpp"])
                     (concat ["b_", lowerName, ".cpp"])) $ do
         moduleAddHaskellName modulePath
         moduleAddExports $ mapMaybe qtExportToExport qtExports
     }

qtExportToExport :: QtExport -> Maybe Export
qtExportToExport qtExport = case qtExport of
  QtExport export -> Just export
  QtExportFnRenamed fn _ -> Just $ ExportFn fn
  QtExportSignal {} -> Nothing

data QtExport =
  QtExport Export
  | QtExportFnRenamed Function String
  | QtExportSignal Signal

-- | Creates a 'CppEnum' whose 'ExtName' is the concatenation of all part of its
-- 'Identifier'.  This should be used for all Qt enums.
makeQtEnum :: Identifier -> [Include] -> [(Int, [String])] -> CppEnum
makeQtEnum identifier includes valueNames =
  addReqIncludes includes $
  makeEnum identifier
           (Just $ toExtName $ concat $ map idPartBase $ identifierParts identifier)
           valueNames

-- | Creates an (enum, bitspace) pair with the same values and similar names,
-- and whose enum values can be converted to bitspace values.
makeQtEnumBitspace :: Identifier -> String -> [Include] -> [(Int, [String])] -> (CppEnum, Bitspace)
makeQtEnumBitspace identifier bitspaceName includes valueNames =
  let enum = makeQtEnum identifier includes valueNames
      bitspaceExtName = toExtName $ concat $
                        replaceLast bitspaceName $
                        map idPartBase (identifierParts identifier)
  in (enum,
      addReqIncludes (includeStd "QFlag" : includeStd "QFlags" : includes) $
      bitspaceAddCppType (identT "QFlags" [TEnum enum])
                         (Just "QFlag")
                         (Just "int") $
      bitspaceAddEnum enum $
      makeBitspace bitspaceExtName TInt valueNames)
  where replaceLast _ [] = []
        replaceLast y [_] = [y]
        replaceLast y (x:xs) = x:replaceLast y xs

-- | Specification for a signal in the Qt signals and slots framework.
data Signal = Signal
  { signalClass :: Class
    -- ^ The class to which the signal belongs.
  , signalCName :: String
    -- ^ The C name of the signal, without parameters, e.g. @"clicked"@.
  , signalListenerClass :: Class
    -- ^ An appropriately typed listener class.
  }

makeSignal :: Class  -- ^ 'signalClass'
           -> String  -- ^ 'signalCName'
           -> Class  -- ^ 'signalListenerClass'
           -> Signal
makeSignal = Signal
