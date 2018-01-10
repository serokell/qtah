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

module Graphics.UI.Qtah.Generator.Types (
  QtExport (..),
  qtExportToExport,
  makeQtEnum,
  makeQtEnumBitspace,
  Signal, makeSignal, makeSignal',
  signalCName, signalHaskellName, signalClass, signalListenerClass,
  ) where

import Foreign.Hoppy.Generator.Spec (
  Bitspace,
  Class,
  CppEnum,
  Export (ExportClass, ExportFn),
  Function,
  Identifier,
  Include,
  addReqIncludes,
  bitspaceAddCppType,
  bitspaceAddEnum,
  bitspaceSetValuePrefix,
  enumSetValuePrefix,
  identifierParts,
  identT,
  idPartBase,
  includeStd,
  makeBitspace,
  makeEnum,
  toExtName,
  )
import Foreign.Hoppy.Generator.Types (enumT, intT)

data QtExport =
  QtExport Export
  | QtExportFnRenamed Function String
  | QtExportSignal Signal
  | QtExportEvent Class
  | QtExportSceneEvent Class
  | QtExportSpecials
    -- ^ This is a special value that is exported exactly once, and generates
    -- some bindings that need special logic.

qtExportToExport :: QtExport -> Maybe Export
qtExportToExport qtExport = case qtExport of
  QtExport export -> Just export
  QtExportFnRenamed fn _ -> Just $ ExportFn fn
  QtExportSignal {} -> Nothing
  QtExportEvent cls -> Just $ ExportClass cls
  QtExportSceneEvent cls -> Just $ ExportClass cls
  QtExportSpecials -> Nothing

-- | Creates a 'CppEnum' whose 'ExtName' is the concatenation of all part of its
-- 'Identifier'.  This should be used for all Qt enums.
makeQtEnum :: Identifier -> [Include] -> [(Int, [String])] -> CppEnum
makeQtEnum identifier includes valueNames =
  addReqIncludes includes $
  enumSetValuePrefix "" $
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
      bitspaceAddCppType (identT "QFlags" [enumT enum])
                         (Just "QFlag")
                         (Just "int") $
      bitspaceAddEnum enum $
      bitspaceSetValuePrefix "" $
      makeBitspace bitspaceExtName intT valueNames)
  where replaceLast _ [] = []
        replaceLast y [_] = [y]
        replaceLast y (x:xs) = x:replaceLast y xs

-- | Specification for a signal in the Qt signals and slots framework.
data Signal = Signal
  { signalClass :: Class
    -- ^ The class to which the signal belongs.
  , signalCName :: String
    -- ^ The C name of the signal, without parameters, e.g. @"clicked"@.
  , signalHaskellName :: String
    -- ^ The base name of the Haskell binding for the signal.  Normally the same
    -- as the C name.
  , signalListenerClass :: Class
    -- ^ An appropriately typed listener class.
  }

makeSignal :: Class  -- ^ 'signalClass'
           -> String  -- ^ 'signalCName'
           -> Class  -- ^ 'signalListenerClass'
           -> Signal
makeSignal cls cName = Signal cls cName cName

makeSignal' :: Class  -- ^ 'signalClass'
            -> String  -- ^ 'signalCName'
            -> String  -- ^ 'signalHaskellName'
            -> Class  -- ^ 'signalListenerClass'
            -> Signal
makeSignal' = Signal
