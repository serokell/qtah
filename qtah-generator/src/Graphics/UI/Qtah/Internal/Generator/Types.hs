module Graphics.UI.Qtah.Internal.Generator.Types (
  moduleNameAppend,
  QtModule,
  makeHoppyModule,
  makeQtModule,
  makeQtModuleForClass,
  qtModuleSubname,
  qtModuleQtExports,
  qtModuleExports,
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
  Export (ExportClass, ExportFn),
  Function,
  Identifier,
  Module,
  Type (TEnum, TInt),
  addModuleHaskellName,
  addModuleExports,
  addReqIncludes,
  bitspaceAddCppType,
  bitspaceAddEnum,
  classExtName,
  fromExtName,
  identifierParts,
  identT,
  idPartBase,
  includeStd,
  makeBitspace,
  makeEnum,
  makeModule,
  modifyModule',
  toExtName,
  )

moduleNameAppend :: String -> String -> String
moduleNameAppend "" y = y
moduleNameAppend x "" = x
moduleNameAppend x y = concat [x, ".", y]

-- | A @QtModule@ (distinct from a Hoppy 'Module'), is a description of a
-- Haskell module in the @Graphics.UI.Qtah.Q@ namespace that:
--
--     1. reexports 'Export's from a Hoppy module, dropping @ClassName_@
--        prefixes from the reexported names.
--     2. generates Signal definitions for Qt signals.
data QtModule = QtModule
  { qtModuleSubname :: String
  , qtModuleQtExports :: [QtExport]
    -- ^ A list of exports whose generated Hoppy bindings will be re-exported in
    -- this module.
  }

makeHoppyModule :: String -> String -> QtModule -> Module
makeHoppyModule moduleParentName moduleBaseName qtModule =
  let lowerBaseName = map toLower moduleBaseName
  in modifyModule' (makeModule lowerBaseName
                    (concat ["b_", lowerBaseName, ".hpp"])
                    (concat ["b_", lowerBaseName, ".cpp"])) $ do
    addModuleHaskellName [moduleParentName, moduleBaseName]
    addModuleExports $ qtModuleExports qtModule

makeQtModule :: String -> [QtExport] -> QtModule
makeQtModule = QtModule

makeQtModuleForClass :: Class -> [QtExport] -> QtModule
makeQtModuleForClass cls exports =
  QtModule (fromExtName $ classExtName cls) $
  QtExport (ExportClass cls) : exports

qtModuleExports :: QtModule -> [Export]
qtModuleExports = mapMaybe getExport . qtModuleQtExports
  where getExport qtExport = case qtExport of
          QtExport export -> Just export
          QtExportFnRenamed fn _ -> Just $ ExportFn fn
          QtExportSignal {} -> Nothing

data QtExport =
  QtExport Export
  | QtExportFnRenamed Function String
  | QtExportSignal Signal

-- | Creates a 'CppEnum' whose 'ExtName' is the concatenation of all part of its
-- 'Identifier'.  This should be used for all Qt enums.
makeQtEnum :: Identifier -> [(Int, [String])] -> CppEnum
makeQtEnum identifier =
  makeEnum identifier $ Just $ toExtName $ concat $
  map idPartBase $ identifierParts identifier

-- | Creates an (enum, bitspace) pair with the same values and similar names,
-- and whose enum values can be converted to bitspace values.
makeQtEnumBitspace :: Identifier -> String -> [(Int, [String])] -> (CppEnum, Bitspace)
makeQtEnumBitspace identifier bitspaceName valueNames =
  let enum = makeQtEnum identifier valueNames
      bitspaceExtName = toExtName $ concat $
                        replaceLast bitspaceName $
                        map idPartBase (identifierParts identifier)
  in (enum,
      addReqIncludes [includeStd "QFlag", includeStd "QFlags"] $
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
