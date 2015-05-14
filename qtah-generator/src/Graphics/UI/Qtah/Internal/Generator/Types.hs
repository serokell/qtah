module Graphics.UI.Qtah.Internal.Generator.Types (
  moduleNameAppend,
  QtModule, makeQtModule, makeQtModuleForClass, qtModuleSubname, qtModuleQtExports, qtModuleExports,
  QtExport (..),
  Signal, makeSignal, signalCName, signalClass, signalListenerClass,
  ) where

import Data.Maybe (mapMaybe)
import Foreign.Cppop.Generator.Spec (
  Class,
  Export (ExportClass),
  classExtName,
  fromExtName,
  )

moduleNameAppend :: String -> String -> String
moduleNameAppend "" y = y
moduleNameAppend x "" = x
moduleNameAppend x y = concat [x, ".", y]

-- | A @QtModule@ (distinct from a Cppop 'Module'), is a description of a
-- Haskell module in the @Graphics.UI.Qtah.Q@ namespace that:
--
--     1. reexports 'Export's from a Cppop module, dropping @ClassName_@
--        prefixes from the reexported names.
--     2. generates Signal definitions for Qt signals.
data QtModule = QtModule
  { qtModuleSubname :: String
  , qtModuleQtExports :: [QtExport]
    -- ^ A list of exports whose generated Cppop bindings will be re-exported in
    -- this module.
  }

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
          QtExportSignal {} -> Nothing

data QtExport = QtExport Export | QtExportSignal Signal

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
