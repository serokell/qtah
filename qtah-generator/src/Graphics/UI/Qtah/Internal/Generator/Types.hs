module Graphics.UI.Qtah.Internal.Generator.Types (
  QtModule, makeQtModule, qtModuleSubname, qtModuleExports,
  moduleNameAppend,
  QtExport (..),
  qtExportToExport,
  QtClass, makeQtClass, makeQtClass', qtClassClass, qtClassSignals,
  Signal, makeSignal, signalCName, signalExtName, signalClass, signalListenerClass,
  ) where

import Foreign.Cppop.Generator.Spec (
  Callback,
  Class,
  CppEnum,
  Ctor,
  Export (ExportCallback, ExportClass, ExportEnum, ExportFn),
  ExtName,
  Function,
  Identifier,
  Method,
  makeClass,
  )

data QtModule = QtModule
  { qtModuleSubname :: String
  , qtModuleExports :: [QtExport]
  }

makeQtModule :: String -> [QtExport] -> QtModule
makeQtModule = QtModule

moduleNameAppend :: String -> String -> String
moduleNameAppend "" y = y
moduleNameAppend x "" = x
moduleNameAppend x y = concat [x, ".", y]

-- | A data type that wraps a Cppop 'Export' and adds support for 'QtClass'es.
data QtExport =
  QtExportEnum CppEnum
  | QtExportFn Function
  | QtExportClass QtClass
  | QtExportCallback Callback

qtExportToExport :: QtExport -> Export
qtExportToExport export = case export of
  QtExportEnum enum -> ExportEnum enum
  QtExportFn fn -> ExportFn fn
  QtExportClass qtCls -> ExportClass $ qtClassClass qtCls
  QtExportCallback cb -> ExportCallback cb

-- | A @QtClass@ is a 'Class' that also may have 'Signal's.
data QtClass = QtClass
  { qtClassClass :: Class
  , qtClassSignals :: [Signal]
  }

makeQtClass :: Identifier  -- ^ The class's C++ identifier.
            -> Maybe ExtName
            -- ^ An optional external name; will be automatically derived from
            -- the identifier if absent.
            -> [Class]  -- ^ Superclasses.
            -> [Ctor]
            -> [Method]
            -> [Signal]
            -> QtClass
makeQtClass identifier maybeExtName supers ctors methods signals = QtClass
  { qtClassClass = makeClass identifier maybeExtName supers ctors methods
  , qtClassSignals = signals
  }

makeQtClass' :: [Signal] -> Class -> QtClass
makeQtClass' = flip QtClass

-- | Specification for a signal in the Qt signals and slots framework.
data Signal = Signal
  { signalCName :: String
    -- ^ The C name of the signal, without parameters, e.g. @"clicked"@.
  , signalExtName :: ExtName
    -- ^ The signal's external name.
  , signalClass :: QtClass
    -- ^ The class to which the signal belongs.
  , signalListenerClass :: Class
    -- ^ An appropriately typed listener class.
  }

makeSignal :: String  -- ^ 'signalCName'
           -> ExtName  -- ^ 'signalExtName'
           -> QtClass  -- ^ 'signalClass'
           -> Class  -- ^ 'signalListenerClass'
           -> Signal
makeSignal = Signal
