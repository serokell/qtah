module Graphics.UI.Qtah.Internal.Generator.Moc (
  QtClass (..),
  makeQtClass,
  Signal, makeSignal, signalCName, signalExtName, signalClass, signalListenerClass,
  ) where

import Foreign.Cppop.Generator.Spec (
  Class,
  Ctor,
  ExtName,
  Identifier,
  Method,
  makeClass,
  )

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
