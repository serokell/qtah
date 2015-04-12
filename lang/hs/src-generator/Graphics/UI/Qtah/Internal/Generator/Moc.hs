module Graphics.UI.Qtah.Internal.Generator.Moc (
  QtClass (..),
  makeQtClass,
  Signal (..),
  ) where

import Foreign.Cppop.Generator.Spec (
  Class,
  Ctor,
  ExtName,
  Identifier,
  Method,
  makeClass,
  )

data QtClass = QtClass
  { qtClassClass :: Class
  , qtClassSignals :: [Signal]
  }

makeQtClass :: Identifier
            -> Maybe ExtName
            -> [Class]
            -> [Ctor]
            -> [Method]
            -> [Signal]
            -> QtClass
makeQtClass identifier maybeExtName supers ctors methods signals = QtClass
  { qtClassClass = makeClass identifier maybeExtName supers ctors methods
  , qtClassSignals = signals
  }

data Signal = Signal
  { signalCName :: String
  , signalExtName :: ExtName
  , signalClass :: QtClass
  , signalListenerClass :: Class
  }
