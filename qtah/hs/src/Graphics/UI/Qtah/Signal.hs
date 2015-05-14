module Graphics.UI.Qtah.Signal (
  Signal (..),
  on,
  ) where

newtype Signal object handler = Signal { internalConnectSignal :: object -> handler -> IO Bool }

on :: object -> Signal object handler -> handler -> IO Bool
on = flip internalConnectSignal
