module Graphics.UI.Qtah.Signal (
  Signal (..),
  on,
  ) where

newtype Signal object handler = Signal { connectSignal :: object -> handler -> IO Bool }

on :: object -> Signal object handler -> handler -> IO Bool
on = flip connectSignal
