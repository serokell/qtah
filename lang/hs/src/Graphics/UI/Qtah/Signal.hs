module Graphics.UI.Qtah.Signal (
  Signal,
  on,
  qAbstractButton_clicked_signal,
  ) where

import Foreign.Cppop.Generated.Qtah (
  QAbstractButtonClass,
  listenerBool_connectListener,
  listenerBool_new,
  )

newtype Signal object handler = Signal { connectSignal :: object -> handler -> IO Bool }

on :: object -> Signal object handler -> handler -> IO Bool
on = flip connectSignal

qAbstractButton_clicked_signal :: QAbstractButtonClass object => Signal object (Bool -> IO ())
qAbstractButton_clicked_signal = Signal
  { connectSignal = \object fn -> do
    listener <- listenerBool_new fn
    listenerBool_connectListener listener object "2clicked(bool)"
  }
