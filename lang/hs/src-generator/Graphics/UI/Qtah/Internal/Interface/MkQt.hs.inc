#include "Mk.hs.inc"

_mkSignal :: String -> Class -> Signal
_mkSignal name listenerClass =
  makeSignal name (toExtName $ fromExtName (classExtName this) ++ "_" ++ name)
  thisQt listenerClass
