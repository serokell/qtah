#include "Mk.hs.inc"

_mkSignal :: String -> Class -> Signal
_mkSignal name =
  makeSignal name (toExtName $ fromExtName (classExtName this) ++ "_" ++ name)
  thisQt
