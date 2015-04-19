_mkCtor :: String -> [Type] -> Ctor
_mkCtor name =
  makeCtor (toExtName $ fromExtName (classExtName this) ++ "_" ++ name)

_mkMethod :: String -> [Type] -> Type -> Method
_mkMethod name =
  makeMethod name (toExtName $ fromExtName (classExtName this) ++ "_" ++ name)
  MNormal Nonpure

_mkMethod' :: String -> String -> [Type] -> Type -> Method
_mkMethod' cName hsName =
  makeMethod cName (toExtName $ fromExtName (classExtName this) ++ "_" ++ hsName)
  MNormal Nonpure

_mkConstMethod :: String -> [Type] -> Type -> Method
_mkConstMethod name =
  makeMethod name (toExtName $ fromExtName (classExtName this) ++ "_" ++ name)
  MConst Nonpure

_mkConstMethod' :: String -> String -> [Type] -> Type -> Method
_mkConstMethod' cName hsName =
  makeMethod cName (toExtName $ fromExtName (classExtName this) ++ "_" ++ hsName)
  MConst Nonpure

_mkStaticMethod :: String -> [Type] -> Type -> Method
_mkStaticMethod name =
  makeMethod name (toExtName $ fromExtName (classExtName this) ++ "_" ++ name)
  MStatic Nonpure
