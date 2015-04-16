_mkCtor :: String -> [Type] -> Ctor
_mkCtor name paramTypes =
  makeCtor (toExtName $ fromExtName (classExtName this) ++ "_" ++ name) paramTypes

_mkMethod :: String -> [Type] -> Type -> Method
_mkMethod name paramTypes retType =
  makeMethod name (toExtName $ fromExtName (classExtName this) ++ "_" ++ name)
  MNormal Nonpure paramTypes retType

_mkMethod' :: String -> String -> [Type] -> Type -> Method
_mkMethod' cName hsName paramTypes retType =
  makeMethod cName (toExtName $ fromExtName (classExtName this) ++ "_" ++ hsName)
  MNormal Nonpure paramTypes retType

_mkConstMethod :: String -> [Type] -> Type -> Method
_mkConstMethod name paramTypes retType =
  makeMethod name (toExtName $ fromExtName (classExtName this) ++ "_" ++ name)
  MConst Nonpure paramTypes retType

_mkStaticMethod :: String -> [Type] -> Type -> Method
_mkStaticMethod name paramTypes retType =
  makeMethod name (toExtName $ fromExtName (classExtName this) ++ "_" ++ name)
  MStatic Nonpure paramTypes retType
