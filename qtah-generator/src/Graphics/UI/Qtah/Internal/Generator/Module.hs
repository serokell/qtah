module Graphics.UI.Qtah.Internal.Generator.Module (
  generateModule,
  ) where

import Control.Monad (forM_, when)
import Data.List (find, intersperse, isPrefixOf)
import Data.Maybe (isJust)
import Foreign.Cppop.Common (fromMaybeM)
import Foreign.Cppop.Generator.Language.Cpp.General (execChunkWriter, sayType)
import Foreign.Cppop.Generator.Language.Haskell.General (
  Generator,
  HsTypeSide (HsHsSide),
  addExport,
  addExports,
  addImport,
  addImportsForClass,
  addQualifiedImports,
  abort,
  cppTypeToHsTypeAndUse,
  execGenerator,
  indent,
  ln,
  prettyPrint,
  sayLn,
  saysLn,
  toHsCastMethodName,
  toHsClassName,
  toHsClassNullName,
  toHsDataTypeName,
  toHsEnumTypeName,
  toHsFnName,
  )
import Foreign.Cppop.Generator.Spec (
  Class,
  Constness (Const, Nonconst),
  Ctor,
  Export (ExportCallback, ExportClass, ExportEnum, ExportFn),
  ExtName,
  Function,
  Interface,
  Method,
  Type (TCallback, TObj),
  callbackParams,
  classCtors,
  classHaskellType,
  classEncoding,
  classExtName,
  classMethods,
  ctorExtName,
  ctorParams,
  fnExtName,
  fromExtName,
  methodCName,
  methodExtName,
  toExtName,
  )
import Graphics.UI.Qtah.Internal.Generator.Types (
  QtExport (QtExport, QtExportSignal),
  QtModule,
  Signal,
  moduleNameAppend,
  qtModuleQtExports,
  qtModuleSubname,
  signalClass,
  signalCName,
  signalListenerClass,
  )
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsQualType (HsQualType),
  HsType (HsTyApp, HsTyCon, HsTyFun, HsTyVar),
  )
import System.Exit (exitFailure)
import System.FilePath ((</>), (<.>), pathSeparator)

generateModule :: Interface -> FilePath -> String -> String -> QtModule -> IO ()
generateModule iface srcDir baseModuleName foreignModuleName qtModule = do
  let fullModuleName = moduleNameAppend baseModuleName $ qtModuleSubname qtModule
      qtExports = qtModuleQtExports qtModule

  let generation =
        fmap ("{-# LANGUAGE NoMonomorphismRestriction #-}\n\n" ++) $
        execGenerator iface fullModuleName $ do
          -- As in generated Cppop bindings, avoid non-qualified Prelude uses in
          -- generated code here.
          addImport "Prelude ()"

          -- Generate bindings for all of the exports.
          forM_ qtExports $ sayQtExport foreignModuleName

  case generation of
    Left errorMsg -> do
      putStrLn $ "Error generating Qt modules: " ++ errorMsg
      exitFailure
    Right source -> do
      let path =
            srcDir </>
            map (\c -> if c == '.' then pathSeparator else c) fullModuleName <.>
            "hs"
      writeFile path source

getFnReexportName :: Function -> String
getFnReexportName = getFnImportName

classCastReexportName :: String
classCastReexportName = "cast"

classConstCastReexportName :: String
classConstCastReexportName = "castConst"

classNullReexportName :: String
classNullReexportName = "null"

classEncodeReexportName :: String
classEncodeReexportName = "encode"

classDecodeReexportName :: String
classDecodeReexportName = "decode"

getCtorReexportName :: Class -> Ctor -> String
getCtorReexportName cls = toBindingNameWithoutClassPrefix cls . ctorExtName

getMethodReexportName :: Class -> Method -> String
getMethodReexportName cls = toBindingNameWithoutClassPrefix cls . methodExtName

-- | Qtah uses @ClassName_innerName@ 'ExtName's for things in classes.  This
-- function strips the @ClassName_@ prefix off of an 'ExtName', if present, and
-- converts it to a function name.
toBindingNameWithoutClassPrefix :: Class -> ExtName -> String
toBindingNameWithoutClassPrefix cls name =
  toHsFnName $ toExtName $
  dropPrefix (fromExtName (classExtName cls) ++ "_") $
  fromExtName name

-- | @dropPrefix prefix str@ strips @prefix@ from @str@ if @str@ starts with
-- @prefix@, and returns @str@ unmodified otherwise.
dropPrefix :: String -> String -> String
dropPrefix prefix str =
  if prefix `isPrefixOf` str
  then drop (length prefix) str
  else str

getFnImportName :: Function -> String
getFnImportName = toHsFnName . fnExtName

sayClassEncodingFnReexports :: Class -> Generator ()
sayClassEncodingFnReexports cls =
  when (isJust $ classHaskellType $ classEncoding cls) $ do
    -- Generated encode and decode functions require some things from Cppop
    -- support and the Prelude.
    addImport "qualified Foreign.Cppop.Runtime.Support as FCRS"
    addImport "qualified Prelude as P"

    hsHsType <-
      fromMaybeM (abort $ "generateModule: Expected a Haskell type for class " ++
                  show (fromExtName $ classExtName cls) ++ ".") =<<
      cppTypeToHsTypeAndUse HsHsSide (TObj cls)
    let constClassName = toHsClassName Const cls
        dataTypeName = toHsDataTypeName Nonconst cls
        ptrHsType = HsTyCon $ UnQual $ HsIdent dataTypeName
        thisTyVar = HsTyVar $ HsIdent "this"
        encodeFnType = HsTyFun hsHsType $ HsTyApp (HsTyCon $ UnQual $ HsIdent "P.IO") ptrHsType
        decodeFnType = HsQualType [(UnQual $ HsIdent constClassName, [thisTyVar])] $
                       HsTyFun thisTyVar $
                       HsTyApp (HsTyCon $ UnQual $ HsIdent "P.IO") hsHsType
    ln
    saysLn [classEncodeReexportName, " :: ", prettyPrint encodeFnType]
    saysLn [classEncodeReexportName, " = FCRS.encodeAs (P.undefined :: ", dataTypeName, ")"]
    ln
    saysLn [classDecodeReexportName, " :: ", prettyPrint decodeFnType]
    saysLn [classDecodeReexportName, " = FCRS.decode P.. ", toHsCastMethodName Const cls]

sayQtExport :: String -> QtExport -> Generator ()
sayQtExport foreignModuleName qtExport = case qtExport of
  QtExport (ExportEnum e) -> do
    let spec = toHsEnumTypeName e ++ " (..)"
    addImport $ wrap spec
    addExport spec

  QtExport (ExportFn fn) -> do
    addImport $ wrap $ getFnImportName fn
    addExport $ getFnReexportName fn

  QtExport (ExportCallback _) -> return ()

  QtExport (ExportClass cls) -> do
    addImport $ wraps $
      (toHsClassName Const cls ++ " (..)") :
      (toHsClassName Nonconst cls ++ " (..)") :
      toHsDataTypeName Const cls :
      toHsDataTypeName Nonconst cls :
      toHsClassNullName cls :
      concat [ map (toHsFnName . ctorExtName) $ classCtors cls
             , map (toHsFnName . methodExtName) $ classMethods cls
             ]

    addExports $
      toHsClassName Const cls :
      toHsClassName Nonconst cls :
      toHsDataTypeName Const cls :
      toHsDataTypeName Nonconst cls :
      classConstCastReexportName :
      classCastReexportName :
      classNullReexportName :
      concat [ case classHaskellType $ classEncoding cls of
                 Nothing -> []
                 Just _ -> [classEncodeReexportName, classDecodeReexportName]
             , map (getCtorReexportName cls) $ classCtors cls
             , map (getMethodReexportName cls) $ classMethods cls
             ]

    ln
    sayBind classConstCastReexportName $ toHsCastMethodName Const cls
    sayBind classCastReexportName $ toHsCastMethodName Nonconst cls
    sayBind classNullReexportName $ toHsClassNullName cls
    sayClassEncodingFnReexports cls
    forM_ (classCtors cls) $ \ctor ->
      sayBind (getCtorReexportName cls ctor) $ toHsFnName $ ctorExtName ctor
    forM_ (classMethods cls) $ \method ->
      sayBind (getMethodReexportName cls method) $ toHsFnName $ methodExtName method

  QtExportSignal sig -> saySignalExport sig

  where wrap spec = concat [foreignModuleName, " (", spec, ")"]
        wraps specs = concat [foreignModuleName, " (\n",
                              concatMap (\spec -> concat ["  ", spec, ",\n"]) specs,
                              "  )"]
        sayBind name value = saysLn [name, " = ", value]

-- | Generates and exports a @Signal@ definition.  We create the signal from
-- scratch in this module, rather than reexporting it from somewhere else.
saySignalExport :: Signal -> Generator ()
saySignalExport signal = do
  addQualifiedImports
  addImport "qualified Graphics.UI.Qtah.Signal as QtahSignal"

  let name = signalCName signal
      className = toHsClassName Nonconst $ signalClass signal
      varName = toSignalBindingName signal
  addExport varName

  let listenerClass = signalListenerClass signal
  addImportsForClass listenerClass
  -- Find the listener constructor that only takes a callback.
  listenerCtor <-
    fromMaybeM (abort $ "saySignalExport: Couldn't find an appropriate " ++
                show (fromExtName $ classExtName listenerClass) ++
                " constructor for signal " ++ show name ++ ".") $
    flip find (classCtors listenerClass) $ \ctor -> case ctorParams ctor of
      [TCallback {}] -> True
      _ -> False
  let [callbackType@(TCallback callback)] = ctorParams listenerCtor
      paramTypes = callbackParams callback

  -- Also find the 'connectListener' method.
  listenerConnectMethod <-
    fromMaybeM (abort $ "generateSignals: Couldn't find the connectListener method in class " ++
                show (fromExtName $ classExtName listenerClass) ++
                " for signal " ++ show name ++ ".") $
    find (("connectListener" ==) . methodCName) $ classMethods listenerClass

  callbackHsType <-
    fromMaybeM (abort $
                "generateSignals: Can't generate Haskell callback type for signal " ++
                show name ++ ".") =<<
    cppTypeToHsTypeAndUse HsHsSide callbackType

  let varType = HsQualType [(UnQual $ HsIdent className, [HsTyVar $ HsIdent "object"])] $
                HsTyApp (HsTyApp (HsTyCon $ UnQual $ HsIdent "QtahSignal.Signal") $
                         HsTyVar $ HsIdent "object")
                callbackHsType
  ln
  saysLn [varName, " :: ", prettyPrint varType]
  saysLn [varName, " = QtahSignal.Signal"]
  indent $ do
    sayLn "{ QtahSignal.internalConnectSignal = \\object' fn' -> do"
    indent $ do
      saysLn ["listener' <- ", toHsFnName $ ctorExtName listenerCtor, " fn'"]
      saysLn [toHsFnName $ methodExtName listenerConnectMethod, " listener' object' ",
              show (toSignalConnectName signal paramTypes)]
    sayLn "}"

toSignalBindingName :: Signal -> String
toSignalBindingName = (++ "Signal") . signalCName

toSignalConnectName :: Signal -> [Type] -> String
toSignalConnectName signal paramTypes =
  concat $
  "2" :  -- This is a magic code added by the SIGNAL() macro.
  signalCName signal :
  "(" :
  intersperse "," (map (execChunkWriter . sayType Nothing) paramTypes) ++
  [")"]
