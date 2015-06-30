module Graphics.UI.Qtah.Internal.Generator.Module (
  generateModule,
  ) where

import Control.Monad (forM_, when)
import Data.List (find, intersperse, isPrefixOf, sort)
import Data.Monoid (mconcat)
import Data.Maybe (isJust)
import Foreign.Cppop.Common (fromMaybeM, writeFileIfDifferent)
import Foreign.Cppop.Generator.Language.Cpp.General (execChunkWriter, sayType)
import Foreign.Cppop.Generator.Language.Haskell.General (
  Generator,
  HsTypeSide (HsHsSide),
  addExport,
  addExports,
  addImports,
  abort,
  cppTypeToHsTypeAndUse,
  execGenerator,
  importHsModuleForExtName,
  indent,
  ln,
  prettyPrint,
  renderPartial,
  sayLn,
  saysLn,
  toHsCastMethodName,
  toHsClassNullName,
  toHsDataTypeName,
  toHsEnumTypeName,
  toHsFnName,
  toHsPtrClassName,
  toHsValueClassName,
  )
import Foreign.Cppop.Generator.Spec (
  Class,
  Constness (Const, Nonconst),
  Ctor,
  Export (ExportCallback, ExportClass, ExportEnum, ExportFn),
  ExtName,
  FnName (FnName),
  Function,
  Interface,
  Method,
  Type (TCallback, TObj),
  callbackParams,
  classConversions,
  classCtors,
  classExtName,
  classHaskellConversion,
  classMethods,
  ctorExtName,
  ctorParams,
  enumExtName,
  fnExtName,
  fromExtName,
  hsImports,
  methodCName,
  methodExtName,
  toExtName,
  )
import Graphics.UI.Qtah.Internal.Generator.Types (
  QtExport (QtExport, QtExportFnRenamed, QtExportSignal),
  QtModule,
  Signal,
  moduleNameAppend,
  qtModuleQtExports,
  qtModuleSubname,
  signalClass,
  signalCName,
  signalListenerClass,
  )
import Graphics.UI.Qtah.Internal.Interface.Imports
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsQualType (HsQualType),
  HsType (HsTyApp, HsTyCon, HsTyFun, HsTyVar),
  )
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath ((</>), (<.>), pathSeparator, takeDirectory)

generateModule :: Interface -> FilePath -> String -> QtModule -> IO ()
generateModule iface srcDir baseModuleName qtModule = do
  let fullModuleName = moduleNameAppend baseModuleName $ qtModuleSubname qtModule
      qtExports = qtModuleQtExports qtModule

  let generation =
        fmap (("{-# LANGUAGE NoMonomorphismRestriction #-}\n\n" ++) . renderPartial) $
        execGenerator iface fullModuleName $ do
          -- As in generated Cppop bindings, avoid non-qualified Prelude uses in
          -- generated code here.
          addImports $ hsImports "Prelude" []

          -- Generate bindings for all of the exports.
          mapM_ sayQtExport qtExports

  case generation of
    Left errorMsg -> do
      putStrLn $ "Error generating Qt modules: " ++ errorMsg
      exitFailure
    Right source -> do
      let path =
            srcDir </>
            map (\c -> if c == '.' then pathSeparator else c) fullModuleName <.>
            "hs"
      createDirectoryIfMissing True $ takeDirectory path
      writeFileIfDifferent path source

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
  when (isJust $ classHaskellConversion $ classConversions cls) $ do
    -- Generated encode and decode functions require some things from Cppop
    -- support and the Prelude.
    addImports $ mconcat [importForPrelude, importForSupport]

    hsHsType <-
      fromMaybeM (abort $ "generateModule: Expected a Haskell type for class " ++
                  show (fromExtName $ classExtName cls) ++ ".") =<<
      cppTypeToHsTypeAndUse HsHsSide (TObj cls)
    let constPtrClassName = toHsPtrClassName Const cls
        dataTypeName = toHsDataTypeName Nonconst cls
        ptrHsType = HsTyCon $ UnQual $ HsIdent dataTypeName
        thisTyVar = HsTyVar $ HsIdent "this"
        encodeFnType = HsTyFun hsHsType $ HsTyApp (HsTyCon $ UnQual $ HsIdent "QtahP.IO") ptrHsType
        decodeFnType = HsQualType [(UnQual $ HsIdent constPtrClassName, [thisTyVar])] $
                       HsTyFun thisTyVar $
                       HsTyApp (HsTyCon $ UnQual $ HsIdent "QtahP.IO") hsHsType
    ln
    saysLn [classEncodeReexportName, " :: ", prettyPrint encodeFnType]
    saysLn [classEncodeReexportName, " = QtahFCRS.encodeAs (QtahP.undefined :: ", dataTypeName, ")"]
    ln
    saysLn [classDecodeReexportName, " :: ", prettyPrint decodeFnType]
    saysLn [classDecodeReexportName, " = QtahFCRS.decode QtahP.. ", toHsCastMethodName Const cls]

sayQtExport :: QtExport -> Generator ()
sayQtExport qtExport = case qtExport of
  QtExport (ExportEnum e) -> do
    importHsModuleForExtName $ enumExtName e
    let spec = toHsEnumTypeName e ++ " (..)"
    addExport spec

  QtExport (ExportFn fn) -> do
    importHsModuleForExtName $ fnExtName fn
    addExport $ getFnReexportName fn

  QtExportFnRenamed fn rename -> do
    importHsModuleForExtName $ fnExtName fn
    addExport rename
    sayBind rename $ getFnImportName fn

  QtExport (ExportCallback _) -> return ()

  QtExport (ExportClass cls) -> do
    importHsModuleForExtName $ classExtName cls
    addExports $
      (toHsValueClassName cls ++ " (..)") :
      toHsPtrClassName Const cls :
      toHsPtrClassName Nonconst cls :
      toHsDataTypeName Const cls :
      toHsDataTypeName Nonconst cls :
      classConstCastReexportName :
      classCastReexportName :
      classNullReexportName :
      concat [ case classHaskellConversion $ classConversions cls of
                 Nothing -> []
                 Just _ -> [classEncodeReexportName, classDecodeReexportName]
             , sort $ map (getCtorReexportName cls) $ classCtors cls
             , sort $ map (getMethodReexportName cls) $ classMethods cls
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

  where sayBind name value = saysLn [name, " = ", value]

-- | Generates and exports a @Signal@ definition.  We create the signal from
-- scratch in this module, rather than reexporting it from somewhere else.
saySignalExport :: Signal -> Generator ()
saySignalExport signal = do
  addImports importForSignal

  let name = signalCName signal
      ptrClassName = toHsPtrClassName Nonconst $ signalClass signal
      varName = toSignalBindingName signal
  addExport varName

  let listenerClass = signalListenerClass signal
  importHsModuleForExtName $ classExtName listenerClass
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
    find ((FnName "connectListener" ==) . methodCName) $ classMethods listenerClass

  callbackHsType <-
    fromMaybeM (abort $
                "generateSignals: Can't generate Haskell callback type for signal " ++
                show name ++ ".") =<<
    cppTypeToHsTypeAndUse HsHsSide callbackType

  let varType = HsQualType [(UnQual $ HsIdent ptrClassName, [HsTyVar $ HsIdent "object"])] $
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
