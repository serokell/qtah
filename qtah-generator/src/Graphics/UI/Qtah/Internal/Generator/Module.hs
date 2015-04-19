module Graphics.UI.Qtah.Internal.Generator.Module (
  generateModule,
  ) where

import Control.Arrow ((&&&))
import Control.Monad (forM_, unless, when)
import Data.List (isPrefixOf)
import Data.Maybe (isJust)
import Foreign.Cppop.Common (fromMaybeM)
import Foreign.Cppop.Generator.Language.Haskell.General (
  Generator,
  HsTypeSide (HsHsSide),
  abort,
  cppTypeToHsType,
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
  ExtName,
  Function,
  HaskellEncoding,
  Method,
  Type (TObj),
  classCtors,
  classHaskellType,
  classEncoding,
  classExtName,
  classMethods,
  ctorExtName,
  fnExtName,
  fromExtName,
  methodExtName,
  toExtName,
  )
import Graphics.UI.Qtah.Internal.Generator.Types
import Graphics.UI.Qtah.Internal.Generator.Signal (
  moduleNameToSignalModuleName,
  toSignalBindingName,
  )
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (UnQual),
  HsQualType (HsQualType),
  HsType (HsTyApp, HsTyCon, HsTyFun, HsTyVar),
  )
import System.Exit (exitFailure)
import System.FilePath ((</>), (<.>), pathSeparator)

generateModule :: FilePath -> String -> String -> QtModule -> IO ()
generateModule srcDir baseModuleName foreignModuleName qtModule = do
  let fullModuleName = moduleNameAppend baseModuleName $ qtModuleSubname qtModule
      qtImports = qtModuleImports qtModule
      qtExports = qtModuleExports qtModule

  let generation = execGenerator $ do
        sayLn "{-# LANGUAGE NoMonomorphismRestriction #-}"
        sayLn "---------- GENERATED FILE, EDITS WILL BE LOST ----------"
        ln
        saysLn ["module ", fullModuleName, " ("]
        indent $ do
          forM_ (concatMap getReexportNames qtExports) $ \name ->
            saysLn [name, ","]
          sayLn ") where"

        -- Import functions and classes from the main module.
        ln
        saysLn ["import ", foreignModuleName, " ("]
        indent $ do
          forM_ (concatMap getImportSpecsFromMainModule qtExports) $ \spec ->
            saysLn [spec, ","]
          sayLn ")"

        -- Import signals from the signal module.
        saysLn ["import ", moduleNameToSignalModuleName foreignModuleName, " ("]
        indent $ do
          forM_ (concatMap getImportSpecsFromSignalModule qtExports) $ \spec ->
            saysLn [spec, ","]
          sayLn ")"
        -- Generated encode and decode functions require some things from Cppop
        -- support and the Prelude; we'll use them qualified.
        if (any (isJust . exportClassHaskellEncoding) qtExports)
          then do sayLn "import qualified Foreign.Cppop.Runtime.Support as FCRS"
                  sayLn "import qualified Prelude as P"
          else sayLn "import Prelude ()"

        -- Import other things required by the module.
        unless (null qtImports) $ do
          ln
          forM_ qtImports $ \x -> saysLn ["import ", x]

        -- Generate bindings with abbreviated names.
        ln
        forM_ (concatMap getBindings qtExports) $ \(name, value) ->
          -- Functions aren't renamed, and declaring "x = x" would be a problem!
          when (name /= value) $
          saysLn [name, " = ", value]

        -- Generate encode/decode functions for classes.
        forM_ qtExports $ \qtExport -> case qtExport of
          QtExportClass qtCls -> sayClassEncodingFnReexports $ qtClassClass qtCls
          QtExportCallback {} -> return ()
          QtExportEnum {} -> return ()
          QtExportFn {} -> return ()

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

exportClassHaskellEncoding :: QtExport -> Maybe HaskellEncoding
exportClassHaskellEncoding export = case export of
  QtExportClass qtCls -> classHaskellType $ classEncoding $ qtClassClass qtCls
  _ -> Nothing

getReexportNames :: QtExport -> [String]
getReexportNames qtExport = case qtExport of
  QtExportEnum enum -> [toHsEnumTypeName enum ++ " (..)"]
  QtExportFn fn -> [getFnReexportName fn]
  QtExportClass qtCls ->
    let cls = qtClassClass qtCls
    in toHsClassName Const cls :
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
              , map (getSignalReexportName cls) $ qtClassSignals qtCls
              ]
  QtExportCallback _ -> []

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

getSignalReexportName :: Class -> Signal -> String
getSignalReexportName cls =
  (++ "Signal") . toBindingNameWithoutClassPrefix cls . signalExtName

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

getImportSpecsFromMainModule :: QtExport -> [String]
getImportSpecsFromMainModule qtExport = case qtExport of
  QtExportEnum enum -> [toHsEnumTypeName enum ++ " (..)"]
  QtExportFn fn -> [getFnImportName fn]
  QtExportClass qtCls ->
    let cls = qtClassClass qtCls
    in (toHsClassName Const cls ++ " (..)") :
       (toHsClassName Nonconst cls ++ " (..)") :
       toHsDataTypeName Const cls :
       toHsDataTypeName Nonconst cls :
       toHsClassNullName cls :
       concat [ map (toHsFnName . ctorExtName) $ classCtors cls
              , map (toHsFnName . methodExtName) $ classMethods cls
              ]
  QtExportCallback _ -> []

getImportSpecsFromSignalModule :: QtExport -> [String]
getImportSpecsFromSignalModule qtExport = case qtExport of
  QtExportEnum _ -> []
  QtExportFn _ -> []
  QtExportClass qtCls -> map toSignalBindingName $ qtClassSignals qtCls
  QtExportCallback _ -> []

getFnImportName :: Function -> String
getFnImportName = toHsFnName . fnExtName

getBindings :: QtExport -> [(String, String)]
getBindings qtExport = case qtExport of
  QtExportEnum _ -> []
  QtExportFn fn -> [(getFnReexportName fn, getFnImportName fn)]
  QtExportClass qtCls ->
    let cls = qtClassClass qtCls
    in (classConstCastReexportName, toHsCastMethodName Const cls) :
       (classCastReexportName, toHsCastMethodName Nonconst cls) :
       (classNullReexportName, toHsClassNullName cls) :
       concat [ map (getCtorReexportName cls &&& toHsFnName . ctorExtName) $
                classCtors cls
              , map (getMethodReexportName cls &&& toHsFnName . methodExtName) $
                classMethods cls
              , map (getSignalReexportName cls &&& toSignalBindingName) $
                qtClassSignals qtCls
              ]
  QtExportCallback _ -> []

sayClassEncodingFnReexports :: Class -> Generator ()
sayClassEncodingFnReexports cls =
  when (isJust $ classHaskellType $ classEncoding cls) $ do
    hsHsType <-
      fromMaybeM (abort $ "generateModule: Expected a Haskell type for class " ++
                  show (fromExtName $ classExtName cls) ++ ".") $
      cppTypeToHsType HsHsSide $ TObj cls
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
