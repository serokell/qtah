module Main where

import Data.Foldable (forM_)
import Data.Maybe (mapMaybe)
import Foreign.Cppop.Generator.Language.Haskell.General (execGenerator)
import Foreign.Cppop.Generator.Main (Action (GenHaskell), run)
import Foreign.Cppop.Generator.Spec (
  Callback,
  Class,
  Function,
  Include,
  Interface,
  Export (ExportFn, ExportClass, ExportCallback),
  includeLocal,
  includeStd,
  interface,
  )
import Foreign.Cppop.Generator.Std (c_std__string)
import Graphics.UI.Qtah.Internal.Generator.Moc
import Graphics.UI.Qtah.Internal.Generator.Signal
import Graphics.UI.Qtah.Internal.Interface.Callback
import Graphics.UI.Qtah.Internal.Interface.Listener
import Graphics.UI.Qtah.Internal.Interface.QAbstractButton
import Graphics.UI.Qtah.Internal.Interface.QApplication
import Graphics.UI.Qtah.Internal.Interface.QCoreApplication
import Graphics.UI.Qtah.Internal.Interface.QMainWindow
import Graphics.UI.Qtah.Internal.Interface.QObject
import Graphics.UI.Qtah.Internal.Interface.QPushButton
import Graphics.UI.Qtah.Internal.Interface.QString
import Graphics.UI.Qtah.Internal.Interface.QWidget
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (replaceBaseName, takeBaseName)

-- | A data type that wraps a Cppop 'Export' and adds support for 'QtClass'es.
data QtahExport =
  QtahFn Function
  | QtahClass Class
  | QtahQtClass QtClass
  | QtahCallback Callback

toExport :: QtahExport -> Export
toExport export = case export of
  QtahFn fn -> ExportFn fn
  QtahClass cls -> ExportClass cls
  QtahQtClass qtCls -> ExportClass $ qtClassClass qtCls
  QtahCallback cb -> ExportCallback cb

bindingImports :: [Include]
bindingImports =
  [ includeStd "math.h"  -- TODO cmath?
  , includeStd "cstring"
  , includeStd "QAbstractButton"
  , includeStd "QApplication"
  , includeStd "QMainWindow"
  , includeStd "QObject"
  , includeStd "QPushButton"
  , includeStd "QString"
  , includeStd "QWidget"
  , includeLocal "cbtest.hpp"
  , includeLocal "listeners.hpp"
  , includeLocal "shim_qapplication.hpp"
  ]

callbackImports :: [Include]
callbackImports =
  [ includeStd "cstring"
  , includeStd "string"
  , includeStd "QObject"
  ]

allExports :: [QtahExport]
allExports =
  [ QtahClass c_std__string
  --, ExportFn f_sin
  --, ExportFn f_sinf
  , QtahQtClass qtc_QAbstractButton
  , QtahFn f_QApplication_new
  , QtahClass c_QApplication
  , QtahClass c_QCoreApplication
  , QtahClass c_QMainWindow
  , QtahQtClass qtc_QObject
  , QtahClass c_QPushButton
  , QtahClass c_QString
  , QtahClass c_QWidget
  , QtahFn f_testIntCallback
  , QtahFn f_testStringCallback
  ]

interfaceResult :: Either String Interface
interfaceResult =
  interface "qtah"
  "bindings.cpp" "bindings.hpp" bindingImports
  (Just ("callbacks.cpp", "callbacks.hpp", callbackImports))
  (concat [ map ExportClass allListeners
          , map ExportCallback allCallbacks
          , map toExport allExports
          ])

-- TODO Disabled because encoding CFloat, CDouble is hard.
--
--f_sin = Function (ident "sin") (toExtName "sin") Pure [TDouble] TDouble
--
--f_sinf = Function (ident "sinf") (toExtName "sinf") Pure [TFloat] TFloat

main :: IO ()
main = do
  case interfaceResult of
    Left errorMsg -> do
      putStrLn $ "Error initializing interface: " ++ errorMsg
      exitFailure
    Right iface -> do
      args <- getArgs
      actions <- run [iface] args
      forM_ actions $ \action -> case action of
        GenHaskell path -> do
          let baseModuleName = takeBaseName path
              signalModuleName = moduleNameToSignalModuleName baseModuleName
              signalPath = replaceBaseName path signalModuleName
              generation =
                execGenerator $ generateSignals baseModuleName $
                mapMaybe (\export -> case export of
                             QtahFn {} -> Nothing
                             QtahClass {} -> Nothing
                             QtahQtClass qtCls -> Just qtCls
                             QtahCallback {} -> Nothing)
                allExports
          case generation of
            Left errorMsg -> do
              putStrLn $ "Error generating Qt signal bindings: " ++ show errorMsg
              exitFailure
            Right body -> writeFile signalPath body

        _ -> return ()
