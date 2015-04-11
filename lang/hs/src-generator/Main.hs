module Main where

import Foreign.Cppop.Generator.Main
import Foreign.Cppop.Generator.Spec
import Foreign.Cppop.Generator.Std
import Graphics.UI.Qtah.Internal.Interface.Callbacks
import Graphics.UI.Qtah.Internal.Interface.Listeners
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
  ]

interfaceResult :: Either String Interface
interfaceResult =
  interface "qtah"
  "bindings.cpp" "bindings.hpp" bindingImports
  (Just ("callbacks.cpp", "callbacks.hpp", callbackImports))
  (allListeners ++ exports)
  where exports =
          [ ExportClass cls_std__string
          --, ExportFn f_sin
          --, ExportFn f_sinf
          , ExportClass c_QAbstractButton
          , ExportFn f_QApplication_new
          , ExportClass c_QApplication
          , ExportClass c_QCoreApplication
          , ExportClass c_QMainWindow
          , ExportClass c_QObject
          , ExportClass c_QPushButton
          , ExportClass c_QString
          , ExportClass c_QWidget
          , ExportCallback cb_BoolVoid
          , ExportCallback cb_IntIntVoid
          , ExportCallback cb_IntVoid
          , ExportCallback cb_StringVoid
          , ExportFn f_testIntCallback
          , ExportFn f_testStringCallback
          ]

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
      run [iface] args
