module Main where

import Foreign.Cppop.Generator.Main
import Foreign.Cppop.Generator.Spec
import Foreign.Cppop.Generator.Std
import System.Environment (getArgs)
import System.Exit (exitFailure)

interfaceResult :: Either String Interface
interfaceResult =
  interface "qtpi"
  [ includeStd "math.h"  -- TODO cmath?
  , includeStd "cstring"
  , includeStd "QAbstractButton"
  , includeStd "QApplication"
  , includeStd "QMainWindow"
  , includeStd "QObject"
  , includeStd "QPushButton"
  , includeStd "QString"
  , includeStd "QWidget"
  , includeLocal "shim_qapplication.hpp"
  ]
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
  ]

-- TODO Disabled because encoding CFloat, CDouble is hard.
--
--f_sin = Function (ident "sin") (toExtName "sin") Pure [TDouble] TDouble
--
--f_sinf = Function (ident "sinf") (toExtName "sinf") Pure [TFloat] TFloat

c_QAbstractButton =
  makeClass (ident "QAbstractButton") Nothing
  [ c_QWidget ]
  []
  [ Method "setText" (toExtName "QAbstractButton_setText") MNormal Nonpure
    [TObj c_QString] TVoid
  , Method "text" (toExtName "QAbstractButton_text") MConst Nonpure
    [] $ TObj c_QString
  ]

f_QApplication_new =
  Function (ident1 "qtpi" "shim_QApplication_new") (toExtName "QApplication_new") Nonpure
  [] $ TPtr $ TObj c_QApplication

c_QApplication =
  makeClass (ident "QApplication") Nothing
  [ c_QCoreApplication ]
  [ {-Ctor (toExtName "QApplication_new") []-} ]
  [ Method "exec" (toExtName "QApplication_exec") MNormal Nonpure [] TVoid ]

c_QCoreApplication =
  makeClass (ident "QCoreApplication") Nothing [c_QObject] [] []

c_QMainWindow =
  makeClass (ident "QMainWindow") Nothing
  [ c_QWidget ]
  [ Ctor (toExtName "QMainWindow_new") [TPtr $ TObj c_QWidget] ]
  [ Method "setCentralWidget" (toExtName "QMainWindow_setCentralWidget") MNormal Nonpure
    [TPtr $ TObj c_QWidget] TVoid
  ]

c_QObject = makeClass (ident "QObject") Nothing [] [] []

c_QPushButton =
  makeClass (ident "QPushButton") Nothing
  [ c_QAbstractButton ]
  [ Ctor (toExtName "QPushButton_new") [TPtr $ TObj c_QWidget]
  , Ctor (toExtName "QPushButton_newWithText") [TObj c_QString, TPtr $ TObj c_QWidget]
  ]
  []

c_QString =
  classModifyEncoding
  (\c -> c { classCppCType = Just $ TPtr TChar
           , classCppDecoder = Just $ CppCoderFn $ ident "QString"
           , classCppEncoder = Just $ CppCoderExpr [Just "strdup(", Nothing, Just ".toStdString().c_str())"]
           }) $
  classCopyEncodingFrom cls_std__string $
  makeClass (ident "QString") Nothing [] [] []

c_QWidget =
  makeClass (ident "QWidget") Nothing
  [ c_QObject ]
  [ Ctor (toExtName "QWidget_new") [TPtr $ TObj c_QWidget] ]
  [ Method "resize" (toExtName "QWidget_resize") MNormal Nonpure
    [TInt, TInt] TVoid
  , Method "setWindowTitle" (toExtName "QWidget_setWindowTitle") MNormal Nonpure
    [TObj c_QString] TVoid
  , Method "show" (toExtName "QWidget_show") MNormal Nonpure
    [] TVoid
  ]

main :: IO ()
main = do
  case interfaceResult of
    Left errorMsg -> do
      putStrLn $ "Error initializing interface: " ++ errorMsg
      exitFailure
    Right iface -> do
      args <- getArgs
      run [iface] args
