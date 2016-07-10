-- This file is part of Qtah.
--
-- Copyright 2015-2016 Bryan Gardiner <bog@khumba.net>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- | Haskell definitions for preprocessor flags that Qt uses for conditional
-- compilation.
--
-- A list of flags enabled on your system can be obtained with:
--
-- > gcc -dM -E $(pkg-config --cflags QtCore) /usr/include/qt4/Qt/qconfig.h | grep '#define QT'
--
-- Using @qglobal.h@ and @#define Q@ provides additional defintions,
-- e.g. version and windowing system information.
module Graphics.UI.Qtah.Generator.Flags (
  Version,
  qtVersion,
  keypadNavigation,
  qdoc,
  qrealFloat,
  wsWince,
  ) where

import Control.Monad (unless)
import Data.Char (isDigit, isSpace)
import Data.List (isPrefixOf)
import Graphics.UI.Qtah.Generator.Common (fromMaybeM, splitOn)
import System.Directory (findExecutable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)

-- | A type synonym for Qt version specifications.  These are just lists of
-- integers, of length two.  Examples are @[4, 8]@ and @[5, 0]@ to denote
-- versions 4.8 and 5.0 respectively.  A third component may be used in the
-- future, if necessary.
type Version = [Int]

-- | This is initialized at program startup with the version of Qt that the
-- generator will work with.  This controls which functions and types are made
-- available in the API.
--
-- This is determined the following method:
--
-- * If @QTAH_QT=x.y@ is in the environment, then this value will be used.
--
-- * Otherwise, if @QTAH_QT=x@ is in the environment, then we query @qmake
-- -qt=$QTAH_QT -version@ for the version of Qt to use.
--
-- * Otherwise, we query @qmake -version@ for the version of Qt to use.  Setting
-- @QT_SELECT@ in the environment can select a major version of Qt to use.
--
-- For more information on @qmake -qt@ and @QT_SELECT@, see @man qtchooser@.
qtVersion :: Version
{-# NOINLINE qtVersion #-}
qtVersion = unsafePerformIO readQtVersion

keypadNavigation :: Bool
keypadNavigation = False

qdoc :: Bool
qdoc = False

-- | Whether Qt was configured with qreal=float instead of double.
qrealFloat :: Bool
{-# NOINLINE qrealFloat #-}
qrealFloat = unsafePerformIO $ readBool "QTAH_QREAL_FLOAT" False

wsWince :: Bool
wsWince = False

-- | Reads a Qt version from the environment variable @QTAH_QT@.  See
-- 'qtVersion'.
readQtVersion :: IO Version
readQtVersion = do
  maybeStr <- fmap (\x -> case x of
                      Just "" -> Nothing
                      _ -> x) $
              lookupEnv "QTAH_QT"
  case maybeStr of
    Just str -> do
      let strs = splitOn '.' str
      unless (length strs `elem` [1, 2] && all (\n -> not (null n) && all isDigit n) strs) $
        fail $ concat
        ["qtah-generator requires QTAH_QT=x or QTAH_QT=x.y, can't parse value ", show str, "."]
      let version = map (read :: String -> Int) strs
      case version of
        [_, _] -> return version
        [x] -> queryQmakeQtVersion ["-qt=" ++ show x]
        _ -> fail $ concat
             ["qtah-generator: Internal error, incorrect parsing of QTAH_QT value ", show str, "."]
    Nothing -> queryQmakeQtVersion []

  where queryQmakeQtVersion :: [String] -> IO Version
        queryQmakeQtVersion extraArgs = do
          qmakePath <- findQMake
          let args = extraArgs ++ ["-version"]
          (exitCode, out, err) <- readProcessWithExitCode qmakePath args ""
          let qmakeDebugWords =
                ["  Ran ", show (qmakePath : args), ".\nStdout:\n", out, "\nStderr:\n", err]
          unless (exitCode == ExitSuccess) $
            fail $ concat $ "qtah-generator: qmake returned non-zero exit code." : qmakeDebugWords

          let versionLinePrefix = "Using Qt version "

              maybeVersionStrs = do
                versionLine <- expectSingle $
                               filter (versionLinePrefix `isPrefixOf`) $
                               lines out
                let str = takeWhile (not . isSpace) $ drop (length versionLinePrefix) versionLine
                    strs = take 2 $ splitOn '.' str
                if length strs == 2 && all (\n -> not (null n) && all isDigit n) strs
                  then Just strs
                  else Nothing

          case maybeVersionStrs of
            Just strs -> return $ map (read :: String -> Int) strs
            Nothing ->
              fail $ concat $
              "qtah-generator: Can't parse Qt version from qmake output." : qmakeDebugWords

        expectSingle :: [a] -> Maybe a
        expectSingle [x] = Just x
        expectSingle _ = Nothing

-- | Reads a boolean value from the program's environment.  If the variable is
-- set and non-empty, then if must be one of the strings @true@ or @false@.  An
-- empty or unset value is treated as the provided default value.
readBool :: String -> Bool -> IO Bool
readBool name defaultValue = do
  maybeStr <- lookupEnv name
  case maybeStr of
    Nothing -> return defaultValue
    Just str -> case str of
      "" -> return defaultValue
      "true" -> return True
      "false" -> return False
      s -> fail $ concat
           ["qtah-generator: Expected a boolean value for ", name,
            " (true/false).  Got ", show s, "."]

findQMake :: IO FilePath
findQMake = lookupEnv "QTAH_QMAKE" >>= fromMaybeM findQmake
  where findQmake =
          findExecutable "qmake" >>=
          fromMaybeM
          (fail $ "qtah-generator: Can't find qmake.  Please ensure qmake " ++
           "is installed and set QTAH_QMAKE to qmake's path.")
