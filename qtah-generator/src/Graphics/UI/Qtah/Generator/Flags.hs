-- This file is part of Qtah.
--
-- Copyright 2015-2018 The Qtah Authors.
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
  qmakeExecutable,
  qmakeArguments,
  keypadNavigation,
  qdoc,
  qrealFloat,
  wsWince,
  ) where

import Control.Monad (unless)
import Data.Char (isDigit, isSpace)
import Data.List (intercalate, isPrefixOf)
import Graphics.UI.Qtah.Generator.Common (firstM, fromMaybeM, splitOn)
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

showVersion :: Version -> String
showVersion = intercalate "." . map show

-- | An internal record of Qt configuration info.
data QtConfig = QtConfig
  { configVersion :: Version
  , configQmakeExecutable :: FilePath
  , configQmakeArguments :: [String]
  }

-- | This is initialized at program startup with the version of Qt that the
-- generator will work with, along with the corresponding QMake binary and
-- arguments necessary to invoke it.  The Qt version which functions and types
-- are made available in the API.
--
-- The Qt version determined the following method:
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
qtConfig :: QtConfig
{-# NOINLINE qtConfig #-}
qtConfig = unsafePerformIO readQt

qtVersion :: Version
qtVersion = configVersion qtConfig

qmakeExecutable :: FilePath
qmakeExecutable = configQmakeExecutable qtConfig

qmakeArguments :: [String]
qmakeArguments = configQmakeArguments qtConfig

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

-- | Reads a Qt version from the environment variable @QTAH_QT@, and looks up a
-- qmake binary.
readQt :: IO QtConfig
readQt = do
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
        [x] -> queryQmake $ Just x
        [x, y] -> do
          config <- queryQmake $ Just x
          case configVersion config of
            foundVersion@(x':y':_) | x /= x' || y /= y' ->
              fail $ "qtah-generator: Mismatch between requested and installed " ++
              "Qt versions.  Requested " ++ showVersion version ++ ", found " ++
              showVersion foundVersion ++ "."
            _ -> return ()
          return config
        _ -> fail $ concat
             ["qtah-generator: Internal error, incorrect parsing of QTAH_QT value ", show str, "."]
    Nothing -> queryQmake Nothing

  where -- | When we don't have a preferred qmake version, then we'll search for
        -- qmake's executables, first unqualified, then qualified by version
        -- number in decreasing order.
        allQmakeExecutableNames :: [String]
        allQmakeExecutableNames = ["qmake", "qmake-qt5", "qmake-qt4"]

        -- | When we /do/ have a prefered qmake version, then try the qualified
        -- version name first, falling back to the generic qmake executable if
        -- possible.
        qmakeExecutableNamesForVersion :: Int -> [String]
        qmakeExecutableNamesForVersion major = ["qmake-qt" ++ show major, "qmake"]

        queryQmake :: Maybe Int -> IO QtConfig
        queryQmake maybePreferredMajorVersion = do
          case maybePreferredMajorVersion of
            Nothing ->
              -- No major version preference, so take whatever Qt is available.
              queryQmake' allQmakeExecutableNames []
            Just preferredMajorVersion -> do
              -- Even though we have a preferred major version, we don't want to
              -- run "qmake -qt=X -version" initially because we might be on a
              -- system (NixOS) where qtchooser isn't available and the only
              -- qmake available *is* the desired version (in NixOS's case, the
              -- binary is called "qmake", not "qmake-qtX").  Only pass "-qt=X"
              -- if we get the wrong default version.
              let executableNames = qmakeExecutableNamesForVersion preferredMajorVersion
              defaultConfig <- queryQmake' executableNames []
              case configVersion defaultConfig of
                (x:_) | x == preferredMajorVersion -> return defaultConfig
                _ -> queryQmake' executableNames ["-qt=" ++ show preferredMajorVersion]

        queryQmake' :: [String] -> [String] -> IO QtConfig
        queryQmake' executableNames extraArgs = do
          qmakePath <- findQMake executableNames
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
            Just strs ->
              return QtConfig
              { configVersion = map (read :: String -> Int) strs
              , configQmakeExecutable = qmakePath
              , configQmakeArguments = extraArgs
              }
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

findQMake :: [String] -> IO FilePath
findQMake executableNames = lookupEnv "QTAH_QMAKE" >>= fromMaybeM findBinary
  where findBinary =
          firstM (map findExecutable executableNames) >>=
          fromMaybeM
          (fail $ "qtah-generator: Can't find qmake named any of " ++
           show executableNames ++ ".  Please ensure qmake is installed " ++
           "and set QTAH_QMAKE to qmake's path.")
