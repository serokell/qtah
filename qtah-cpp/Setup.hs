-- This file is part of Qtah.
--
-- Copyright 2016-2017 Bryan Gardiner <bog@khumba.net>
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

{-# OPTIONS_GHC -W -fwarn-incomplete-patterns -fwarn-unused-do-bind #-}

import Control.Applicative ((<|>))
import Control.Monad (forM_, unless, when)
import Data.Char (isDigit)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe)
import Distribution.Package (PackageName (PackageName), pkgName)
import Distribution.PackageDescription (FlagName (FlagName), PackageDescription, package)
import Distribution.Simple (defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (
  LocalBuildInfo,
  absoluteInstallDirs,
  buildDir,
  libdir,
  localPkgDescr,
  withPrograms,
  )
import Distribution.Simple.Program (
  Program,
  ProgramConfiguration,
  getProgramOutput,
  lookupProgram,
  runDbProgram,
  simpleProgram,
  )
import Distribution.Simple.Setup (
  BuildFlags,
  CleanFlags,
  ConfigFlags,
  CopyDest (CopyTo, NoCopyDest),
  buildVerbosity,
  cleanVerbosity,
  configConfigurationsFlags,
  configVerbosity,
  copyDest,
  copyVerbosity,
  flagToMaybe,
  fromFlagOrDefault,
  installDistPref,
  installVerbosity,
  )
import Distribution.Simple.UserHooks (
  UserHooks (
    buildHook,
    hookedPrograms,
    cleanHook,
    copyHook,
    instHook,
    postConf
    ),
  )
import Distribution.Simple.Utils (
  die,
  info,
  installExecutableFile,
  installOrdinaryFile,
  notice,
  warn,
  )
import Distribution.Verbosity (Verbosity, normal)
import System.Directory (
  createDirectoryIfMissing,
  doesDirectoryExist,
  getCurrentDirectory,
  getDirectoryContents,
  setCurrentDirectory,
  removeFile,
  )
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Environment (lookupEnv, setEnv)
import System.FilePath ((</>), takeDirectory)
import System.Posix (createSymbolicLink, getSymbolicLinkStatus)
import System.Process (callProcess)

packageName :: String
-- Careful, this line is modified by set-qt-version.sh.
packageName = "qtah-cpp"

main :: IO ()
main = defaultMainWithHooks qtahHooks

qtahHooks :: UserHooks
qtahHooks = simpleUserHooks
  { hookedPrograms = [generatorProgram, listenerGenProgram, makeProgram]
  , postConf = \_ cf _ lbi -> do generateSources cf lbi
  , buildHook = \pd lbi uh bf -> do doBuild lbi bf
                                    buildHook simpleUserHooks pd lbi uh bf
  , copyHook = \pd lbi uh cf -> do let verbosity = fromFlagOrDefault normal $ copyVerbosity cf
                                       dest = fromFlagOrDefault NoCopyDest $ copyDest cf
                                   doInstall verbosity pd lbi dest
                                   copyHook simpleUserHooks pd lbi uh cf
  , instHook = \pd lbi uh if' -> do let verbosity = fromFlagOrDefault normal $ installVerbosity if'
                                        dest = maybe NoCopyDest CopyTo $
                                               flagToMaybe $ installDistPref if'
                                    doInstall verbosity pd lbi dest
                                    instHook simpleUserHooks pd lbi uh if'
  , cleanHook = \pd z uh cf -> do doClean cf
                                  cleanHook simpleUserHooks pd z uh cf
  }

generatorProgram :: Program
generatorProgram = simpleProgram "qtah-generator"

listenerGenProgram :: Program
listenerGenProgram = simpleProgram "qtah-listener-gen"

makeProgram :: Program
makeProgram = simpleProgram "make"

findQmake :: ProgramConfiguration -> Verbosity -> IO (FilePath, [String])
findQmake programDb verbosity = do
  generatorConfiguredProgram <-
    maybe (die $ packageName ++ ": Couldn't find qtah-generator.  Is it installed?") return $
    lookupProgram generatorProgram programDb
  output <- fmap lines $
            getProgramOutput verbosity generatorConfiguredProgram ["--qmake-executable"]
  case output of
    executable:args -> return (executable, args)
    [] -> fail $ packageName ++
          ": Couldn't ask qtah-generator for the location of qmake.  Received no output."

generateSources :: ConfigFlags -> LocalBuildInfo -> IO ()
generateSources configFlags localBuildInfo = do
  startDir <- getCurrentDirectory
  let cppSourceDir = startDir </> "cpp"
      programDb = withPrograms localBuildInfo
      verbosity = fromFlagOrDefault normal $ configVerbosity configFlags

  -- Parse the Qt version to use from flags and the environment, and export it
  -- to the generator.
  _ <- exportQtVersion configFlags localBuildInfo

  -- Generate binding source code.
  runDbProgram verbosity generatorProgram programDb ["--gen-cpp", "cpp"]
  runDbProgram verbosity listenerGenProgram programDb ["--gen-cpp-dir", "cpp"]

  -- Run qmake to generate the makefile.
  setCurrentDirectory cppSourceDir
  (qmakeExecutable, qmakeArguments) <- findQmake programDb verbosity
  callProcess qmakeExecutable $ qmakeArguments ++ ["qtah.pro"]

  setCurrentDirectory startDir

doBuild :: LocalBuildInfo -> BuildFlags -> IO ()
doBuild localBuildInfo buildFlags = do
  startDir <- getCurrentDirectory
  let cppSourceDir = startDir </> "cpp"
      programDb = withPrograms localBuildInfo
      verbosity = fromFlagOrDefault normal $ buildVerbosity buildFlags

  -- Determine how many parallel build jobs to use.
  --
  -- TODO Eventually require Cabal >=1.20 and use BuildFlags.numBuildJobs
  -- instead.
  numJobsStr <- lookupEnv "QTAH_BUILD_JOBS"
  (makeArgs, jobMsg) <- case numJobsStr of
    Nothing -> return ([], "")
    Just "" -> return ([], "")
    Just s ->
      if all isDigit s
      then do let n = read s :: Int
              return (["-j" ++ show n],
                      concat [" with ", show n, if n == 1 then " job" else " jobs"])
      else do warn verbosity $ concat
                [packageName, ": Unknown QTAH_BUILD_JOBS=", show s,
                 ", expected a positive integer."]
              return ([], "")

  setCurrentDirectory cppSourceDir
  notice verbosity $ concat ["Building the Qtah C++ library", jobMsg, "..."]
  runDbProgram verbosity makeProgram programDb makeArgs

  setCurrentDirectory startDir

doInstall :: Verbosity -> PackageDescription -> LocalBuildInfo -> CopyDest -> IO ()
doInstall verbosity packageDesc localBuildInfo copyDest = do
  startDir <- getCurrentDirectory
  let cppSourceDir = startDir </> "cpp"
      libDir = libdir $ absoluteInstallDirs packageDesc localBuildInfo copyDest

  -- Install the built library into the package's libdir.
  createDirectoryIfMissing True libDir
  forM_ ["libqtah.so", "libqtah.so.0", "libqtah.so.0.2", "libqtah.so.0.2.0"] $ \p -> do
    let path = libDir </> p
    shouldDelete <-
      catchIOError (do _ <- getSymbolicLinkStatus path
                       return True)
      (\e -> if isDoesNotExistError e then return False else ioError e)
    when shouldDelete $ removeFile path
  installExecutableFile verbosity
                        (cppSourceDir </> "libqtah.so.0.2.0")
                        (libDir </> "libqtah.so.0.2.0")
  createSymbolicLink "libqtah.so.0.2.0" (libDir </> "libqtah.so.0.2")
  createSymbolicLink "libqtah.so.0.2" (libDir </> "libqtah.so.0")
  createSymbolicLink "libqtah.so.0" (libDir </> "libqtah.so")

  -- Also record what version of Qt we are using, so that qtah can check that
  -- it's using the same version.
  installOrdinaryFile verbosity
                      (buildDir localBuildInfo </> "qtah-qt-version")
                      (libDir </> "qtah-qt-version")

doClean :: CleanFlags -> IO ()
doClean cleanFlags = do
  startDir <- getCurrentDirectory

  -- Remove generated C++ sources and build outputs.
  let cppDir = startDir </> "cpp"
  cppDirExists <- doesDirectoryExist cppDir
  when cppDirExists $
    mapM_ (delFile cppDir) .
    filter (\file ->
              "b_" `isPrefixOf` file ||
              "moc_" `isPrefixOf` file ||
              "libqtah.so" `isPrefixOf` file ||
              ".o" `isSuffixOf` file ||
              file `elem` ["Makefile",
                           "callback.cpp", "callback.hpp",
                           "listener.cpp", "listener.hpp"]) =<<
    getDirectoryContents cppDir

  where verbosity = fromFlagOrDefault normal $ cleanVerbosity cleanFlags

        delFile dir file = do
          let path = dir </> file
          info verbosity $ concat ["Removing file ", path, "."]
          removeFile path

-- | This function should be called in a 'postConf' hook.  It determines the
-- requested Qt version based on package flags and the program environment, and
-- sets the environment variables @QTAH_QT@ and @QT_SELECT@ appropriately.
--
-- The mutually exclusive package flags @qt4@ and @qt5@ specify a preference on
-- a major version of Qt.  Additionally, the environment variable @QTAH_QT@ can
-- either be @x@ or @x.y@ to specify a major or minor version of Qt,
-- respectively.  If both QTAH_QT and a flag is specified, they must agree on
-- the major version of Qt.  If using QTAH_QT, it only needs to be set for the
-- configure phase.  If neither flags nor QTAH_QT are set, then the system
-- default Qt version (as determined by @qmake@) will be used.  This may be
-- influenced by @qtchooser@.
--
-- If this package's name ends with @-qtX@, then Qt X (major version only) is
-- used unconditionally.  This overrides the above methods.
--
-- This returns the preferred major version of Qt, if there is a preference
-- (@Maybe Int@), along with the Qt version string returned from qtah-generator
-- (@String@).
--
-- !!! KEEP THIS FUNCTION IN SYNC WITH qtah/Setup.hs !!!
exportQtVersion :: ConfigFlags -> LocalBuildInfo -> IO String
exportQtVersion configFlags localBuildInfo = do
  let verbosity = fromFlagOrDefault normal $ configVerbosity configFlags
      programDb = withPrograms localBuildInfo

  -- Determine what version of Qt to use.  If we have a Qt version preference
  -- specified, either through package flags or through QTAH_QT, then
  -- maybeQtMajor will get that value.
  let PackageName myName = pkgName $ package $ localPkgDescr localBuildInfo
  maybeQtMajor <- case reverse myName of
    -- If the package name ends in "-qtX", then build for Qt X (whatever the
    -- available minor version is).  Ignore QTAH_QT and package flags.
    n:'t':'q':'-':_ | isDigit n -> do
      setEnv "QTAH_QT" [n]
      notice verbosity $ concat [packageName, ": Requesting Qt ", [n], " because of package name."]
      return $ Just (read [n] :: Int)

    -- Otherwise, we'll inspect the environment and flags.
    _ -> do
      -- Inspect the 'qt4' and 'qt5' package flags.
      let flags = configConfigurationsFlags configFlags
          qt4Flag = fromMaybe False $ lookup (FlagName "qt4") flags
          qt5Flag = fromMaybe False $ lookup (FlagName "qt5") flags
          qtFlag = if qt4Flag then Just 4 else if qt5Flag then Just 5 else Nothing
      when (qt4Flag && qt5Flag) $
        die $ concat
        [packageName, ": The qt4 and qt5 flags are mutually exclusive.  Please select at most one."]

      -- Inspect the QTAH_QT environment variable.
      qtahQtStr <- lookupEnv "QTAH_QT"
      qtahQtMajor <- case qtahQtStr of
        Just s | not $ null s -> do
          let majorStr = takeWhile (/= '.') s
          unless (all isDigit majorStr) $
            die $ concat [packageName, ": Invalid QTAH_QT value ", show s,
                          ".  Expected a numeric version string."]
          return $ Just (read majorStr :: Int)
        _ -> return Nothing

      -- Determine which version of Qt to use, and put it in QTAH_QT for the
      -- generator to pick up.
      case (qtahQtMajor, qtFlag) of
        -- If both QTAH_QT and one of the qtX flags above is set, then they must agree.
        (Just m, Just n) -> do
          when (m /= n) $
            die $ concat
            [packageName, ": QTAH_QT=", show $ fromMaybe "" qtahQtStr, " and the qt",
             show n, " flag conflict."]
        -- Otherwise, if QTAH_QT is not already set but we have a flag preference,
        -- then use QTAH_QT to tell qtah-generator about the flag.
        (Nothing, Just n) -> setEnv "QTAH_QT" $ show n
        _ -> return ()

      -- Log a message showing which Qt we're requesting.
      case (qtahQtMajor, qtFlag) of
        (Just m, _) ->
          notice verbosity $
          concat [packageName, ": Requesting Qt ", show m, " because of QTAH_QT=",
                  show $ fromMaybe "" qtahQtStr, "."]
        (_, Just n) ->
          notice verbosity $
          concat [packageName, ": Requesting Qt ", show n, " because of the qt", show n, " flag."]
        _ -> notice verbosity $ concat [packageName, ": Requesting system default Qt."]

      return $ qtahQtMajor <|> qtFlag

  -- If we have a major version preference, then set QT_SELECT in case we're
  -- calling QMake.  We use QT_SELECT over "-qt=X" because it doesn't break when
  -- qtchooser isn't available.
  case maybeQtMajor of
    Just qtMajor -> setEnv "QT_SELECT" $ show qtMajor
    Nothing -> return ()

  -- Log a message showing which Qt qtah-generator is actually using.
  generatorConfiguredProgram <-
    maybe (die $ packageName ++ ": Couldn't find qtah-generator.  Is it installed?") return $
    lookupProgram generatorProgram programDb
  qtVersionOutput <- getProgramOutput verbosity generatorConfiguredProgram ["--qt-version"]
  qtVersion <- case lines qtVersionOutput of
    [line] -> return line
    _ -> die $ concat
         [packageName, ": Couldn't understand qtah-generator --qt-version output: ",
          show qtVersionOutput]
  notice verbosity $
    concat [packageName, ": Using Qt ", qtVersion, "."]

  -- Record the selected Qt version in a file for later installation.
  let qtVersionFile = buildDir localBuildInfo </> "qtah-qt-version"
  createDirectoryIfMissing True $ takeDirectory qtVersionFile
  writeFile qtVersionFile $ unlines [qtVersion]

  return qtVersion
