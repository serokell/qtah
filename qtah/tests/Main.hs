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

module Main (main) where

import Control.Monad (when)
import Foreign.Hoppy.Runtime (withScopedPtr)
import qualified Graphics.UI.Qtah.EventTest
import qualified Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
import Graphics.UI.Qtah.Core.QCoreApplication (QCoreApplication)
import System.Exit (exitFailure)
import Test.HUnit (
  Test (TestList),
  (~:),
  errors,
  failures,
  runTestTT,
  )

main :: IO ()
main = withScopedPtr (QCoreApplication.new ([] :: [String])) $ \app -> do
  counts <- runTestTT $ tests app
  when (errors counts /= 0 || failures counts /= 0) exitFailure

tests :: QCoreApplication -> Test
tests app =
  TestList
  [ "Graphics.UI.Qtah.Event" ~: Graphics.UI.Qtah.EventTest.tests app
  ]
