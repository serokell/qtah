-- This file is part of Qtah.
--
-- Copyright 2016 Bryan Gardiner <bog@khumba.net>
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
import Graphics.UI.Qtah.Widgets.QApplication (QApplication)
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import System.Exit (exitFailure)
import Test.HUnit (
  Test (TestList),
  (~:),
  errors,
  failures,
  runTestTT,
  )

main :: IO ()
main = withScopedPtr (QApplication.new ([] :: [String])) $ \app -> do
  counts <- runTestTT $ tests app
  when (errors counts /= 0 || failures counts /= 0) exitFailure

tests :: QApplication -> Test
tests app =
  TestList
  [ "Graphics.UI.Qtah.Event" ~: Graphics.UI.Qtah.EventTest.tests app
  ]
