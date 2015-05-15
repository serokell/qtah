-- | Utilities for conditional compilation of parts of Qt interfaces.
module Graphics.UI.Qtah.Internal.Flag (
  collect,
  just,
  test,
  ) where

import Data.Maybe (catMaybes)

collect :: [Maybe a] -> [a]
collect = catMaybes

just :: a -> Maybe a
just = Just

test :: Bool -> a -> Maybe a
test True = Just
test False = const Nothing
