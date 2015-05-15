-- | Haskell definitions for preprocessor flags that Qt uses for conditional
-- compilation.
--
-- A list of flags enabled on your system can be obtained with:
--
-- > gcc -dM -E $(pkg-config --cflags QtCore) Qt/qconfig.h | grep '#define QT'
--
-- Using @qglobal.h@ instead of @qconfig.h@ provides additional defintions,
-- e.g. version information.
module Graphics.UI.Qtah.Internal.Flags (
  keypadNavigation,
  qdoc,
  ) where

keypadNavigation :: Bool
keypadNavigation = False

qdoc :: Bool
qdoc = False
