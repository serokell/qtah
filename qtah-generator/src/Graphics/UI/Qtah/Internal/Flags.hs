-- | Haskell definitions for preprocessor flags that Qt uses for conditional
-- compilation.
--
-- A list of flags enabled on your system can be obtained with:
--
-- > gcc -dM -E $(pkg-config --cflags QtCore) /usr/include/qt4/Qt/qconfig.h | grep '#define QT'
--
-- Using @qglobal.h@ and @#define Q@ provides additional defintions,
-- e.g. version and windowing system information.
module Graphics.UI.Qtah.Internal.Flags (
  keypadNavigation,
  qdoc,
  wsWince,
  ) where

keypadNavigation :: Bool
keypadNavigation = False

qdoc :: Bool
qdoc = False

wsWince :: Bool
wsWince = False
