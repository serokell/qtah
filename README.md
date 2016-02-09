# Qtah - Qt bindings for Haskell

Qtah is a set of [Qt](https://www.qt.io/) bindings for Haskell, providing a
traditional imperative interface to a mature GUI toolkit.

Homepage: http://khumba.net/projects/qtah

Copyright 2015-2016 Bryan Gardiner <bog@khumba.net>

A range of successive copyright years may be written as XXXX-YYYY as an
abbreviation for listing all of the years from XXXX to YYYY inclusive,
individually.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

## Building

Dependencies:

- GHC 7.8-7.10
- haskell-src
- hoppy-generator (for compilation only)
- hoppy-runtime
- hoppy-std (for compilation only)
- mtl

Currently packaging for Qtah isn't entirely worked out, since it is split
between a C++ glue component and a Haskell component.  The `build.sh` script
takes care of building everything up to the bindings themselves (in `qtah/`).
After building, the Haskell package in `qtah/hs` is installable (`install.sh`)
and ready to use, although it's hard-coded to work with the C++ library built in
`qtah/cpp-build`.

    Build for Qt 5.4 with four threads:
    $ QT_SELECT=5 QTAH_QT_FLAG=qt5_4 MAKEOPTS=-j4 ./build.sh
    Do a user install of the Cabal package:
    $ ./install.sh

There is an example program that can be run with `run-example.sh` after building
and installing Qtah.

## Code layout

There is a Hoppy generator in `/qtah-generator`.  Within there, all API
definitions are in `src/Graphics/UI/Qtah/Internal/Interface`.  Generated
bindings go into `/qtah/cpp` and `/qtah/hs` for the C++ and Haskell sides,
respectively, and the C++ build outputs end up in `/qtah/cpp-build`.

For each supported Qt class, Hoppy creates the module
`Graphics.UI.Qtah.Generated.<module>.<class>`.  These bindings' names are
prefixed with their class name, for example
`Graphics.UI.Qtah.Generated.Core.QPoint.qPoint_setX`.  Rather than expose this
interface directly, Qtah includes a second generator that creates wrapper
modules that are meant to be imported qualified, and leave out the class name
from their bindings:

    import qualified Graphics.UI.Qtah.Core.QPoint as QPoint

    ... QPoint.setX ...

These wrapper modules are also where Qtah adds support for signals and events.
Core support for these is in `Graphics.UI.Qtah.Signal` and
`Graphics.UI.Qtah.Event`.  Signals are represented by `Signal` objects in the
module for the defining class.  Events are classes in their own right, but also
have an `Event` instance.

Most Qt classes are not convertible in the Hoppy `ClassConversion` sense.
Select classes are, including `QString` (which converts to a native Haskell
string) and some simple classes that have pure Haskell implementations to mirror
their C++ ones, such as `QPoint` and `HPoint`.  These make it easier to
construct values, since the Haskell version can be passed anywhere a const C++
version is expected.

Bindings for the `Qt::` namespace are in `Graphics.UI.Qtah.Core.Types`.  Many
enums in Qt also support bitwise or on their values in certain contexts, so
these types have both a Hoppy enum and a bitspace defined.

For templates, a separate module is created for each instantiation:
`Graphics.UI.Qtah.Core.QList.QObject` is for `QList<QObject*>`.

## Developing

When creating patches, please enable the pre-commit hook at
`scripts/git-pre-commit` which checks lint and copyright/license issues.  Also
try to ensure that your changes compile cleanly without warnings when `-W` is
used, and follow the style guide at:

https://gitlab.com/khumba/haskell-style/blob/master/haskell-style.md
