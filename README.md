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

To build and install locally, run `install.sh`, selecting the version of Qt you
want to build against in the environment:

    Build and install for Qt 5.4 with four parallel jobs:
    $ QT_SELECT=5 QTAH_QT_FLAG=qt5_4 QTAH_BUILD_JOBS=4 ./install.sh

`QTAH_QT_FLAG` instructs the generator to create bindings for a specific Qt
version.  `QT_SELECT` is a `qtchooser` variable that selects the version of
`qmake` (see `man qtchooser`).

If you want to change the version of Qt that Qtah is built against, you must
first clean the existing build outputs (`clean.sh`) before running the build
script again.

Qtah is split into three separate Cabal packages, `qtah-generator`, `qtah-cpp`,
and `qtah`, that are built in order.  The first contains a Hoppy generator; the
second builds generated C++ code; and the third builds generated Haskell code.

Packages that use Qtah should only depend on the `qtah` package.  Executables
that use Qtah should be linked dynamically, by passing the
`--enable-executable-dynamic` flag to `cabal configure` or `cabal install`.

There is a demo program in `/qtah-examples` that can be built and run after
installing Qtah:

    $ cd qtah-examples
    $ cabal configure --enable-executable-dynamic
    $ cabal run

### Dependencies

- Qt 4.x or 5.x
- GHC 7.8-7.10
- haskell-src
- hoppy-generator
- hoppy-runtime
- hoppy-std
- mtl

## Using

Qtah modules live under `Graphics.UI.Qtah`.  Each Qt class gets its own module,
and these are split based on the Qt module they belong to, for example `module
Graphics.UI.Qtah.Core.QObject`.  For the QtGui/QtWidgets split that happened in
Qt 5, classes are always associated with their Qt 5 module, even when building
for Qt 4.

There are some exceptions to this pattern:

- `Graphics.UI.Qtah.Core.Types` contains things in the top-level `Qt::`
  namespace.  This is mostly enums.  Many enums in Qt also support bitwise or on
  their values in certain contexts, so these types have both a Hoppy enum and a
  bitspace defined.

- `Graphics.UI.Qtah.Event` contains general event-handling functions.  Events
  are C++ classes in their own right, but also have an `Event` typeclass
  instance.

- `Graphics.UI.Qtah.Signal` contains functions for working with signals.
  Signals are represented by `Signal` objects in the module for the defining
  class.

- Templates: Classes such as `QList` have instantiations to separate types
  manually.  In this case, there is a separate module for each instantiation,
  e.g. `Graphics.UI.Qtah.Core.QList.QObject` represents `QList<QObject*>`.

In each class's module, there are the data types and typeclasses associated with
the C++ class, as well as Haskell functions that wrap C++ methods:

- Constructors start with `new`.  Copy constructors are called `newCopy`.

- Casting is provided by `cast` (upcast to nonconst), `castConst` (upcast to
  const), `downCast` (downcast to nonconst), and `downCastConst` (downcast to
  const).

- `encode` and `decode` functions for classes with a native Haskell type.

- All other methods, enums, etc. provided by the class.

There are many overlapping function names between these modules, so they are
meant to be imported qualified, for example:

    import qualified Graphics.UI.Qtah.Widgets.QTextEdit as QTextEdit

    te <- QTextEdit.new
    QTextEdit.setText te "Hello there."

Some types provide native Haskell types for easier manipulation.  Naturally,
`QString` maps to Haskell's `String`.  Other types, such as `QSize` and `QRect`,
come with types that start with `H` instead (`HSize` and `HRect`) in an adjacent
module.  These make it easier to construct values, since the Haskell version can
be passed anywhere a const C++ version is expected.

For working with C++ objects, you will probably also want the functionality in
`Foreign.Hoppy.Runtime`.

### Object lifetimes

Objects returned from constructors are not garbage-collected by default.  Most
of the time, this is correct, because Qt's object hierarchy ensures that objects
get deleted properly.  Objects that aren't owned by some other object that will
be deleted need to be deleted manually though, e.g. with Hoppy's `delete`
function.

Another option is to let the Haskell garbage collector manage objects, with
Hoppy's `toGc` function.  Don't use this for objects that are owned by another
object, because they will be deleted twice.  Some objects, such as `QDir`, are
returned by-value from functions but don't have a native Haskell type.  In these
cases, they are assigned to the garbage collector so that in general, you do not
have to manage objects you didn't create explicitly with a constructor call.

## Code layout

There is a Hoppy generator in `/qtah-generator`.  Within there, all API
definitions are in `src/Graphics/UI/Qtah/Internal/Interface`.  Generated
bindings end up in `/qtah-cpp` and `/qtah` for the C++ and Haskell sides,
respectively.

For each supported Qt class, Hoppy creates the module
`Graphics.UI.Qtah.Generated.<module>.<class>`.  These bindings' names are
prefixed with their class name, for example
`Graphics.UI.Qtah.Generated.Core.QPoint.qPoint_setX`.  Rather than expose this
interface directly, Qtah includes a second generator that creates wrapper
modules that are meant to be imported qualified, and leave out the class name
from their bindings:

    import qualified Graphics.UI.Qtah.Core.QPoint as QPoint

    ... QPoint.setX ...

These wrapper modules are also where Qtah adds support for signals and events,
using the core support for these in `Graphics.UI.Qtah.Signal` and
`Graphics.UI.Qtah.Event`.

## Developing

Patches welcome!  Please enable the pre-commit hook at `scripts/git-pre-commit`
which checks lint and copyright/license issues:

    $ ln -s ../../scripts/git-pre-commit .git/hooks/pre-commit

Also please try to fix warnings that your changes introduce, and follow local
style, or the
[style guide](https://gitlab.com/khumba/haskell-style/blob/master/haskell-style.md).
