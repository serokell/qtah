# Qtah - Qt bindings for Haskell

Qtah is a set of [Qt](https://www.qt.io/) bindings for Haskell, providing a
traditional imperative interface to a mature GUI toolkit.

Homepage: http://khumba.net/projects/qtah

Copyright 2015-2018 The Qtah Authors.

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

## Table of Contents

- Building
  - Dependencies
  - Qt version selection
  - Release versions
- Using
  - Object lifetimes
- Developing
  - Code layout
  - Extending the Qt API

## Building

Qtah is split into three separate Cabal packages, `qtah-generator`, `qtah-cpp`,
and `qtah`, that are built in order.  The first contains a Hoppy generator; the
second builds generated C++ code; and the third builds generated Haskell code.
On Hackage, `qtah-cpp` and `qtah` have `-qtX` variants for specific major
versions of Qt.

To build and install locally, run:

    $ ./install.sh

If you want to instead build against Qt 4, then run:

    $ QTAH_QT_FLAGS=qt4 ./install.sh

The `install.sh` script is just a thin wrapper around running `cabal configure`,
`build`, `install` on each of the packages in turn.  For more information about
controlling the version of Qt used and the `-qtX` package name variants, see the
subsections on Qt version selection and release versions.  If you want to change
the version of Qt that Qtah is built against, you must first clean the existing
build outputs (`clean.sh`) before running the build script again.

Packages that use Qtah should only depend on a `qtah-qtX` package (or just
`qtah` if building against Qtah git; see "Release versions" below).  Executables
that use Qtah need to be linked dynamically, so put `ghc-options: -dynamic` in
your Cabal file.  This includes unit tests.

There is a demo program in `/qtah-examples` that can be built and run after
installing Qtah:

    $ cd qtah-examples
    $ cabal run

### Dependencies

- Qt 4.8 or 5.x with development files
- make and a C++ compiler
- bash 4.1 or newer
- GHC 7.8-8.2
- haskell-src
- hoppy-generator
- hoppy-runtime
- hoppy-std
- HUnit (for tests)
- mtl

On Debian and derivatives:

    apt-get install g++ make
    # For Qt 5:
    apt-get install qtbase5-dev  # Required.
    apt-get install qtbase5-doc qttools5-dev-tools  # For Assistant (Qt API documentation).
    # For Qt 4:
    apt-get install libqt4-dev  # Required.
    apt-get install qt4-dev-tools  # For Assistant (Qt API documentation).

On Fedora and derivatives:

    dnf install gcc-c++ make
    # For Qt 5:
    dnf install qt5-qtbase-devel  # Required.
    dnf install qt5-qtbase-doc qt5-qtdoc qt5-assistant  # For Assistant (Qt API documentation).
    # For Qt 4:
    dnf install qt-devel  # Required.
    dnf install qt-doc qt-assistant  # For Assistant (Qt API documentation).

### Qt version selection

Qtah uses custom Cabal build scripts to tie it's pieces together and to Qt.
There are a few different ways to control the Qt version; we'll describe them
from the lowest-level one up.

The `qtah-generator` package takes the Qt version to use at runtime.  By
default, it uses whatever version of Qt is provided by QMake (via `qmake
-version`).  If qtchooser is installed on your system, then you can select from
multiple versions of Qt with e.g. `qmake -qt=5` or `QT_SELECT=5 qmake` (valid
values here are shown by running `qtchooser -list-versions`).  So putting
`QT_SELECT` in the environment when building is one way to select the version of
Qt that Qtah will use.

`qtah-generator` also supports a `QTAH_QT` environment variable.  This takes
precedence over `QT_SELECT`, and can take version numbers of the form `x.y` or
`x` (for example `5.4` or `5`).  If given `x.y`, then Qt _x.y_ will be used, no
questions asked.  If given `x`, then QMake will be queried for the version of Qt
to use.  First `qmake -version` will be queried, and if this turns out to be a
different major version of Qt, then `qmake -qt=x -version` will be queried (this
lets `QTAH_QT=x` work on systems such as NixOS that activate a single Qt and
don't have qtchooser).  So putting `QTAH_QT` in the environment when building is
another way to select the version of Qt to use, and unlike `QT_SELECT`, you can
force a specific minor version.

Rather than environment variables, the preferred way of specifying a version is
to set the `qt4` or `qt5` package flags on `qtah-cpp` and `qtah`.  This is
equivalent to setting `QTAH_QT` but is tracked by Cabal.  At most one of these
flags may be set, and if `QTAH_QT` is set as well, then they must agree.  When
using `install.sh`, the environment variable `QTAH_QT_FLAGS` will be passed via
`--flags` to these two packages.  This variable defaults to `qt5` when unset
(but not when empty).

Whether using an environment variable or a flag to specify a Qt version, it
needs to be specified for both `cabal configure` and `cabal install`.

The path to the QMake executable can be specified in the environment variable
`QTAH_QMAKE`, if Qtah can't find it or you wish to override it (it's used while
building `qtah-cpp`).

### Release versions

Different major versions of Qt can be installed in parallel.  To extend this to
Haskell, Qtah supports building variant packages for each major Qt version.
`qtah-cpp` and `qtah` both have `-qtX` variants, e.g. `qtah-cpp-qt5` and
`qtah-qt5`.  These are what we upload to Hackage.  `qtah-generator` works for
all Qt versions and doesn't need variants.  We choose separate package names
because while Cabal supports installing multiple versions of a package at once,
many other package managers don't.

To create these, run e.g. `scripts/set-qt-version 5` before running
`install.sh`.  `set-qt-version` renames packages and does source patching as
necessary for the variant packages to work with each other.  There isn't an undo
script and `set-qt-version` can't be run multiple times in a row; use Git to
reset back to HEAD instead.

When `qtah-cpp` and `qtah` have `-qtX` on the end of their package names, they
always use that major version of Qt, regardless of `QTAH_QT` and `QT_SELECT`.
The `qtX` package flags are removed by `set-qt-version`.

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

## Developing

Patches welcome!  Please enable the pre-commit hook at `scripts/git-pre-commit`
which checks lint and copyright/license issues:

    $ ln -s ../../scripts/git-pre-commit .git/hooks/pre-commit

Also please try to fix warnings that your changes introduce, and follow local
style, or the
[style guide](https://gitlab.com/khumba/haskell-style/blob/master/haskell-style.md).

### Code layout

There is a Hoppy generator in `/qtah-generator`.  Within there, all API
definitions are in `src/Graphics/UI/Qtah/Generator/Interface`.  Qtah uses the
following prefixes for naming C++ bindings in the generator:

- `c_MyClass` for classes.
- `e_MyEnum` for enums.
- `bs_MyBitspace` for bitspaces.
- `f_MyFunction` for functions.
- `cb_MyCallback` for callbacks.

Generated bindings end up in `/qtah-cpp` and `/qtah` for the C++ and Haskell
sides, respectively.  For each supported Qt class, Hoppy creates the module
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

Items in the `Qt::` namespace go into
[Types.hs](qtah-generator/src/Graphics/UI/Qtah/Generator/Interface/Core/Types.hs).

There are some circular dependencies among API definition modules, such as
between `QString` and `QChar`.  In these cases, GHC's circular import support
makes this easy to solve: pick one of the modules in the cycle, create a
[.hs-boot](qtah-generator/src/Graphics/UI/Qtah/Generator/Interface/Core/QString.hs-boot)
file, and change the
[dependent module(s)](qtah-generator/src/Graphics/UI/Qtah/Generator/Interface/Core/QChar.hs)
to use `import {-# SOURCE #-}`.

### Extending the Qt API

#### To add a method to an existing class

Declare the method in the class's interface file
`qtah-generator/src/Graphics/UI/Qtah/Generator/Interface/<module>/<class>.hs`.

Check the Qt documentation for mention of when your function was introduced.  If
you find this out, then add a version check to the method.  See for example
[QSpinBox](qtah-generator/src/Graphics/UI/Qtah/Generator/Interface/Widgets/QSpinBox.hs)
for how this is done with `test` and `collect`.

#### To add a class

Create a source file in the generator for your class (say `QFoo`):

`qtah-generator/src/Graphics/UI/Qtah/Generator/Interface/Widgets/QFoo.hs`.

Use a similar class as a prototype.  Use `AQtModule` to create declare a module
for your class (this encompasses both a Haskell module and a Hoppy module).
List the items that the module will export; this includes your class and may
include other things such as enums and signals.

You also need to add your new module to the following places:

- `qtah-generator/src/Graphics/UI/Qtah/Generator/Interface/Widgets.hs`: This
  ties your definitions into the generator.

- `qtah-generator/qtah-generator.cabal`: Your definitions need to be part of the
  generator package.

- `qtah/qtah.cabal` **twice**: These are the API that the generator produces.
  Add `Graphics.UI.Qtah.Widgets.QFoo` to `exposed-modules` and
  `Graphics.UI.Qtah.Generated.Widgets.QFoo` to `other-modules`.  (These are
  actually both generated modules.)

Check the Qt documentation for mention of when your class was introduced.  If
you find this out, then use `makeQtModuleWithMinVersion` instead of
`makeQtModule`.  See
[QFormLayout](qtah-generator/src/Graphics/UI/Qtah/Generator/Interface/Widgets/QFormLayout.hs)
as an example.

**Note:** For classes that moved from QtGui in Qt 4 to QtWidgets in Qt 5, these
class files always live under `Widgets` in Qtah.

#### To add an enum or bitspace

These are generally associated with a class.  Use `makeQtEnum` or
`makeQtEnumBitspace` and include it in the associated class's module.  See
[QMessageBox](qtah-generator/src/Graphics/UI/Qtah/Generator/Interface/Widgets/QMessageBox.hs)
as an example.

#### To add a signal

Declare your signals in a list using `makeSignal` and export them in the
associated class's module with `QtExportSignal`.  As an example, see
[QLineEdit](qtah-generator/src/Graphics/UI/Qtah/Generator/Interface/Widgets/QLineEdit.hs).

Use `test`/`collect` as necessary for signals with minimum versions different
from their classes'.

Each type of argument list that a signal can use must have associated listener
classes and callbacks to provide machinery needed for the signal.  For example,
the signal `QLineEdit::textChanged(QString)` uses a listener class
`c_ListenerQString` which uses a callback named `cb_QStringVoid`.  The
translation from `c_Listener<args>` to `cb_<args>Void` is automatic.  Listeners
can be added to [qtah-listener-gen](qtah-generator/qtah-listener-gen) and
callbacks to
[Callback.hs](qtah-generator/src/Graphics/UI/Qtah/Generator/Interface/Internal/Callback.hs).
