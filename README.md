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

- GHC >=7.10
- Hoppy

Currently packaging for Qtah isn't entirely worked out, since it is split
between a C++ glue component and a Haskell component.  The `build.sh` script
takes care of building everything up to the bindings themselves (in `qtah/`).
After building, the Haskell package in `qtah/hs` is installable and ready to
use, although it's hard-coded to work with the C++ library built in
`qtah/cpp-build`.

## Developing

When creating patches, please enable the pre-commit hook at
`scripts/git-pre-commit` which checks lint and copyright/license issues.  Also
try to ensure that your changes compile cleanly without warnings when `-W` is
used, and follow the style guide at:

http://khumba.net/projects/haskell-style
