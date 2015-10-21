# Qtah - Qt bindings for Haskell

Qtah is a set of [Qt](https://www.qt.io/) bindings for Haskell, providing a
traditional imperative interface to a mature GUI toolkit.

Homepage: http://khumba.net/projects/qtah

Qtah is free software under the GNU Affero General Public License version 3,
the terms of which are in the `LICENSE` file.  I, Bryan Gardiner, reserve the
right (a) to release all AGPL parts of Qtah under a future version of the AGPL
per section 14 of the AGPLv3, at my sole discretion, as well as the right (b) to
extend rights (a) and (b) to another entity.  By offering contributions to the
project, you accept these terms, and agree to license your contributions under
the project's current license(s) at the time of your submission.

Copyright 2015 Bryan Gardiner <bog@khumba.net>

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
