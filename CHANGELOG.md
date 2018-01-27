# Qtah Changelog

## Unreleased

## (2018-01-27) *-0.4.0

- [API change] Changed QAction's constructors so that "new" and "newWithText"
  don't require parent parameters, and have "WithParent" forms instead, like
  normal.

- [API change] `QImage::fromData*()` and `::loadFromData*()` were renamed to
  `...DataRaw*()`, because there are QByteString versions of these functions
  now.

- New classes, many of these thanks to Yuriy Syrovetskiy:
  - Core: QByteArray, QTextCodec
  - Gui: QIcon, QPixmap, QStandardItem{,Model}
  - Widgets: QSystemTrayIcon, QToolBar, QTreeView, QTreeWidget{,Item}

- Added support for Cabal 2.0 and fixed building under macOS (thanks Yuriy).

- qtah-listener-gen now checks that bash 4.1 or newer is available, as it
  requires features introduced in that version, and OS X doesn't ship a bash
  anywhere near that new (issue #19).

- install.sh now defaults to Qt 5 when no `QTAH_QT_FLAGS` environment variable
  is set, instead of selecting the system default (which is still Qt 4 on some
  Linux distros).  To get the old behaviour, set `QTAH_QT_FLAGS` to an empty
  value.

## (2017-06-10) *-0.3.0, qtah-cpp-0.3.1

37 new classes and 10 new enums in this release!

- Added support for the Qt graphics view framework thanks to @effectfully.

- Began adding model-view classes.

- Qtah now builds on OS X, with help from @biglambda.

- qtah-examples now presents a launcher, so that we can have more than one
  example program.

- Discovered that "ghc-options: -dynamic" can be used for executables and tests
  linking to Qtah in lieu of having to pass Cabal "--enable-executable-dynamic"
  on the command line (thanks again @effectfully).

- qtah-cpp now uses all available CPUs to build by default, as "cabal build"
  does (this requires Cabal >=1.20).

- Dependency bump to support directory-1.3 in GHC 8.0.2, and HUnit-1.6.

- Support for Qt <4.8 has been dropped, since even Qt 4.8 is quite old now.

## (2017-01-14) qtah-examples-0.2.1

- Add a configure warning when qtah-examples is being built without dynamic
  executable linking, since without this hint, it's not obvious at all why a
  simple "cabal install qtah-examples" fails.

## (2016-12-16) *-0.2.0, qtah-generator-0.2.1

- Makes use of the new class/enum/bitspace prefix customization feature in Hoppy
  0.3.0 to strip the repetitive class name off of enum value names, for enums
  contained within classes (issue #10).  This is an API change from 0.1.*.  (We
  also do this internally for class entities to simplify Qtah's generator.)

- Fixed the conversion from QPoint and QPointF to their H* components mistakenly
  swapping the components.

- Fixed build issue with Cabal 1.24 / GHC 8 (issue #14).

## (2016-10-01) qtah-0.1.2, qtah-examples-0.1.2

- Version bump to support binary-0.8.*.

## (2016-08-04) qtah-generator-0.1.2, qtah-cpp-0.1.2, qtah-0.1.1

- Fixes the custom install logic to install additional files into requested
  locations instead of the default system ones.

## (2016-07-30) qtah-cpp-0.1.1, qtah-examples-0.1.1

- Another fix for NixOS, qtah-cpp expected qtchooser to be available when a
  version preference was specified (issue #8).

- The notepad example is a much more usable program now.

## (2016-07-15) qtah-generator-0.1.1

- Allow `QTAH_QT=x` to work when qtchooser is not available, to fix building
  with Nixpkgs (issue #8).

## (2016-07-10) *-0.1.0

- Initial release.
