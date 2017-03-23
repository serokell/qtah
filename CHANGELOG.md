# Qtah Changelog

## Unreleased

- Dependency bump to support directory-1.3 in GHC 8.0.2.

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
