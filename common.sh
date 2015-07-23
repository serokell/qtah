# Defensive.  Scripts should still set this themselves to start with, in case
# sourcing this file fails:
set -euo pipefail

msg() {
    echo "qtah >>> $*"
}

run() {
    echo "*** $*"
    "$@"
}

declare -r cppBuildDir="$(readlink -f "$projectDir/qtah/cpp-build")"
