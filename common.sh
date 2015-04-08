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

declare -r projectDir="$(dirname "$(realpath "$0")")"
declare -r buildDir="$projectDir/build-cpp"

declare -r cppopProjectDir="$projectDir/../../cppop/git"

if ! [[ -d $cppopProjectDir ]]; then
    echo "Cppop project directory doesn't exist ($cppopProjectDir), aborting."
fi
