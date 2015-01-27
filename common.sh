# Defensive.  Scripts should still set this themselves to start with, in case
# sourcing this file fails:
set -euo pipefail

msg() {
    echo "qtpi >>> $*"
}

run() {
    echo "*** $*"
    "$@"
}

declare -r projectDir="$(dirname "$(realpath "$0")")"
declare -r buildDir="$projectDir/../build-qtpi-Desktop-Debug"

declare -r cppopProjectDir="$projectDir/../cppop"
declare -r cppopObjDir="$projectDir/../build-cppop-Desktop-Debug"

declare -r serverInFifo="/tmp/serverin"
declare -r serverOutFifo="/tmp/serverout"
declare -r serverLogFile="/tmp/serverlog"

if ! [[ -d $cppopProjectDir ]]; then
    echo "Cppop project directory doesn't exist ($cppopProjectDir), aborting."
fi
