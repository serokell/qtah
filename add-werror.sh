#!/usr/bin/env bash

# This script inserts the GHC option -Werror into all Cabal files in the
# project.  This change shouldn't be committed, but it's useful during
# development to keep warning-free, since build output can be lengthly.

set -euo pipefail
myDir=$(readlink -f "$0")
myDir=$(dirname "$myDir")
cd "$myDir"

sed -i 's/-W -f/-W -Werror -f/g' $(git grep -l -e '-W -f')
