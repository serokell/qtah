#!/usr/bin/env bash

time (../../generate-bindings.sh && cabal install ~/dev/projects/hoppy/generator && cabal configure && cabal run)
