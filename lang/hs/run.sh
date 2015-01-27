#!/usr/bin/env bash

time (../../generate-bindings.sh && cabal install ~/dev/projects/cppop/generator && cabal configure && cabal run)
