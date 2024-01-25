#!/bin/bash

# run this script from the root directory of the project
# (otherwise cabal run won't work)

cabal run -- s2j test/test*
node out.scm.js
