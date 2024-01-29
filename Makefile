# This Makefile is primarily a wrapper for cabal

PACKAGES := -package parsec
OPTIMIZATION :=
GHC := ghc -dynamic -no-keep-hi-files -no-keep-o-files $(PACKAGES) $(OPTIMIAZATION)
GHCi := ghci -dynamic -fbreak-on-error $(PACKAGES)
SOURCE_FILES := src/*
EXE_NAME := s2j


build:
	cabal build

run:
	cabal run

debug:
	cd src; $(GHCi) Main.hs

test: FORCE
	cabal run -- s2j test/test*
	node out.scm.js

FORCE: ;

# bin/$(EXE_NAME): $(SOURCE_FILES)
# 	cd src; $(GHC) -o ../bin/$(EXE_NAME) Main.hs
# 
# clean:
# 	rm bin/s2j
