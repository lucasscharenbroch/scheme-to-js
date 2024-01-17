PACKAGES := -package parsec
OPTIMIZATION :=
GHC := ghc -dynamic -no-keep-hi-files -no-keep-o-files $(PACKAGES) $(OPTIMIAZATION)
GHCi := ghci -dynamic -fbreak-on-error $(PACKAGES)
SOURCE_FILES := src/*
EXE_NAME := s2j

bin/$(EXE_NAME): $(SOURCE_FILES)
	cd src; $(GHC) -o ../bin/$(EXE_NAME) Main.hs

clean:
	rm bin/ai

debug:
	cd src; $(GHCi) Main.hs
