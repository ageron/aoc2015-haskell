TARGET = aoc2015-haskell

all: $(TARGET)

$(TARGET): *.hs
	cabal build

run:
	cabal run $(TARGET) -- $(filter-out $@,$(MAKECMDGOALS))

clean:
	rm -rf dist-newstyle *.fls *.hi *.o *.aux *.fdb_latexmk *.log *.gz *.toc *.pdf *.dvi

%:
	@:

.PHONY: clean all run