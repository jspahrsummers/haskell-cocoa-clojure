BINARY = bin/llvm-clojure
GHC = ghc
GHCFLAGS = -fllvm

all: llvm-clojure

clean:
	rm -f $(BINARY)
	rm -f compiler/*.hi
	rm -f compiler/*.o

llvm-clojure:
	$(GHC) $(GHCFLAGS) -o $(BINARY) compiler/*.hs

install: llvm-clojure
	install $(BINARY) /usr/local/bin/

repl: llvm-clojure
	$(BINARY)
