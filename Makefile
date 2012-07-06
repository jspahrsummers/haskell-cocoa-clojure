BINARY = bin/cocoa-clojure
GHC = ghc
GHCFLAGS = -fllvm

all: cocoa-clojure

clean:
	rm -f $(BINARY)
	rm -f compiler/*.hi
	rm -f compiler/*.o

cocoa-clojure:
	$(GHC) $(GHCFLAGS) -o $(BINARY) compiler/*.hs

install: cocoa-clojure
	install $(BINARY) /usr/local/bin/

repl: cocoa-clojure
	$(BINARY)
