BINARY = bin/cocoa-clojure
GHC = ghc
GHCFLAGS = 

all: cocoa-clojure

clean:
	rm -f $(BINARY)
	rm -f compiler/*.hi
	rm -f compiler/*.o
	cd runtime && xcodebuild -alltargets clean

cocoa-clojure:
	$(GHC) $(GHCFLAGS) -o $(BINARY) compiler/*.hs

install: cocoa-clojure
	install $(BINARY) /usr/local/bin/

repl: cocoa-clojure
	$(BINARY)

runtime:
	cd runtime
	xcodebuild -scheme 'CocoaClojureRuntime' build
