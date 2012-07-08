BINARY = bin/cocoa-clojure
RUNTIME = lib/libCocoaClojureRuntime.a
GHC = ghc
GHCFLAGS = 

all: cocoa-clojure

.PHONY: clean cocoa-clojure install repl runtime

clean:
	rm -f $(BINARY) $(RUNTIME) a.out
	rm -f compiler/*.hi
	rm -f compiler/*.o
	cd runtime && xcodebuild -alltargets clean && rm -rf build

cocoa-clojure: runtime
	$(GHC) $(GHCFLAGS) -o $(BINARY) compiler/*.hs

install: cocoa-clojure
	install $(BINARY) /usr/local/bin/

repl: cocoa-clojure
	$(BINARY)

runtime:
	cd runtime && xcodebuild -scheme 'CocoaClojureRuntime' -configuration Release build BUILD_DIR=build BUILD_ROOT=build
	cp -f runtime/build/Release/libCocoaClojureRuntime.a $(RUNTIME)
