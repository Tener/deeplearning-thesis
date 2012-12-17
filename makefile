
build:
	cabal-dev install -j -fcairo

add-sources:
	cabal-dev add-source HNN-0.1/dist/hnn-0.1.2.3.4.tar.gz
	cabal-dev add-source hashable-1.2.0.2/dist/hashable-1.2.0.2.20121217.tar.gz

test:
	cabal-dev install -j -fcairo --enable-tests
prof:
	cabal-dev install -j --enable-executable-profiling --enable-library-profiling && cabal-dev/bin/abalone +RTS -sstderr -P
run:
	cabal-dev install -fcairo && cabal-dev/bin/abalone +RTS -sstderr -N
clean:
	cabal-dev clean

