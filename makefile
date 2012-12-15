
test:
	cabal-dev install -j -fcairo --enable-tests
prof:
	cabal-dev install --enable-executable-profiling --enable-library-profiling && cabal-dev/bin/abalone +RTS -sstderr -P
run:
	cabal-dev install -fcairo && cabal-dev/bin/abalone +RTS -sstderr -N
clean:
	cabal-dev clean

