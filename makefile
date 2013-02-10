
build:
	cabal-dev install --force-reinstalls -f-cairo -funsafe

build-par:
	cabal-dev install -j -f-cairo -funsafe

hmatrix:
	cabal-dev install -j -funsafe hmatrix --enable-library-profiling

add-sources:
	cabal-dev add-source HNN-0.1/dist/hnn-0.1.2.3.4.tar.gz
	cabal-dev add-source hnn/dist/hnn-0.2.0.0.20121218.tar.gz
	cabal-dev add-source hashable-1.2.0.2/dist/hashable-1.2.0.2.20121217.tar.gz
test:
	cabal-dev install -j -funsafe -f-cairo --enable-tests
prof:
	cabal-dev install -j -funsafe -f-cairo --enable-executable-profiling --enable-library-profiling && cabal-dev/bin/abalone +RTS -sstderr -P
run:
	cabal-dev install -j -funsafe -f-cairo && cabal-dev/bin/abalone +RTS -sstderr -N | tee abalone.log.txt

tournament:
	cabal-dev install -j -funsafe -f-cairo && cabal-dev/bin/tournament +RTS -sstderr -N | tee tournament.log.txt

tournament-ii:
	cabal-dev install -funsafe -f-cairo && cabal-dev/bin/tournament +RTS -sstderr -N | tee tournament.log.txt

clean:
	cabal-dev clean

