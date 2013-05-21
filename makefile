
build:
	cabal-dev install --force-reinstalls -funsafe

build-par:
	cabal-dev install -j -funsafe

hmatrix:
	cabal-dev install -j -funsafe hmatrix --enable-library-profiling

add-sources:
	cabal-dev add-source HNN-0.1/dist/hnn-0.1.2.3.4.tar.gz
	cabal-dev add-source hnn/dist/hnn-0.2.0.0.20121218.tar.gz
	cabal-dev add-source hashable-1.2.0.2/dist/hashable-1.2.0.2.20121217.tar.gz
test:
	cabal-dev install -j -funsafe --enable-tests
prof:
	cabal-dev install -j -funsafe --enable-executable-profiling --enable-library-profiling
run:
	cabal-dev install -j -funsafe && cabal-dev/bin/abalone +RTS -sstderr -N | tee -a abalone.log.txt

run-g3:
	cabal-dev/bin/gg-exp3 +RTS -A8m -sstderr -N3 | tee -a g3.txt

run-g5:
	cabal-dev/bin/gg-exp5 +RTS -A8m -sstderr -N3 | tee -a g5.txt

tournament:
	cabal-dev install -j -funsafe && cabal-dev/bin/tournament +RTS -sstderr -N | tee -a tournament.log.txt

tournament-ii:
	cabal-dev install -funsafe && cabal-dev/bin/tournament +RTS -sstderr -N | tee -a tournament.log.txt

clean:
	cabal-dev clean


doc:
	firefox cabal-dev/share/doc/abalone-0.1/html/index.html &
