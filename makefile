
build:
	cabal-dev install -fmatlab -fbuild-utils -fbuild-experiments --force-reinstalls -funsafe

build-par:
	cabal-dev install -fmatlab -fbuild-utils -fbuild-experiments -j -funsafe

hmatrix:
	cabal-dev install -j -funsafe hmatrix --enable-library-profiling

test:
	cabal-dev install -j -funsafe --enable-tests
prof:
	cabal-dev install -j -funsafe --enable-executable-profiling --enable-library-profiling
run:
	cabal-dev install -j -funsafe && cabal-dev/bin/abalone +RTS -sstderr -N | tee -a abalone.log.txt

run-g3:
	cabal-dev/bin/gg-exp3 +RTS -A8m -sstderr -N${JOBS} | tee -a g3.txt

run-g5:
	cabal-dev/bin/gg-exp5 +RTS -A8m -sstderr -N${JOBS} | tee -a g5.txt

run-g6:
	cabal-dev/bin/gg-exp6 +RTS -A8m -sstderr -N${JOBS} | tee -a g6.txt

run-g7:
	cabal-dev/bin/gg-exp7 +RTS -A8m -sstderr -N${JOBS} | tee -a g7.txt

tournament:
	cabal-dev install -j -funsafe && cabal-dev/bin/tournament +RTS -sstderr -N | tee -a tournament.log.txt

tournament-ii:
	cabal-dev install -funsafe && cabal-dev/bin/tournament +RTS -sstderr -N | tee -a tournament.log.txt

clean:
	cabal-dev clean

doc:
	firefox cabal-dev/share/doc/deeplearning-0.2/html/index.html &

cli:
	runhaskell src/experiment-cli.hs

report:
	cabal-dev/bin/experiment-progress-email-reporter ${USER} ${USER} ${USER} ${USER}

webui:
	runhaskell -package-conf=cabal-dev/packages-`ghc --numeric-version`.conf gui/breakthrough-gui.hs

dbn-converter:
	runhaskell -package-conf=cabal-dev/packages-`ghc --numeric-version`.conf src/dbn-converter.hs gui/assets/*.txt
