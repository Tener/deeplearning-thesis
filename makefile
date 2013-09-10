build:
	cabal install --only-dependencies -fmatlab -fbuild-utils -fbuild-experiments --force-reinstalls -funsafe
	cabal configure -fmatlab -fbuild-utils -fbuild-experiments -funsafe
	cabal build
	cabal copy
	cabal register

build-dev:
	cabal-dev install --only-dependencies -fmatlab -fbuild-utils -fbuild-experiments --force-reinstalls -funsafe
	cabal-dev configure -fmatlab -fbuild-utils -fbuild-experiments -funsafe
	cabal-dev build
	cabal-dev copy
	cabal-dev register

build-par:
	cabal install -fmatlab -fbuild-utils -fbuild-experiments --force-reinstalls -j -funsafe

hmatrix:
	cabal-dev install -j -funsafe hmatrix --enable-library-profiling

test:
	cabal-dev install -j -funsafe --enable-tests
prof:
	cabal-dev install -j -funsafe --enable-executable-profiling --enable-library-profiling
run:
	cabal-dev install -j -funsafe && .cabal-sandbox/bin/abalone +RTS -sstderr -N | tee -a abalone.log.txt

run-g3:
	.cabal-sandbox/bin/gg-exp3 +RTS -A8m -sstderr -N4 | tee -a g3.txt

run-g5:
	.cabal-sandbox/bin/gg-exp5 +RTS -A8m -sstderr -N4 | tee -a g5.txt

run-g6:
	.cabal-sandbox/bin/gg-exp6 +RTS -A8m -sstderr -N4 | tee -a g6.txt

run-g7:
	.cabal-sandbox/bin/gg-exp7 +RTS -A8m -sstderr -N4 | tee -a g7.txt

run-g8:
	.cabal-sandbox/bin/gg-exp8 +RTS -A8m -sstderr -N4 | tee -a g8.txt

run-g9:
	.cabal-sandbox/bin/gg-exp9 +RTS -A8m -sstderr -N4 | tee -a g9.txt

run-g10:
	.cabal-sandbox/bin/gg-exp10 +RTS -A8m -sstderr -N4 | tee -a g10.txt

tournament:
	cabal-dev install -j -funsafe && .cabal-sandbox/bin/tournament +RTS -sstderr -N | tee -a tournament.log.txt

tournament-ii:
	cabal-dev install -funsafe && .cabal-sandbox/bin/tournament +RTS -sstderr -N | tee -a tournament.log.txt

clean:
	cabal-dev clean

doc:
	firefox .cabal-sandbox/share/doc/deeplearning-0.2/html/index.html &

cli:
	runhaskell src/experiment-cli.hs

report:
	.cabal-sandbox/bin/experiment-progress-email-reporter ${USER} ${USER} ${USER} ${USER}

webui:
	runhaskell -package-conf=.cabal-sandbox/packages-`ghc --numeric-version`.conf gui/breakthrough-gui.hs

dbn-converter:
	runhaskell -package-conf=.cabal-sandbox/packages-`ghc --numeric-version`.conf src/dbn-converter.hs gui/assets/*.txt
