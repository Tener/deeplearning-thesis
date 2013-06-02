echo "-----> Build hook"
cabal-dev -j --sandbox=$CABAL_DEV_SANDBOX install --disable-library-profiling --disable-executable-profiling --disable-shared --extra-include-dirs=$BUILD_DIR/debs-extra/usr/include --extra-lib-dirs=$BUILD_DIR/debs-extra/usr/lib
