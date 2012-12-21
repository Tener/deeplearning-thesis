mkdir -p /var/tmp/tener
cd /var/tmp/tener

case `uname -m` in
    x86_64 )
             wget -nc http://www.haskell.org/ghc/dist/7.4.2/ghc-7.4.2-x86_64-unknown-linux.tar.bz2 ;;
    i686 )
             wget -nc http://www.haskell.org/ghc/dist/7.4.2/ghc-7.4.2-i386-unknown-linux.tar.bz2 ;;
esac

wget -nc http://www.haskell.org/cabal/release/cabal-install-0.14.0/cabal-install-0.14.0.tar.gz

# ghc

if test -d ghc-7.4.2; then
      echo "Skipping GHC installation"
else
      rm -rf ghc-7.4.2
      tar xvf ghc-7.4.2-*-unknown-linux.tar.bz2
      cd ghc-7.4.2
      ./configure --prefix=/var/tmp/tener/opt
      make install
      cd ..
fi

# cabal-install

rm -rf cabal-install-0.14.0/
tar xvf cabal-install-0.14.0.tar.gz
cd cabal-install-0.14.0/
export DEFAULT_PREFIX=/var/tmp/tener/cabal-prefix
export PREFIX=/var/tmp/tener/cabal-prefix
export PATH=/var/tmp/tener/opt/bin:/var/tmp/tener/cabal-prefix/bin/:$PATH
DEFAULT_PREFIX=/var/tmp/tener/cabal-prefix PREFIX=/var/tmp/tener/cabal-prefix PATH=/var/tmp/tener/opt/bin:$PATH sh bootstrap.sh 

# abalone

cd ~/
cabal update
cabal --prefix=/var/tmp/tener/cabal-prefix install cabal-install
cabal --prefix=/var/tmp/tener/cabal-prefix install -j cabal-dev
rm -rfv /var/tmp/tener/abalone
git clone ~/abalone /var/tmp/tener/abalone
cd /var/tmp/tener/abalone
cabal-dev add-source ~/devel/hnn/0.2.0.0.20121218/hnn-0.2.0.0.20121218.tar.gz ~/devel/hashable/1.2.0.2.20121217/hashable-1.2.0.2.20121217.tar.gz
cabal-dev install -j -f-cairo

# execute program

mkdir -p ./data
rm -f nn.txt
ln -s ~/nn.txt
make tournament-ii

