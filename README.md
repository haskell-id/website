clone
cabal sandbox init
cabal install --only-dependencies
cabal install

./.cabal-sanbox/bin/site watch
