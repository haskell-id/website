# Website Haskell-id

Kode sumber website haskell-id. Powered by [hakyll](http://jaspervdj.be/hakyll/index.html)

## Menjalankan secara Lokal

**Dependensi:**

- GHC (Glasgow Haskell Compiler)
- cabal minimal versi 1.18 (fitur sanbox)

```
git clone git@github.com:haskell-id/website.git
cd website/
cabal sandbox init
cabal install --only-dependencies
cabal install
./.cabal-sanbox/bin/site watch
```
