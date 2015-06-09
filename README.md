# Website Haskell-id

Kode sumber website haskell-id. Powered by [hakyll](http://jaspervdj.be/hakyll/index.html)

Termasuk di dalamnya [materi kuliah di UPenn](http://www.seas.upenn.edu/~cis194/spring13/lectures.html).

Mohon bantuan saran/kritik/koreksi baik dari kesalahan tulis maupun terjemahan yang lebih baik.


## Menjalankan secara Lokal

**Menggunakan Cabal Sandbox**

- GHC (Glasgow Haskell Compiler) 7.10
- cabal minimal versi 1.18 (fitur sandbox)

```
git clone git@github.com:haskell-id/website.git
cd website/
cabal sandbox init
cabal install
./.cabal-sandbox/bin/site watch
```

**Menggunakan Nix/NixOS**

```
nix-shell
cabal build
dist/build/site/site watch
```

