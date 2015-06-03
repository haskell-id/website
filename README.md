# Website Haskell-id

Kode sumber website haskell-id. Powered by [hakyll](http://jaspervdj.be/hakyll/index.html)

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

## Penerjemahan

Berikut status penerjemahan materi [kuliah di UPenn](http://www.seas.upenn.edu/~cis194/spring13/lectures.html)

Status:

```
chapter 1: Done
chapter 2: Done
chapter 3: Done
chapter 4: Done
chapter 5: Done
chapter 6: Done
chapter 7: Done
chapter 8: Done
chapter 9: Done
chapter 10: Done
chapter 11: Done
chapter 12: Ongoing
```

Meski beberapa bab sudah selesai, tidak menutup kemungkinan untuk dikoreksi baik dari kesalahan tulis maupun terjemahan yang lebih baik.

Jika tertarik membantu, silakan update status di atas. Koreksi dan saran bisa melalui *issues* atau *mention* [@haskellID](https://twitter.com/haskellID).
