# Website Haskell-id

Kode sumber website haskell-id. Powered by [hakyll](http://jaspervdj.be/hakyll/index.html)

Termasuk di dalamnya [materi kuliah di UPenn](http://www.seas.upenn.edu/~cis194/spring13/lectures.html).

Mohon bantuan saran/kritik/koreksi baik dari kesalahan tulis maupun terjemahan yang lebih baik.


## Menjalankan secara Lokal

**Menggunakan Stack**
- `git clone git@github.com:haskell-id/website.git`
- `cd website/`
- `stack setup`
- `stack build`
- `stack exec site watch`

Penjelasan lebih lengkap terdapat di [sini](http://haskell.web.id/install.html).


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

## Membuat Meteri Pembelajaran versi PDF

Salah satu cara untuk menghasilkan output pdf adalah dengan menggunakan program
pendukung `pandoc` dan `pdflatex`.


**Install `pandoc` dan `pdflatex` pada Linux Ubuntu**

```
sudo apt-get install pandoc
sudo apt-get install texlive-latex-base
```

**OS-X**

```
brew install pandoc
brew install cask install basictex
```

Kemudian, masuk ke direktori `provider/lectures` dimana tersedia kode sumber
literate haskell untuk buku "Dasar Haskell" dan generate pdf dengan `pandoc`:

```
cd provider/lectures
pandoc -t latex --latex-engine=xelatex -o /tmp/dasar-haskell.pdf *.lhs
```
