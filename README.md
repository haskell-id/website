# Website Haskell-id
![Build](https://github.com/haskell-id/website/actions/workflows/haskell.yml/badge.svg)

Kode sumber website haskell-id. Powered by [hakyll](http://jaspervdj.be/hakyll/index.html)

Termasuk di dalamnya [materi kuliah di UPenn](http://www.seas.upenn.edu/~cis194/spring13/lectures.html).

Mohon bantuan saran/kritik/koreksi baik dari kesalahan tulis maupun terjemahan yang lebih baik.


## Menjalankan secara Lokal
```
# Dites di Cabal 3.10.1 & GHC 9.6.2
# Mungkin membutuhkan aplikasi libnuma-dev di Debian/Ubuntu
git clone git@github.com:haskell-id/website.git
cd website/
cabal build
cabal run site watch
```


## Membuat Materi Pembelajaran versi PDF

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
