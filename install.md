Berikut adalah berbagai macam cara untuk menginstall Haskell di mesin Anda. Jika heran
dengan banyaknya cara, ini timbul akibat ketidak-puasan pengguna dengan manajemen
paket di Haskell.

Metode yang direkomendasikan untuk menginstal Haskell *tools* ialah
dengan menggunakan Stack. 

Catatan:

* GHC: Glasgow Haskell Compiler
* cabal-install: *command-line tool* untuk mengurus manajemen paket

## Stack

Saat ini, [Stack](http://haskellstack.org/) merupakan metode instalasi Haskell yang populer dan mudah.
Stack akan mengurusi *compiler* untuk kita, isolasi proyek, dan membuat kita berbagi
Haskell *package* antar proyek, sehingga tidak perlu *compile* hal yang sama berulang kali.

Dengan keuntungan yang sedemikian besar, sedikit langkah ekstra yang diperlukan untuk menginstal
Stack jadi tidak terasa berat.

Stack akan mengurusi seluruh *tools* yang diperlukan dengan bertindak sebagai *front end* dari
cabal-install.

Stack tersedia dalam berbagai OS dan linux distro. Jika Anda pengguna linux *bleeding edge*, Anda
tetap bisa mendapatkan *binary* Stack untuk Linux secara umum.


## Instal GHC dan Cabal-Install terpisah

Sebelum Stack populer, sebagian orang menginstall GHC dan cabal-install secara terpisah. Ada
yang melalui cara yang spesifik dengan OS-nya ([PPA](https://launchpad.net/~hvr/+archive/ubuntu/ghc),
AUR, MinGHC, dan lain-lain) atau ada juga yang melakukan
[kompilasi sendiri](http://www.davesquared.net/2014/05/platformless-haskell.html).

Biasanya `cabal sandbox` digunakan untuk mengisolasi proyek. Isolasi ini mencegah terjadinya
dependensi yang bentrok karena berbeda versi antar proyek tetapi tidak mengizinkan untuk
berbagi dependensi yang memiliki kesamaan versi. Hal ini menyebabkan dependensi-dependensi yang
berversi sama antar proyek akan dikompilasi berulang-ulang.


## Haskell Platform

Haskell Platform membundle GHC, cabal-install, beserta beberapa paket lain. Cara ini semakin
ditinggalkan saat ini karena meski terlihat mudah, tapi akan membawa masalah di kemudian hari.

Platform merangkum paket-paket yang cukup "tertinggal". Hal ini akan menyulitkan jika
proyek kita membutuhkan paket-paket terbaru. Isolasi juga suboptimal karena banyaknya
paket-paket umum yang diinstal secara global.
