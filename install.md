Berikut adalah berbagai macam cara untuk menginstall Haskell di mesin Anda. Jika heran
dengan banyaknya cara, ini timbul akibat ketidak-puasan pengguna dengan manajemen
paket di Haskell.

Metode yang direkomendasikan untuk menginstal Haskell *tools* ialah
dengan menggunakan Stack. 

GHC: Glasgow Haskell Compiler
cabal-install: *command-line tool* untuk mengurus manajemen paket

## Stack

Saat ini, [Stack](http://haskellstack.org/) merupakan metode instalasi Haskell yang populer dan mudah.
Stack akan mengurusi *compiler* untuk kita, isolasi proyek, dan membuat kita berbagi
Haskell *package* antar proyek, sehingga tidak perlu *compile* hal yang sama berulang kali.

Dengan keuntungan yang sedemikian besar, sedikit langkah ekstra yang diperlukan untuk menginstal
Stack jadi tidak terasa berat.

Stack akan mengurusi seluruh *tools* yang diperlukan dengan bertindak sebagai *front end* dari
cabal-install.

Stack tersedia dalam berbagai OS dan linux distro. Jika Anda pengguna linux *bleeding edge*, Anda
tetap bisa mendapatkan Stack untuk Linux generic.


## Instal GHC dan Cabal-Install terpisah

Sebelum Stack populer, sebagian orang menginstall GHC dan cabal-install secara terpisah. Ada
yang melalui cara yang spesifik dengan OS-nya ([PPA](https://launchpad.net/~hvr/+archive/ubuntu/ghc),
AUR, MinGHC, dan lain-lain) atau ada juga yang melakukan kompilasi sendiri.

Biasanya `cabal sandbox` digunakan untuk mengisolasi proyek. Isolasi ini mencegah terjadinya
dependensi yang bentrok antar proyek tetapi tidak mengizinkan untuk berbagi dependensi tersebut,
sehingga banyak waktu terbuang untuk kompilasi paket yang sama.


## Haskell Platform

Haskell Platform membundle GHC, cabal-install, beserta beberapa paket lain. Cara ini semakin
ditinggalkan saat ini karena meski terlihat mudah, tapi akan membawa masalah di kemudian hari.
