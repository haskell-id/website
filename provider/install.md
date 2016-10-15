Sebelum melakukan instalasi bahasa pemrograman Haskell pada komputer Anda, maka Anda dapat terlebih dahulu mencoba bahasa pemrograman Haskell melalui situs resminya [disini](http://haskell.org) pada bagian "Try It!".

Apaila Anda pada akhirnya ingin memasang Haskell pada komputer Anda, berikut ini adalah beberapa cara yang dapat dilakukan. 

- [Instalasi Minimal](#minimal) : Hanya berupa GHC (compiler) dan Cabal (sebuah alat pemasangan dan pembuatan paket) yang terpasang secara global pada sistem Anda, menggunakan manajemen paket sistem anda.

- [Stack](#stack) : Memasang perintah "stack" secara global: sebuah alat pembuat program-terpusat untuk mengunduh dan mengelola secara otomatis dependensi Haskell berbasis proyek per proyek.

- [Haskell Platform](#platform) : Memasang GHC, Cabal dan beberapa alat lain, berikut dengan kumpulan pustaka pengantar pada lokasi global sistem Anda.

Adanya perbedaan opsi tersebut dikarenakan apa yang secara global terinstall pada sistem Anda begitu pula karena apa yang dikelola dalam lingkungan proyek yang spesifik. 

Bagi pemula, metode yang direkomendasikan untuk instalasi Haskell dan perangkatnya ialah dengan menggunakan *Stack*.

Catatan:

* GHC: [Glasgow Haskell Compiler](https://www.haskell.org/ghc/)
* cabal-install: *command-line tool* untuk mengurus manajemen paket

## Instalasi Minimal <a name="minimal"></a>

Instalasi minimal hanya memberikan GHC, Cabal serta Stack saja. Hanya pustaka inti yang dibutuhkan oleh masing-masing platform saja yang diikutsertakan.

Cabal atau stack harus digunakan untuk mengunduh dan memasang paket setelah proses instalasi selesai.

Untuk mendapatkan instalasi minimal, silahkan merujuk ke tautan berikut (berbahasa Inggris):

- [Linux](http://haskell.org/downloads/linux)
- [OS X](https://ghcformacosx.github.io/) (via GHC for Mac OS X)
- [Windows](https://github.com/fpco/minghc#using-the-installer) (via MinGHC)

## Stack <a name="stack"></a>

Saat ini, [Stack](http://haskellstack.org/) merupakan metode instalasi Haskell yang populer dan mudah.
Stack akan mengunduh *compiler*, membuat kerangka proyek, mengisolasi proyek, dan mengumpulkan
Haskell *package* antar proyek di satu tempat, sehingga tidak perlu *compile* hal yang
sama berulang kali.

Dengan keuntungan yang sedemikian besar, langkah ekstra yang diperlukan untuk menginstal
Stack menjadi tidak berarti.

Stack akan mengelola seluruh *tools* yang diperlukan dengan bertindak sebagai *front end* dari
cabal-install.

Stack tersedia untuk berbagai OS dan linux distro. Jika Anda pengguna linux *bleeding edge*, Anda
tetap bisa mendapatkan *binary* Stack untuk Linux secara umum.

Untuk instalasi, silahkan merujuk pada tautan berikut (berbahasa Inggris):

- [Ubuntu Linux](http://docs.haskellstack.org/en/stable/install_and_upgrade/#ubuntu)
- [OS X](http://docs.haskellstack.org/en/stable/install_and_upgrade/#os-x)
- [Windows](http://docs.haskellstack.org/en/stable/install_and_upgrade/#windows)

## Haskell Platform <a name="platform"></a>

Haskell platform adalah installer all-in-one yang serba lengkap. Setelah mengunduh, Anda akan memiliki segala sesuatu yang dibutuhkan untuk membangun programs haskell terhadap pustaka yang himpunan inti pada pustaka. Platform ini hadir dalam versi minimal dengan alat kecuali pustaka diluar inti GHC.

Untuk instalasi, silahkan merujuk pada tautan berikut (berbahasa Inggris):

- [Linux](http://www.haskell.org/platform/linux.html)
- [OS X](http://www.haskell.org/platform/mac.html)
- [Windows](http://www.haskell.org/platform/windows.html)

## Contoh Proyek

Sebagai contoh, berikut ini adalah instruksi untuk menjalankan situs ini pada mesin lokal Anda dengan menggunakan Stack sebagai alat yang direkomendasikan:

- [install stack](http://docs.haskellstack.org/en/stable/README.html#how-to-install)
- clone repo situs ini (https://github.com/haskell-id/website/)
- masuk ke dalam folder proyek
- `stack setup`. Ini akan mendownload GHC yang sesuai untuk proyek ini.
- `stack build`. Kompilasi proyek.

Setelah kompilasi, binary dapat ditemukan pada path yang bergantung pada tiap-tiap sistem.
Path binary penulis pada saat artikel ini ditulis ialah: `/.stack-work/install/x86_64-linux/lts-3.11/7.10.2/bin/site`.
Stack menyediakan perintah `stack exec` untuk mengeksekusi binary.

Untuk situs ini, kita bisa melakukan `stack exec site watch` untuk menjalankan webserver lokal di mana kita
bisa melihat hasil situs di `localhost:8000`. *Long running process* ini juga akan mendeteksi jika
ada perubahan konten, sehingga kita hanya perlu merefresh browser untuk melihatnya.

Situs ini menggunakan [hakyll](https://jaspervdj.be/hakyll/), jika ingin tahu lebih banyak,
kalian bisa melihat situs dan dokumentasinya.


