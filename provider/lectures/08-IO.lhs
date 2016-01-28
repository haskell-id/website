
 <!-- CLASS

> {-# OPTIONS_GHC -Wall #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}

-->

*IO*
====

Bacaan tambahan:

  * [*LYAH Chapter 9: Input and Output*](http://learnyouahaskell.com/input-and-output)
  * [*RWH Chapter 7: I/O*](http://book.realworldhaskell.org/read/io.html)

Masalah dengan *purity*
-----------------------

Ingat bahwa Haskell bersifat *lazy* dan juga *pure*. Ini mengakibatkan
dua hal:

1. Fungsi tidak boleh memiliki efek eksternal. Sebagai contoh, sebuah
   fungsi tidak bisa mencetak ke layar. Fungsi hanya bisa menghitung
   hasil.

2. Fungsi tidak boleh bergantung pada hal di luar dirinya. Misalnya, tidak
   boleh membaca dari *keyboard*, sistem berkas, atau jaringan. Fungsi
   hanya boleh bergantung pada inputnya. Dengan kata lain, fungsi harus
   memberikan hasil yang sama untuk input yang sama setiap saat.

Tapi terkadang kita perlu melakukan hal-hal tersebut di atas! Jika di Haskell
kita hanya bisa menulis fungsi untuk dievaluasi di ghci, maka tentu tak akan
berguna banyak.

Di Haskell kita sebenarnya bisa melakukan semua itu, tapi terlihat sangat
berbeda jika dibandingkan dengan bahasa pemrograman lain.


Tipe `IO`
---------

Solusi dari masalah di atas ialah tipe khusus bernama `IO`. Nilai bertipe
`IO a` adalah *deskripsi* dari komputasi yang memiliki efek. Dengan kata lain,
jika dijalankan (mungkin) akan melakukan operasi I/O dan (pada akhirnya)
menghasilkan nilai bertipe `a`. Ada tingkat indireksi di sini yang harus
dimengerti. Nilai bertipe `IO a` *dengan sendirinya* adalah sesuatu yang
*tidak aktif* tanpa efek samping. Itu hanyalah *deskripsi* dari sebuah
komputasi dengan efek samping. Bisa dibayangkan tipe `IO a` merupakan sebuah
program imperatif *first-class* di dalam Haskell.

Sebagai ilustrasi, misalkan kalian memiliki

    c :: Cake

Apa yang kalian miliki?  Tentu *cake* yang enak. Cukup sederhana.

Sebaliknya, jika kalian memiliki

    r :: Recipe Cake

Apa yang kalian punya?  *Cake*?  Bukan. Kalian hanya memiliki *instruksi*
bagaimana membuat *cake* (resep), hanya selembar kertas dengan tulisan di atasnya.

Memiliki resep tak menghasilkan efek apapun. Hanya dengan memegang resep tidak
akan menyebabkan *oven* menjadi panas, tepung berserak di lantai, dan lain
sebagainya. Untuk menghasilkan *cake*, resep tersebut harus diikuti (yang
akan menyebabkan tepung berserakan, bahan-bahan tercampur, oven menjadi
panas, dsb).

Sama seperti di atas, nilai bertipe `IO a` hanyalah sebuah "resep" untuk
mendapatkan nilai bertipe `a` (dan memiliki efek samping). Seperti nilai
lainnya, dia bisa diberikan sebagai argumen, dikembalikan sebagai hasil
dari fungsi, disimpan di struktur data, atau (yang akan segera kita lihat)
digabungkan dengan nilai `IO` lain menjadi resep yang lebih kompleks.


Jadi bagaimana nilai bertipe `IO a` bisa dijalankan? Hanya satu cara:
*compiler* Haskell mencari nilai spesial

    main :: IO ()

yang akan diberikan ke sistem *runtime* dan dijalankan. Bayangkan sistem
*runtime* di Haskell sebagai *master chef*, satu-satunya orang yang
diizinkan untuk memasak.

Jika kalian mau resep kalian juga disertakan maka kalian harus membuatnya
menjadi bagian dari resep besar (`main`) yang diberikan ke *master chef*.
Tentunya `main` bisa menjadi kompleks, dan biasanya terdiri dari beberapa
komputasi `IO` yang lebih kecil.

Untuk yang pertama kalinya, mari kita buat program Haskell yang *executable*!
Kita bisa menggunakan fungsi

    putStrLn :: String -> IO ()

yang jika diberikan sebuah `String`, akan mengembalikan sebuah komputasi `IO`
yang akan (ketika dijalankan) mencetak `String` tersebut di layar. Kita cukup
menulis ini ke sebuah file bernama `Hello.hs`:

    main = putStrLn "Hello, Haskell!"

Mengetikkan `runhaskell Hello.hs` di *command-line prompt* menghasilkan pesan
kita tercetak di layar!  Kita juga bisa menggunakan `ghc --make Hello.hs`
untuk menghasilkan berkas *executable* bernama `Hello` (atau `Hello.exe`
di Windows).

Tidak ada `String` "di dalam" `IO String`
-----------------------------------------

Banyak pemula di Haskell bertanya-tanya "Saya punya `IO String`, bagaimana
cara mengubahnya menjadi `String`?", atau "Bagaimana cara mengeluarkan
`String` dari `IO String`?". Dari penjelasan sebelumnya, jelas bahwa
pertanyaan-pertanyaan tersebut tidak masuk akal. Tipe `IO String` merupakan
deskripsi komputasi, sebuah resep, untuk menghasilkan `String`. Tidak
ada `String` di dalam `IO String`, seperti halnya tidak ada *cake* di
dalam resep *cake*. Untuk menghasilkan `String` (atau *cake* yang lezat),
kita perlu menjalankan komputasinya (atau resep). Dan satu-satunya cara
untuk melakukannya ialah dengan memberikannya (mungkin sebagai bagian
dari `IO` yang lebih besar) ke sistem *runtime* Haskell melalui `main`.

Menggabungkan `IO`
------------------

Sudah jelas kalau kita perlu sebuah cara untuk menggabungkan komputasi
`IO` menjadi.

Cara paling sederhana untuk menggabungkan dua buah komputasi `IO` ialah
dengan menggunakan operator `(>>)` (dilafalkan "*and then*", terjemahan:
"lalu" atau "kemudian") yang bertipe

~~~~ {.haskell}
(>>) :: IO a -> IO b -> IO b
~~~~

`(>>)` menciptakan sebuah komputasi `IO` yang terdiri dari menjalankan
dua komputasi input secara berurutan. Perhatikan bahwa hasil dari
komputasi pertama diabaikan. Kita hanya peduli terhadap efeknya.
Sebagai contoh:

~~~~ {.haskell}
main = putStrLn "Hello" >> putStrLn "world!"
~~~~

Ini tidak masalah untuk kode berbentuk "do this; do this; do this" di
mana hasilnya diabaikan. Akan tetapi ini saja belum cukup.
Bagaimana kalau kita tidak ingin mengabaikan hasil dari komputasi pertama?

Yang pertama kali terpikirkan mungkin dengan memiliki tipe seperti
`IO a -> IO b -> IO (a,b)` akan memecahkan masalah. Ini pun belum cukup.
Alasannya, kita ingin komputasi kedua bergantung terhadap hasil komputasi
yang pertama. Misalnya kita ingin membaca sebuah integer dari pengguna,
lalu mencetak bilangan tersebut ditambah satu. Dalam kasus ini, komputasi
kedua (mencetak ke layar) akan berbeda dan bergantung pada hasil dari
komputasi pertama.
  
Solusinya, dengan operator `(>>=)` (dilafalkan "*bind*") yang bertipe

~~~~ {.haskell}
(>>=) :: IO a -> (a -> IO b) -> IO b
~~~~

Ini mungkin akan sulit dimengerti pada awalnya. `(>>=)` menerima sebuah
komputasi yang akan menghasilkan nilai bertipe `a`, dan sebuah *fungsi*
yang akan melakukan komputasi kedua berdasarkan nilai bertipe `a` yang
tadi dihasilkan. Hasil dari `(>>=)` adalah (deskripsi dari) sebuah
komputasi yang menjalankan komputasi pertama, gunakan hasilnya untuk
menentukan komputasi selanjutnya, lalu jalankan komputasi tersebut.

Sebagai contoh, kita bisa menulis program yang menerima bilangan dari
pengguna dan mencetak suksesornya (ditambah 1). Perhatikan penggunaan
`readLn :: Read a => IO a` yang merupakan komputasi yang membaca input
dari pengguna, dan mengubahnya jadi tipe apapun selama merupakan
anggota dari `Read`.

~~~~ {.haskell}
main :: IO ()
main = putStrLn "Please enter a number: " >> (readLn >>= (\n -> putStrLn (show (n+1))))
~~~~

Tentu ini terlihat jelek. Nantinya kita akan mempelajari cara menulisnya dengan
lebih baik.


Sintaks *record*
----------------

*Materi ini tidak dibahas di kuliah tapi disediakan ekstra untuk mengerjakan
 tugas 8.*

Misalkan kita memiliki tipe data seperti

~~~~ {.haskell}
data D = C T1 T2 T3
~~~~

Kita juga bisa mendeklarasi tipe data tersebut  dengan sintaks *record*
sebagai berikut:

~~~~ {.haskell}
data D = C { field1 :: T1, field2 :: T2, field3 :: T3 }
~~~~

di mana kita tidak hanya mendeskripsikan tipe tapi juga *nama* untuk
tiap *field* yang terdapat di konstruktor `C`. `D` versi baru ini
bisa digunakan sama seperti versi lamanya. Kita bisa membuat konstruksi
dan mencocokkan pola terhadap nilai bertipe `D` seperti `C v1 v2 v3`.
Selain itu, kita juga mendapatkan keuntungan tambahan.

  1. Tiap nama *field* secara otomatis merupakan fungsi proyeksi
     (*projection function*) yang mendapatkan nilai dari *field*
     tersebut di nilai bertipe `D`. Sebagai contoh, `field2` ialah
     sebuah fungsi bertipe

    ~~~~ {.haskell}
	field2 :: D -> T2
	~~~~
  
     Sebelumnya kita harus membuat implementasi `field2` sendiri
     dengan menuliskan

    ~~~~ {.haskell}
	field2 (C _ f _) = f
	~~~~

     Ini menghilangkan banyak kode tambahan (*boilerplate*) seandainya
     kita memiliki tipe data dengan banyak *field*!

  1. Terdapat sintaks khusus untuk *membuat*, *mengubah*, and
     *mencocokkan pola* untuk nilai bertipe `D` (selain sintaks biasa).

     Kita bisa membuat *membuat* sebuah nilai bertipe `D` menggunakan
     sintaks seperti

    ~~~~ {.haskell}
    C { field3 = ..., field1 = ..., field2 = ... }
    ~~~~

     dengan `...` diisi dengan ekspresi bertipe tepat. Perhatikan bahwa
     kita bisa deskripsikan *field*-nya dengan urutan bebas.

     Jika kita memiliki sebuah nilai `d :: D`. Kita bisa *mengubah* `d`
     menggunakan sintaks seperti

    ~~~~ {.haskell}
    d { field3 = ... }
    ~~~~

     Tentu yang dimaksud "mengubah" di sini bukan *mutating* `d`, tapi
     membuat nilai baru bertipe `D` yang sama persis seperti `d` kecuali
     `field3` yang nilainya tergantikan dengan nilai baru.

     Akhirnya, kita bisa *mencocokkan pola* pada nilai bertipe `D` seperti
     berikut:

    ~~~~ {.haskell}
    foo (C { field1 = x }) = ... x ...
    ~~~~

     Ini hanya mencocokkan *field* `field1` dari nilai bertipe `D`,
     dengan menyebutnya sebagai `x`, mengabaikan *field* lainnya. (Tentu
     `x` di sini hanya contoh, kita bisa mencocokkan dengan pola apapun).

 <!--

Local Variables:
mode:markdown
compile-command:"mk build"
End:

-->
