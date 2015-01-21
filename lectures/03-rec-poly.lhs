<!--
{-# OPTIONS_GHC -Wall #-}
-->

Pola rekursi, polimorfisme, dan *Prelude*
=========================================

Setelah menyelesaikan Tugas 2, kalian pasti banyak menghabiskan
waktu menulis fungsi rekursif. Kalian mungkin mengira programer
Haskell banyak menghabiskan waktu untuk menulis fungsi rekursif.
Sebaliknya, programer Haskell yang berpengalaman sangatlah jarang
menulis fungsi rekursif!

Bagaimana mungkin? Kuncinya adalah menyadari meskipun fungsi rekursif
bisa melakukan apapun, pada prakteknya ada beberapa pola umum yang
seringkali muncul. Dengan mengabstrak keluar (*abstracting out*)
pola-pola tersebut ke fungsi-fungsi *library*, programer bisa
meninggalkan detil *low level* rekursif ke fungsi-fungsi tersebut,
dan fokus ke masalah yang *higher level*. Itulah tujuan dari
*wholemeal programming*.

Pola rekursi
------------

Ingat definisi list yang berisi nilai `Int` sebelumnya:

> data IntList = Empty | Cons Int IntList
>   deriving Show

Apa yang kita bisa lakukan terhadap `IntList`? Berikut ini adalah beberapa
kemungkinan:

  * Lakukan operasi pada tiap elemen di list

  * Buang sebagian elemen, dan simpan sisanya, berdasarkan sebuah tes

  * "Simpulkan" seluruh elemen di list (seperti: cari jumlah total (*sum*),
    *product*, maximum, dan lain-lain).

  * Kalian mungkin masih bisa memberikan contoh lain!

***Map***

Mari kita bahas yang pertama: lakukan operasi pada tiap elemen di list.
Sebagai contoh, kita tambahkan satu ke tiap elemen di list.
        

> addOneToAll :: IntList -> IntList
> addOneToAll Empty       = Empty
> addOneToAll (Cons x xs) = Cons (x+1) (addOneToAll xs)

Atau kita bisa memastkan semua elemen di list tidak negatif dengan
mengambil nilai absolut:


> absAll :: IntList -> IntList
> absAll Empty       = Empty
> absAll (Cons x xs) = Cons (abs x) (absAll xs)

Atau kita bisa mengkuadratkan tiap elemen:

> squareAll :: IntList -> IntList
> squareAll Empty       = Empty
> squareAll (Cons x xs) = Cons (x*x) (squareAll xs)

Sampai di sini, kalian seharusnya sudah menyadari. Ketiga fungsi tersebut
terlihat sangat serupa. Pasti ada cara untuk mengabstraksi keluar kesamaannya
sehingga kita tidak perlu mengulang-ulang.

Ternyata ada caranya. Bisakah kalian menemukannya? Bagian mana yang
serupa di ketiga hal tersebut dan mana yang berubah?

Yang berubah tentunya adalah operasi yang ingin kita lakukan pada tiap elemen
di list. Kita bisa menyebutnya sebagai fungsi bertipe `Int -> Int`. Di sini
kita bisa melihat betapa berguna untuk bisa memberikan fungsi sebagai
input ke fungsi lainnya.

> mapIntList :: (Int -> Int) -> IntList -> IntList
> mapIntList _ Empty       = Empty
> mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs)

Sekarang kita bisa menggunakan `mapIntList` untuk membuat `addOneToAll`,
`absAll`, dan `squareAll`:

> exampleList = Cons (-1) (Cons 2 (Cons (-6) Empty))
>
> addOne x = x + 1
> square x = x * x

    mapIntList addOne exampleList
    mapIntList abs    exampleList
    mapIntList square exampleList

**Filter** (saring)

Satu pola yang sering muncul ialah ketika kita ingin menyimpan sebagian
elemen dari list dan membuang sisanya dengan melakukan sebuah tes. Sebagai
contoh, kita ingin menyimpan hanya bilangan positif saja:

> keepOnlyPositive :: IntList -> IntList
> keepOnlyPositive Empty = Empty
> keepOnlyPositive (Cons x xs) 
>   | x > 0     = Cons x (keepOnlyPositive xs)
>   | otherwise = keepOnlyPositive xs

Atau hanya bilangan genap:

> keepOnlyEven :: IntList -> IntList
> keepOnlyEven Empty = Empty
> keepOnlyEven (Cons x xs)
>   | even x    = Cons x (keepOnlyEven xs)
>   | otherwise = keepOnlyEven xs

Bagaimana bisa mengeneralisir pola ini? Mana yang serupa dan mana yang bisa
diabstrak keluar?

Yang bisa diabstrak keluar adalah tes (atau *predicate*) yang digunakan
untuk menentukan apakah nilai tersebut akan disimpan. Sebuah *predicate*
adalah sebuah fungsi bertipe `Int -> Bool` yang mengembalikan `True` untuk
tiap elemen yang akan disimpan, dan `False` untuk yang akan dibuang. Jadi, kita bisa
menulis `filterIntList` seperti berikut:

> filterIntList :: (Int -> Bool) -> IntList -> IntList
> filterIntList _ Empty = Empty
> filterIntList p (Cons x xs)
>   | p x       = Cons x (filterIntList p xs)
>   | otherwise = filterIntList p xs

-->

***Fold*** (lipat)

Pola terakhir yang tadi kita singgung adalah "menyimpulkan" atau "merangkum"
semua elemendi list. Ini biasa disebut operasi *fold* atau *reduce*. Kita
akan kembali ke sini minggu depan. Sementara itu, kalian mungkin bisa
berpikir bagaimana untuk mengabstrak keluar pola ini.

(*Polymorphism*) Polimorfisme
-----------------------------

Kita telah menulis beberapa fungsi umum untuk *mapping* dan *filtering*
(menyaring) list yang berisi `Int`. Tapi kita belum selesai menggeneralisir!
Bagaimana jika kita ingin menyaring list berisi `Integer`? Atau `Bool`?
Atau list yang berisi list yang berisi *tree* yang berisi tumpukan `String`?
Kita jadi harus membuat tipe data baru dan fungsi baru untuk tiap kasus.
Lebih buruk lagi, kodenya sama persis! Yang berbeda hanyalah notasi tipenya.
Bisakah Haskell membantu kita di sini?

Tentu bisa! Haskell mendukung polimorfisme untuk tipe data dan fungsi.
Kata polimorfis berasal dari kata Yunani (πολύμορφος) yang berarti
"memiliki banyak bentuk". Sesuatu yang polimorfis bisa bekerja untuk
beberapa tipe.

**Tipe data polimorfis**

Pertama, kita lihat bagaimana mendeklarasikan tipe data polimorfis.

> data List t = E | C t (List t)

(Kita tidak bisa menggunakan `Empty` dan `Cons` karena kita telah memakainya
sebagai konstruktor untuk `IntList`. Sebagai gantinya kita menggunakan
`E` dan `C`.)

Sebelumnya kita memiliki `data IntList = ...`, sekarang `data List t = ...`.
`t` merupakan variabel tipe (*type variable*) yang bisa berarti tipe apapun.
Variabel tipe harus dimulai dengan huruf kecil, sedangkan tipe dengan huruf besar.
`data List t = ...` berarti tipe `List` terparameter (*parameterized*)
berdasarkan tipe, serupa dengan sebuah fungsi yang terparameter berdasarkan
input.

Jika ada tipe `t`, maka sebuah `(List t)` terdiri atas konstruktor `E`, atau
konstruktor `C` bersama nilai bertipe `t` dan `(List t)` lainnya. Berikut
beberapa contoh:

> lst1 :: List Int
> lst1 = C 3 (C 5 (C 2 E))
>
> lst2 :: List Char
> lst2 = C 'x' (C 'y' (C 'z' E))
>
> lst3 :: List Bool
> lst3 = C True (C False E)

**Fungsi polimorfis**

Sekarang mari menggeneralisir `filerIntList` supaya bisa bekerja terhadap
`List` yang baru kita buat. Kita bisa mengambil kode `filterIntList`
yang sudah ada dan mengganti `Empty` dengan `E` dan `Cons` dengan `C`:

> filterList _ E = E
> filterList p (C x xs)
>   | p x       = C x (filterList p xs)
>   | otherwise = filterList p xs

Sekarang, bertipe apakah `filterList`? Kita lihat tipe apakah yang
`ghci` *infer* untuknya:

    *Main> :t filterList
    filterList :: (t -> Bool) -> List t -> List t    

Kita bisa membacanya sebagai: "untuk apapun tipe `t`, `filterList`
menerima fungsi dari `t` ke `Bool` dan list `t`, dan mengembalikan
list `t`".

Bagaimana dengan menggeneralisir `mapIntList`? Apa tipe yang harus kita
berikan ke `mapList` sehingga bisa mengaplikasikan sebuah fungsi ke tiap
elemen di `List t`?

Mungkin kita berpikir untuk memberikan tipe

~~~~ {.haskell}
mapList :: (t -> t) -> List t -> List t
~~~~

Ini bisa, tapi ketika kita mengaplikasikan `mapList` kita akan selalu
mendapatkan list yang elemennya bertipe sama dengan elemen di list yang
kita berikan. Hal ini cukup kaku, karena mungkin saja kita ingin melakukan
`mapList show` untuk mengubah list `Int` menjadi list `String`. Berikut
adalah tipe yang paling umum untuk `mapList`, berikut implementasinya:

> mapList :: (a -> b) -> List a -> List b
> mapList _ E        = E
> mapList f (C x xs) = C (f x) (mapList f xs)

Satu hal penting yang perlu diingat mengenai fungsi polimorfis ialah
**pemanggil fungsi yang menentukan tipe**. Ketika kalian menulis sebuah
fungsi polimorfik, fungsi tersebut harus bisa bekerja ke semua tipe.
Hal ini --ditambah dengan Haskell yang tidak bisa memutuskan langsung
berdasarkan tipe-- memiliki implikasi menarik yang akan kita pelajari
lebih jauh nanti.

*Prelude*
---------

        
`Prelude` adalah sebuah modul berisi definisi fungsi-fungsi standar yang terimpor
secara implisit ke tiap program Haskell. [Melihat-lihat dokumentasinya]
(http://haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html)
sangatlah dianjurkan untuk mengenal *tools* yang tersedia di sana.

Tentu saja list polimorfis terdefinisi di `Prelude`, beserta 
[fungsi-fungsi polimorfis yang berguna untuk bekerja dengan list tersebut]
(http://haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html#11).
Sebagai contoh, `filter` dan `map` adalah padanan dari `filterList` dan `mapList`
yang tadi kita bahas. Bahkan, [masih banyak fungsi-fungsi untuk list
di modul `Data.List`](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-List.html).  

Tipe polimorfis lain yang cukup berguna untuk diketahui ialah `Maybe`,
didefinisikan sebagai

~~~~ {.haskell}
data Maybe a = Nothing | Just a
~~~~

Sebuah nilai bertipe `Maybe a` bisa mengandung nilai bertipe `a` (terbungkus
di dalam konstruktor `Just`), atau berupa `Nothing` (mewakili kegagalan atau kesalahan).
[Modul `Data.Maybe` memiliki fungsi-fungsi yang bekerja terhadap nilai bertipe
`Maybe` values](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-Maybe.html).

        
Fungsi total dan parsial
------------------------

        
Perhatikan tipe polimorfis berikut:

~~~~ {.haskell}
[a] -> a
~~~~

Fungsi seperti apa yang bertipe demikian? Tipe di atas menyatakan bahwa
jika diberi list apapun yang bertipe `a`, fungsi tersebut harus sebuah
nilai bertipe `a`. Sebagai contoh, fungsi `head` di *Prelude* bertipe
seperti ini.

Apakah yang terjadi jika `head` diberikan list kosong sebagai input?
Mari kita lihat [kode sumber]
(http://www.haskell.org/ghc/docs/latest/html/libraries/base/src/GHC-List.html#head)
dari `head`...

Program akan *crash*!  Tak ada lagi yang bisa dilakukan karena program harus
bisa bekerja untuk *semua* tipe. Tak mungkin untuk mengetahui tipe dari
elemen yang tidak ada.

`head` dikenal sebagai *fungsi parsial*: ada input yang bisa membuat
`head` *crash*. Fungsi-fungsi yang rekursif tanpa henti pada beberapa
input juga disebut parsial. Fungsi yang terdefinisi lengkap pada semua
kemungkinan input dikenal dengan nama *fungsi total*.

Menghindari fungsi-fungsi parsial sebisa mungkin adalah praktek Haskell
yang bagus.Bahkan, menghindari fungsi parsial bisa dibilang praktek yang
bagus di bahasa pemrograman apapun -- meski sangat sulit di beberapa bahasa.
Haskell membuat hal ini mudah dan masuk akal.

**`head` adalah sebuah kesalahan!** Seharusnya itu tidak berada di `Prelude`.
Fungsi-fungsi parsial lainnya di `Prelude` yang sebaiknya jangan kalian
gunakan antara lain `tail`, `init`, `last`, dan `(!!)`. Sejak sekarang
penggunaan salah satu dari fungsi-fungsi tersebut di tugas kuliah
akan mengurangi nilai!

Jadi mesti bagaimana?

**Mengganti fungsi-fungsi parsial**

Kebanyakan fungsi seperti `head, `tail`, dan lain-lain bisa digantikan
dengan pencocokan pola (*pattern-matching*). Perhatikan definisi berikut:

> doStuff1 :: [Int] -> Int
> doStuff1 []  = 0
> doStuff1 [_] = 0
> doStuff1 xs  = head xs + (head (tail xs)) 

> doStuff2 :: [Int] -> Int
> doStuff2 []        = 0
> doStuff2 [_]       = 0
> doStuff2 (x1:x2:_) = x1 + x2

Fungsi-fungsi di atas menghasilkan hasil yang sama, dan keduanya total.
Akan tetapi, hanya fungsi kedua yang *jelas* terlihat total, dan
lebih mudah dibaca.

**Menulis fungsi parsial**

Bagaimana jika kalian suatu saat tersadar sedang menulis fungsi parsial?
Ada dua pilihan. Yang pertama, ubah tipe hasil fungsi ke tipe yang bisa
mengindikasikan kegagalan. Ingat kembali definisi dari `Maybe`:

~~~~ {.haskell}
data Maybe a = Nothing | Just a
~~~~

Sekarang, andaikan kita sedang membuat `head`. Kita bisa menulisnya
dengan aman seperti ini:

> safeHead :: [a] -> Maybe a
> safeHead []    = Nothing
> safeHead (x:_) = Just x

Sebenarnya, fungsi tersebut sudah terdefinisi di paket [`safe`]
(http://hackage.haskell.org/package/safe).

Mengapa ini ide yang bagus?

1. `safeHead` tak akan pernah *crash*.  
2. Tipe `safeHead` memperjelas bahwa fungsi tersebut bisa gagal pada
   beberapa input.
3. Sistem tipe (*type system*) memastikan pengguna `safeHead` harus
   selalu mengecek tipe hasilnya untuk melihat apakah nilai yang didapat
   atau `Nothing`.

Sebenarnya, `safeHead` masih "parsial"; tapi kita telah mencatat keparsialannya
ke sistem tipe sehingga aman. Tujuannya ialah membuat tipe yang ada
bisa menjelaskan sifat fungsi sebaik mungkin.

Oke, tapi bagaimana jika kita tahu kita akan memakai `head` di mana
kita *terjamin* untuk memiliki list yang tidak kosong? Dalam situasi
tersebut, akan sangat mengganggu untuk mendapatkan hasil berupa `Maybe a`
karena kita harus bekerja lebih untuk mengatasi kasus yang kita "tahu"
tak akan mungkin terjadi.

Jawabannya, jika ada kondisi yang *terjamin*, maka tipe harus merefleksikan
jaminan tersebut! Lalu *compiler* bisa menjamin hal tersebut untuk kalian.
Sebagai contoh:

> data NonEmptyList a = NEL a [a]
>
> nelToList :: NonEmptyList a -> [a]
> nelToList (NEL x xs) = x:xs
>
> listToNel :: [a] -> Maybe (NonEmptyList a)
> listToNel []     = Nothing
> listToNel (x:xs) = Just $ NEL x xs
>
> headNEL :: NonEmptyList a -> a
> headNEL (NEL a _) = a
>
> tailNEL :: NonEmptyList a -> [a]
> tailNEL (NEL _ as) = as

Kalian mungkin berpikir melakukan hal seperti ini hanyalah untuk orang
bodoh yang bukan jenius seperti kalian. Tentu, *kalian* tak mungkin
membuat kesalahan seperti memberikan list kosong ke fungsi yang
mengharapkan list yang tidak kosong. Ya kan? Ada orang bodoh yang
terlibat, tapi bukan yang kalian kira.
