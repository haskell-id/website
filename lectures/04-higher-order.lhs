<!--
{-# OPTIONS_GHC -Wall #-}
-->

*Higher-order programming* dan *type inference*
===============================================

Bacaan tambahan:

  * "*Learn You a Haskell for Great Good*", "*Higher-Order Functions*" (Bab 5 di buku; [Bab 6 online](http://learnyouahaskell.com/higher-order-functions))

Fungsi anonim
-------------

Andai kita ingin menulis sebuah fungsi

~~~~ {.haskell}
lebihDari100 :: [Integer] -> [Integer]
~~~~

yang menyaring `Integer` yang lebih besar dari 100. Sebagai contoh, 

~~~~ {.haskell}
lebihDari100 [1,9,349,6,907,98,105] = [349,907,105].
~~~~

Kita bisa membuatnya begini:

> ld100 :: Integer -> Bool
> ld100 x = x > 100
>
> lebihDari100 :: [Integer] -> [Integer]
> lebihDari100 xs = filter ld100 xs

Akan tetapi, cukup merepotkan untuk menamakan `ld100`, karena mungkin kita tak
akan menggunakannya lagi. Untuk itu, kita bisa menggunakan fungsi anonim
(*anonymous function*), yang juga dikenal sebagai abstraksi lambda
(*lambda abstraction*):

> lebihDari100_2 :: [Integer] -> [Integer]
> lebihDari100_2 xs = filter (\x -> x > 100) xs

`\x -> x > 100` (*backslash* dianggap menyerupai lambda tanpa kaki
yang pendek) adalah sebuah fungsi yang menerima sebuah argument `x`
dan mengembalikan apakah `x` lebih besar dari 100.

Abstraksi lambda juga bisa memiliki beberapa argumen. Contohnya:

    Prelude> (\x y z -> [x,2*y,3*z]) 5 6 3
    [5,12,9]

Akan tetapi, untuk `lebihDari100`, ada cara lebih baik untuk menulisnya.
Yaitu tanpa abstraksi lambda:

> lebihDari100_3 :: [Integer] -> [Integer]
> lebihDari100_3 xs = filter (>100) xs

`(>100)` adalah sebuah *operator section*. Jika `?` adalah sebuah operator,
maka `(?y)` sama saja dengan fungsi `\x -> x ? y`, dan `(y?)`
sama dengan `\x -> y ? x`. Dengan kata lain, *operator section* memungkinkan
kita menerapkan sebagian (*partially apply*) operator ke salah satu di
antara dua argumen. Yang kita dapat ialah suatu fungsi dengan satu argumen.
Contohnya seperti ini:

    Prelude> (>100) 102
    True
    Prelude> (100>) 102
    False
    Prelude> map (*6) [1..5]
    [6,12,18,24,30]

Komposisi fungsi
----------------

Sebelum membaca lebih lanjut,
bisakah kalian menulis suatu fungsi bertipe ini:

~~~~ {.haskell}
(b -> c) -> (a -> b) -> (a -> c)
~~~~

?

Mari kita coba. Fungsi tersebut harus menerima dua argumen
dan mengembalikan suatu fungsi. Masing-masing argumen juga berupa suatu fungsi.

~~~~ {.haskell}
foo f g = ...
~~~~

`...` harus kita ganti dengan suatu fungsi bertipe `a -> c`.
Kita bisa membuatnya dengan menggunakan abstraksi lambda:

~~~~ {.haskell}
foo f g = \x -> ...
~~~~

`x` bertipe `a`, dan sekarang dalam `...` kita akan menulis
ekspresi bertipe `c`. Kita punya fungsi `g` yang mengubah
sebuah `a` menjadi sebuah `b`, dan fungsi `f` yang mengubah 
sebuah `b` menjadi sebuah `c`, jadi ini seharusnya benar:

> foo :: (b -> c) -> (a -> b) -> (a -> c)
> foo f g = \x -> f (g x)

(Kuis: mengapa tanda kurung diperlukan di `g x`?)

OK, jadi apa gunanya? Apakah `foo` berguna dalam suatu hal atau
hanya sekedar latihan iseng dengan *types*?

Ternyata, `foo` disebut `(.)` yang berarti komposisi fungsi
(*function composition*), yaitu, jika `f` dan `g` adalah fungsi, maka
`f . g` adalah fungsi yang melakukan `g` lalu `f`.

Komposisi fungsi berguna untuk menulis kode yang ringkas dan elegan.
Ini sangat cocok dengan gaya "*wholemeal*" di mana kita berpikir untuk
menyusun transformasi *high level* yang berurutan untuk struktur data.

Sebagai contoh:

> myTest :: [Integer] -> Bool
> myTest xs = even (length (lebihDari100 xs))

Kita bisa menggantinya dengan:

> myTest' :: [Integer] -> Bool
> myTest' = even . length . lebihDari100

Versi ini lebih jelas: `myTest` hanyalah "pipa" yang terdiri dari
tiga fungsi yang lebih kecil. Ini juga menjelaskan mengapa komposisi
fungsi terlihat "terbalik": karena aplikasi fungsi juga terbalik!
Karena kita membaca dari kiri ke kanan, lebih masuk akal untuk
membayangkan nilai juga mengalir dari kiri ke kanan. Tapi dengan
begitu kita seharusnya menulis \\( (x)f \\) untuk menggambarkan
pemberian nilai \\(x\\) sebagai input ke fungsi \\(f\\).
Karena Alexis Claude Clairaut dan Euler, kita terjebak dengan notasi
terbalik sejak 1734.

Mari lihat lebih dekat tipe dari `(.)`.  Tanyakan tipenya ke `ghci`,
kita akan mendapatkan:

    Prelude> :t (.)
    (.) :: (b -> c) -> (a -> b) -> a -> c

Tunggu. Apa yang terjadi? Ke mana tanda kurung di sekitar `(a -> c)`?

*Currying* dan aplikasi parsial
-------------------------------

Ingat kalau tipe dari fungsi yang memiliki beberapa argumen
terlihat aneh dengan ekstra anak panah? Seperti:

> f :: Int -> Int -> Int
> f x y = 2*x + y

Sebelumnya dikatakan ada alasan yang indah dan mendalam tentangnya,
dan sekarang saatnya untuk diketahui: *semua fungsi di Haskell hanya
menerima satu argumen*.  Loh?! Bukankah fungsi `f` di atas menerima
dua argumen? Sebenarnya tidak: fungsi `f` menerima satu argumen
(sebuah `Int`) dan *mengembalikan sebuah fungsi* (bertipe `Int -> Int`)
yang menerima satu argumen dan mengembalikan hasil akhir. Jadi kita
menyatakan tipe dari `f`'s seperti ini:

> f' :: Int -> (Int -> Int)
> f' x y = 2*x + y

Perhatikan bahwa anak panah fungsi *asosiatif ke kanan*, dengan kata
lain, `W -> X -> Y -> Z` sama dengan  `W -> (X -> (Y -> Z))`.
Kita bisa menambahkan atau mengurangi tanda kurung di sekitar anak panah
*top level* paling kanan di notasi tipe.

Aplikasi fungsi, kebalikannya, bersifat asosiatif ke *kiri*. `f 3 2`
sama dengan `(f 3) 2`. Masuk akal mengingat tentang `f` sebenarnya
menerima satu argumen dan mengembalikan sebuah fungsi: kita terapkan `f`
ke sebuah argumen `3`, yang mengembalikan fungsi bertipe `Int -> Int`,
yaitu sebuah fungsi yang menerima sebuah `Int` dan menambahkan 6
ke argumen tersebut. Lalu kita terapkan fungsi tersebut ke argumen `2`
dengan menuliskan `(f 3) 2`, yang menghasilkan sebuah `Int`. Karena
aplikasi fungsi asosiatif ke kiri, kita bisa menulis `(f 3) 2` sebagai
`f 3 2`. Dengan begini, kita mendapatkan notasi yang bagus untuk `f`
sebagai fungsi yang memiliki beberapa argumen.

Abstraksi lambda untuk fungsi dengan beberapa argumen

~~~~ {.haskell}
\x y z -> ... 
~~~~

hanyalah versi lain (*syntax sugar*) dari

~~~~ {.haskell}
\x -> (\y -> (\z -> ...)).  
~~~~

Sebaliknya, definisi fungsi

~~~~ {.haskell}
f x y z = ... 
~~~~

hanyalah versi lain dari 

~~~~ {.haskell}
f = \x -> (\y -> (\z -> ...)).
~~~~

Sebagai contoh, kita bisa menulis ulang komposisi fungsi sebelumnya di atas
dengan memindahkan `\x -> ...` dari sisi kanan `=` ke sisi kiri:

> comp :: (b -> c) -> (a -> b) -> a -> c
> comp f g x = f (g x)

Ide untuk merepresentasikan fungsi dengan beberapa arguman sebagai fungsi
satu argumen disebut *currying*, mengambil nama matematikawan dan ahli
logika Inggris Haskell Curry (orang yang juga mengispirasi penamaan Haskell).
Curry (1900-1982) banyak menghabiskan hidupnya di Penn State,
dan juga membantu mengerjakan ENIAC di UPenn. Ide representasi tersebut
sebenarnya ditemukan oleh Moses Schönfinkel, jadi mungkin kita layak
menyebutnya *schönfinkeling*. Curry sendiri mengatributkan ide tersebut
ke Schönfinkel, akan tetapi orang lain sudah terlanjur menyebutnya *currying*.

Jika kita ingin benar-benar menyatakan sebuah fungsi dengan beberapa argumen,
kita bisa memberikan satu argumen berupa *tuple*. Fungsi ini

> f'' :: (Int,Int) -> Int
> f'' (x,y) = 2*x + y

bisa dibayangkan menerima dua argumen, meskip sebenarnya hanya menerima satu
argumen berupa sebuah "pair". Untuk mengubah antar dua representasi fungsi
yang menerima dua argumen, *standard library* menyediakan fungsi `curry` dan
`uncurry` yang didefinisikan sebagai berikut (dengan nama berbeda):

> schönfinkel :: ((a,b) -> c) -> a -> b -> c
> schönfinkel f x y = f (x,y)
>
> unschönfinkel :: (a -> b -> c) -> (a,b) -> c
> unschönfinkel f (x,y) = f x y

`uncurry` berguna jika kita ingin menerapkan sebuah fungsi ke sebuah *pair*.
Sebagai contoh:

    Prelude> uncurry (+) (2,3)
    5

**Aplikasi Parsial**

Fakta bahwa fungsi di Haskell *curried* membuat aplikasi parsial (atau penerapan sebagian,
*partial application*) menjadi mudah. Aplikasi parsial adalah kita mengambil
sebuah fungsi yang menerima beberapa argumen dan menerapkannya ke
*sebagian* dari argumen-argumen tersebut, dan mendapatkan sebuah fungsi yang
menerima argumen-argumen sisanya. Tapi seperti yang kita baru saja saksikan,
di Haskell *tidak ada* fungsi dengan beberapa argumen! Tiap fungsi bisa
diterapkan sebagian ke argumen pertama (dan satu-satunya),
menghasilkan sebuah fungsi yang menerima argumen-argumen sisanya.

Perhatikan bahwa tidak mudah di Haskell untuk melakukan aplikasi parsial
ke argumen selain yang pertama. Pengecualian untuk operator *infix*,
yang kita sudah lihat bisa di aplikasikan sebagian ke salah satu dari dua
argumennya menggunakan sebuah *operator section*. Pada prakteknya ini bukanlah
suatu masalah. Seni menentukan urutan argumen untuk membuat aplikasi parsial
benar-benar berguna: argumen harus diurutkan dari yang *kurang bervariasi*
sampai ke yang *paling bervariasi*. Artinya, argumen yang akan sering sama
harus diletakkan lebih dulu (paling depan), dan argumen yang akan sering
berbeda diletakkan belakangan.

**Wholemeal programming**

Mari kita rangkum semua yang sudah kita pelajari ke dalam sebuah contoh yang
menunjukkan kekuatan pemrograman bergaya *wholemeal*. Fungsi `foobar`
didefinisikan sebagai berikut:

> foobar :: [Integer] -> Integer
> foobar []     = 0
> foobar (x:xs)
>   | x > 3     = (7*x + 2) + foobar xs
>   | otherwise = foobar xs

Fungsi tersebut terlihat cukup jelas, akan tetapi bukan gaya Haskell yang bagus.
Masalahnya adalah:

  * melakukan terlalu banyak hal sekaligus; dan
  * bekerja di level yang terlalu rendah.

Daripada memikirkan apa yang mau kita lakukan ke tiap elemen, kita lebih baik
memikirkan transformasi bertahap ke semua elemen dengan menggunakan pola rekursi
yang kita ketahui. Berikut adalah implementasi `foobar` yang lebih idiomatis:

> foobar' :: [Integer] -> Integer
> foobar' = sum . map (\x -> 7*x + 2) . filter (>3)

Kali ini `foobar` didefinisikan sebagai "pipa" dari tiga fungsi: pertama, kita
membuang semua elemen yang tidak lebih besar dari tiga; lalu kita terapkan operasi
aritmetik ke tiap elemen sisanya; akhirnya, kita jumlahkan semuanya.

Perhatikan `map` dan `filter` diaplikasikan parsial. Sebagai contoh, tipe `filter`
adalah

~~~~ {.haskell}
(a -> Bool) -> [a] -> [a]
~~~~

Mengaplikasikannya ke `(>3)` (yang bertipe `Integer -> Bool`) menghasilkan
fungsi bertipe `[Integer] -> [Integer]`, yang merupakan fungsi yang tepat untuk
digabungkan dengan fungsi lain yang menerima `[Integer]`.

Gaya pemrograman seperti ini yang mendefinisikan fungsi tanpa referensi ke
argumennya (atau bisa juga dibilang menyatakan *definisi* sebuah fungsi ketimbang
apa yang fungsi tersebut *lakukan*) disebut sebagai "point-free". Gaya seperti ini
lebih enak dilihat. Beberapa orang bahkan sampai berkata bahwa gaya "point-free"
harus sebisa mungkin dipakai, meski tentu jika terlalu jauh malah akan membingungkan.
`lambdabot` di kanal IRC `#haskell` memiliki perintah `@pl` untuk mengubah fungsi
menjadi bergaya "point-free". Contohnya:

    @pl \f g x y -> f (x ++ g x) (g y)
    join . ((flip . ((.) .)) .) . (. ap (++)) . (.)

Terlihat untuk kasus ini, fungsi malah menjadi sulit dibaca.

*Folds*
-------

Satu lagi pola rekursi yang akan dibahas: *folds* (terjemahan: lipat). Berikut
adalah beberapa fungsi yang memiliki pola yang serupa: semuanya "menggabungkan"
elemen-elemen yang dimiliki menjadi satu jawaban.

> sum' :: [Integer] -> Integer
> sum' []     = 0
> sum' (x:xs) = x + sum' xs
>
> product' :: [Integer] -> Integer
> product' [] = 1
> product' (x:xs) = x * product' xs
>
> length' :: [a] -> Int
> length' []     = 0
> length' (_:xs) = 1 + length' xs

Apa yang sama dari ketiganya dan apa yang berbeda? Kita akan memisahkan
bagian yang berbeda dengan menggunakan fungsi "*higher-order*".

> fold :: b -> (a -> b -> b) -> [a] -> b
> fold z f []     = z
> fold z f (x:xs) = f x (fold z f xs)

Perhatikan bahwa `fold` sebenarnya mengganti `[]` dengan `z`, dan `(:)` dengan `f`.
Dengan kata lain:

    fold f z [a,b,c] == a `f` (b `f` (c `f` z))

(Jika kalian membayangkan `fold` dengan perspekif tersebut, kalian mungkin bisa
menemukan cara untuk menggeneralisir `fold` ke tipe data lain selain list)

Mari tulis ulang `sum'`, `product'`, dan `length'` dengan menggunakan `fold`:

> sum''     = fold 0 (+)
> product'' = fold 1 (*)
> length''  = fold 0 (\_ s -> 1 + s)

(Selain `(\_ s -> 1 + s)` kita juga bisa menulisnya `(\_ -> (1+))` atau
bahkan `(const (1+))`.)

`fold` sudah tersedia di `Prelude`, dengan nama [`foldr`]
(https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.8.1.0/Prelude.html#v:foldr)
Argumen-argumen untuk fungsi `foldr` sedikit berbeda urutannya dengan `fold` tadi,
meski bekerja dengan cara yang sama. Berikut adalah beberapa fungsi dari `Prelude`
yang didefinisikan dengan `foldr`:

  * `length`  `::          [a] -> Int`
  * `sum`     `:: Num a => [a] -> a`
  * `product` `:: Num a => [a] -> a`
  * `and`     `::          [Bool] -> Bool`
  * `or`      `::          [Bool] -> Bool`
  * `any`     `:: (a -> Bool) -> [a] -> Bool`
  * `all`     `:: (a -> Bool) -> [a] -> Bool`

Ada juga [`foldl`](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.8.1.0/Prelude.html#v:foldl)
yang "melipat (*fold*) dari kiri",

    foldr f z [a,b,c] == a `f` (b `f` (c `f` z))
    foldl f z [a,b,c] == ((z `f` a) `f` b) `f` c

Pada umumnya, kita sebaiknya menggunakan [`foldl'` dari
`Data.List`](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.8.1.0/Data-List.html#v:foldl)
, yang sama seperti `foldl` tapi lebih efisien.
