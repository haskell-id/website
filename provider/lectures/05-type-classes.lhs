<!--
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
-->

Polimorfisme lanjutan dan *type classes*
==========================================

Polimorfisme (*polymorphism*) di Haskell dikenal sebagai polimorfisme parametrik
(*parametric polymorphism*). Ini berarti fungsi polimorfis harus bekerja secara
*seragam* terhadap tipe input apapun. Hal tersebut memiliki implikasi yang
menarik bagi pemrogram maupun pengguna fungsi polimorfis tersebut.

Parametrisitas (*parametricity*)
--------------------------------

Perhatikan tipe

~~~~ {.haskell}
a -> a -> a
~~~~

Ingat bahwa `a` adalah sebuah variable tipe yang bisa berarti tipe apapun.
Fungsi seperti apa yang bertipe seperti itu?

Mari kita coba begini

~~~~ {.haskell}
f :: a -> a -> a
f x y = x && y
~~~~

Ternyata tidak bisa.  *Syntax*-nya valid, tetapi tidak *type check* (tipenya tidak cocok).
Pesan *error*-nya:

    2012-02-09.lhs:37:16:
        Couldn't match type `a' with `Bool'
          `a' is a rigid type variable bound by
              the type signature for f :: a -> a -> a at 2012-02-09.lhs:37:3
        In the second argument of `(&&)', namely `y'
        In the expression: x && y
        In an equation for `f': f x y = x && y

Hal ini dikarenakan *pemanggil* fungsi polimorfis yang menentukan tipenya.
Implementasi di atas mencoba memilih tipe yang spesifik (`Bool`),
tapi tetap ada kemungkinan untuk bertipe `String`, `Int`, atau lainnya.
Bahkan, bisa juga tipe baru buatan orang lain yang didefinisikan dengan `f`.
Kita tidak mungkin mengetahui tipe apa yang akan kita terima.

Dengan kata lain, tipe seperti

~~~~ {.haskell}
a -> a -> a
~~~~

bisa dibaca sebagai jaminan kalau fungsi bertipe demikian akan bekerja dengan benar,
tidak peduli tipe apapun yang akan diberikan.

Impelementasi yang lain bisa seperti

    f a1 a2 = case (typeOf a1) of
                Int  -> a1 + a2
                Bool -> a1 && a2
                _    -> a1

di mana `f` memberikan perlakuan yang berbeda sesuai dengan tipe. Sama seperti
halnya di Java:

    class AdHoc {

        public static Object f(Object a1, Object a2) {
            if (a1 instanceof Integer && a2 instanceof Integer) {
                return (Integer)a1 + (Integer)a2;
            } else if (a1 instanceof Boolean && a2 instanceof Boolean) {
                return (Boolean)a1 && (Boolean)a2;
            } else {
                return a1;
            }
        }

        public static void main (String[] args) {
            System.out.println(f(1,3));
            System.out.println(f(true, false));
            System.out.println(f("hello", "there"));
        }

    }

    [byorgey@LVN513-9:~/tmp]$ javac Adhoc.java && java AdHoc
    4
    false
    hello

Hal ini tidak bisa dilakukan di Haskell. Haskell tidak memiliki operator
seperti `instanceof` di Java, dan tidak mungkin untuk menanyakan tipe dari
suatu nilai. Salah satu alasannya ialah tipe di Haskell dihapus oleh *compiler*
setelah dicek: tidak ada informasi tipe di saat *runtime*! Akan tetapi,
ada pula alasan lainnya.

Gaya polimorfisme seperti ini dikenal sebagai polimorfisme parametrik.
Fungsi seperti `f :: a -> a -> a` dikatakan parametrik untuk tipe `a`.
Di sini, parametrik hanyalah sebutan untuk "bekerja secara seragam
untuk semua tipe yang diberikan". Di Java, gaya polimorfisme seperti ini
disediakan oleh *generics* (yang terinspirasi dari Haskell: salah satu
disainer Haskell, [Philip Wadler](http://homepages.inf.ed.ac.uk/wadler/),
adalah salah satu pengembang kunci Java generics).

Jadi fungsi apa yang mungkin bertipe seperti ini? Ternyata hanya ada dua!

~~~~ {.haskell}
f1 :: a -> a -> a
f1 x y = x

f2 :: a -> a -> a
f2 x y = y
~~~~

Jadi tipe `a -> a -> a` cukup banyak mengandung informasi.

Mari bermain *game* parametrisitas! Perhatikan tipe-tipe berikut.
Untuk tiap tipe, tentukan perilaku yang mungkin dimiliki.

  * `a -> a`
  * `a -> b`
  * `a -> b -> a`
  * `[a] -> [a]`
  * `(b -> c) -> (a -> b) -> (a -> c)`
  * `(a -> a) -> a -> a`

Dua pandangan tentang parametrisitas
------------------------------------

Sebagai penulis implementasi fungsi, ini terasa membatasi.
Terlebih jika kita sudah terbiasa dengan bahasa yang
memiliki hal seperti `instanceof` di Java. "Koq gitu? Kenapa
tidak boleh melakukan X?"

Akan tetapi, ada pandangan berbeda. Sebagai *pengguna* dari fungsi
polimorfis, parametrisitas bukan berarti *larangan*, tapi lebih
sebagai *jaminan*. Pada umumnya, *tools* akan lebih mudah digunakan
dan dibuktikan jika *tools*-nya memberikan jaminan tentang *sifatnya*.
Parametrisitas adalah salah satu alasan mengapa dengan hanya melihat
tipe dari sebuah fungsi Haskell bisa memberitahu kalian banyak hal
tentang fungsi tersebut.

OK, tapi terkadang kita perlu menentukan sesuatu berdasarkan tipe.
Contohnya, penjumlahan. Kita telah melihat bahwa penjumlahan adalah
polimorfis (berlaku untuk `Int`, `Integer`, dan `Double`) tapi tentu
fungsi tersebut perlu mengetahui tipe yang sedang dijumlahkan untuk
mengetahui apa yang harus dilakukan. Menjumlahkan dua `Integer` tentu
berbeda dengan menjumlahkan dua `Double`. Jadi bagaimana? Sihir?

Ternyata tidak!  Dan di Haskell, kita bisa menentukan apa yang harus dilakukan
berdasarkan tipe, meski berbeda dari yang sebelumnya kita bayangkan.
Mari mulai dengan melihat tipe dari `(+)`:

    Prelude> :t (+)
    (+) :: Num a => a -> a -> a

Hmm, apa yang `Num a =>` lakukan di sini?  Sebenarnya, `(+)` bukanlah
satu-satunya fungsi standar yang memiliki *double-arrow* di tipenya.
Berikut contoh-contoh lainnya:

~~~~ {.haskell}
(==) :: Eq a   => a -> a -> Bool
(<)  :: Ord a  => a -> a -> Bool
show :: Show a => a -> String
~~~~

Apakah yang terjadi?

*Type class*
--------------

`Num`, `Eq`, `Ord`, and `Show` adalah *type class*, dan kita sebut
`(==)`, `(<)`, dan `(+)` "*type-class polymorphic*".  Secara intuisi,
*type class* bisa dianggap sebagai himpunan dari tipe yang memiliki
beberapa operasi yang terdefinisikan untuk mereka. Fungsi *type class polymorphic*
hanya menerima tipe yang merupakan anggota (*instances*) dari *type class* tersebut.
Sebagai contoh, mari lihat detil dari *type class* `Eq`.

~~~~ {.haskell}
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
~~~~

Kita bisa membacanya seperti ini: `Eq` dideklarasikan sebagai *type class*
dengan satu argumen, `a`. Tiap tipe `a` yang ingin menjadi anggota (*instance*)
dari `Eq` harus mendefinisikan dua fungsi, `(==)` dan `(/=)`, dengan tipe
yang tercantum. Misalnya, untuk membuat `Int` menjadi anggota `Eq` kita
harus mendefinisikan `(==) :: Int -> Int -> Bool` dan `(/=) ::
Int -> Int -> Bool`. (Tidak perlu tentunya, karena 
*Prelude* sudah mendefinisikan `Int` sebagai anggota `Eq`.)

Mari lihat tipe dari `(==)` lagi:

~~~~ {.haskell}
(==) :: Eq a => a -> a -> Bool
~~~~

`Eq a` sebelum `=>` adalah sebuah *type class constraint*.
Kita bisa menyebutnya:  untuk semua tipe `a`, *selama `a` adalah
anggota `Eq`*, `(==)` bisa menerima dua nilai bertipe `a` dan
mengembalikan sebuah `Bool`. Pemanggilan `(==)` kepada tipe
yang bukan anggota `Eq` mengakibatkan *type error*.

Jika tipe polimorfis adalah suatu jaminan fungsi akan bekerja dengan
apapun tipe input yang pemanggil berikan, fungsi polimorfis *type class* 
adalah jaminan *terbatas* bahwa fungsi akan bekerja dengan apapun
tipe yang diberikan *selama* tipe tersebut anggota dari *type class*
tersebut.

Hal yang perlu diingat, ketika `(==)` (atau *method* *type class*
lainnya) digunakan, *compiler* menggunakan *type inference*
(pada argumen-argumennya) untuk mencari tahu *implementasi `(==)`
yang mana yang akan dipilih*. Mirip dengan *overloaded method* di
bahasa seperti Java.

Untuk mengerti lebih jauh, mari buat tipe sendiri dan jadikan sebagai
anggota `Eq`.

> data Foo = F Int | G Char
>
> instance Eq Foo where
>   (F i1) == (F i2) = i1 == i2
>   (G c1) == (G c2) = c1 == c2
>   _ == _ = False
>
>   foo1 /= foo2 = not (foo1 == foo2)

Cukup merepotkan untuk mendefinisikan `(==)` and `(/=)`. Sebenarnnya
*type class* bisa memberikan implementasi *default* untuk *method*
dalam bentuk *method* lainnya. Ini akan digunakan jika ada anggota yang
tidak meng-*override* implementasi *default*. Kita bisa mendeklarasikan
 `Eq` seperti ini:

~~~~ {.haskell}
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x /= y = not (x == y)
~~~~

Sekarang yang mendeklarasikan anggota `Eq` cukup memberikan implementasi
dari `(==)` saja, dan akan langsung mendapatkan `(/=)`. Tapi jika ada
alasan tertentu untuk mengganti implementasi *default* dari `(/=)`,
kita tetap bisa melakukannya.

Sebenarnya, *class* `Eq` dideklarasikan seperti ini:

~~~~ {.haskell}
class Eq a where
  (==), (/=) :: a -> a -> Bool
  x == y = not (x /= y)
  x /= y = not (x == y)
~~~~

Ini berarti tiap kita membuat anggota dari `Eq`, kita hanya perlu mendefinisikan
*salah satu dari* `(==)` atau `(/=)`, manapun yang lebih praktis; yang lainnya
akan terdefinisi secara otomatis dalam bentuk yang kita definisikan.
(Hati-hati: jika salah satu tidak terdefinisi, kita akan mendapatkan *infinite recursion*!)

Ternyata, `Eq` (beserta beberapa *type class* standar lainnya)
istimewa: GHC bisa membuat anggota `Eq` secara otomatis untuk kita.

> data Foo' = F' Int | G' Char
>   deriving (Eq, Ord, Show)

Di atas, kita memberitahu GHC untuk menjadikan tipe `Foo'` milik kita
anggota dari *type class* `Eq`, `Ord`, dan `Show`.

***Type class* dan interface di Java**

*Type class* serupa dengan *interface* di Java. Keduanya mendefinisikan
himpunan tipe/*class* yang mengimplementasikan beberapa operasi yang
spesifik. Akan tetapi, ada dua hal penting yang menunjukkan kalau
*type class* itu lebih umum daripada *interface* di Java:

  1. Ketika sebuah *class* didefinisikan di Java, *interface* yang
     dimiliki juga dideklarasikan. Sedangkan anggota (*instance* dari)
     *type class* dideklarasikan di tempat berbeda dari tipenya sendiri,
     bahkan bisa diletakkan di modul berbeda.

  2. Tipe untuk *method* di *type class* bisa lebih umum dan fleksibel
     ketimbang yang bisa diberikan di *method* *interface* di Java.
     Terlebih jika mengingat *type class* bisa menerima beberapa argumen.
     Sebagai contoh:

    > class Blerg a b where
    >   blerg :: a -> b -> Bool

    Penggunaan `blerg` berarti melakukan *multiple dispatch*: implementasi
    `blerg` yang dipilih *compiler* bergantung kepada *kedua* tipe `a` dan `b`.
    Di Java hal ini bukanlah hal yang mudah.

    *type class* di Haskell juga bisa menerima *method* biner (atau ternari,
    dst) dengan mudah, seperti

    > class Num a where
    >   (+) :: a -> a -> a
    >   ...

    Di Java hal ini bukanlah sesuatu yang mudah: satu contoh, salah satu dari
    dua argumen haruslah "istimewa" yaitu yang menerima *method* `(+)`. Hal ini
    mengakibatkan funsi menjadi asimetris dan canggung.
    Lebih jauh lagi, karena *subtyping* di Java, *interface* yang menerima
    dua argumen *tidak* menjamin kalau dua argumen tersebut bertipe sama, yang
    mengakibatkan implementasi operator biner seperti `(+)` menjadi canggung
    (biasanya melibatkan semacam pengecekan saat *runtime*).


***Type Class* standar**

Berikut adalah beberapa *type class* standar yang perlu kalian ketahui:

  * [Ord](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.8.1.0/Prelude.html#t:Ord)
    adalah untuk tipe-tipe yang tiap elemennya bisa diurutkan secara total
    (*totally ordered*). Dengan kata lain, tiap elemennya bisa dibandingkan
    satu sama lain untuk dilihat mana yang lebih kecil dari yang lain.
    Ini menyediakan operasi perbandingan seperti `(<)` dan `(<=)`, serta
    fungsi `compare`.

  * [Num](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.8.1.0/Prelude.html#t:Num)
    adalah untuk tipe "numerik", yang mendukung hal-hal seperti penjumlahan,
    pengurangan, dan perkalian. Satu hal yang penting: literal *integer*
    adalah polimorfis *type class*:

        Prelude> :t 5
        5 :: Num a => a

    Ini berarti literal seperti `5` bisa digunakan sebagai `Int`,
    `Integer`, `Double`,atau tipe apapun yang merupakan anggota (*instance*) dari
    `Num` (seperti `Rational`, `Complex Double`, atau tipe kalian sendiri...)

  * [Show](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.8.1.0/Prelude.html#t:Show)
    mendefinisikan *method* `show`, yang mengubah nilai menjadi `String`.

  * [Read](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.8.1.0/Prelude.html#t:Read)
    adalah dual (catatan penerjemah: istilah dari Category Theory,
    yang bisa diartikan kurang lebih sebagai "kebalikan") dari `Show`.

  * [Integral](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.8.1.0/Prelude.html#t:Integral)
    mewakili semua tipe bilangan bulat seperti `Int` dan `Integer`.

**Contoh type class**

Sebagai contoh untuk membuat *type class* sendiri:

> class Listable a where
>   toList :: a -> [Int]

Kita bisa anggap `Listable` sebagai himpunan sesuatu yang bisa diubah
menjadi sebuah list yang berisi `Int`. Perhatikan tipe dari `toList`:

~~~~ {.haskell}
toList :: Listable a => a -> [Int]
~~~~

Mari kita buat anggota (*instance*) dari `Listable`.  Pertama, sebuah `Int`
bisa diubah menjadi sebuah `[Int]` hanya dengan menciptakan list singleton,
begitu pula dengan `Bool`, dengan mengubah `True` menjadi `1` dan `False`
menjadi `0`:

> instance Listable Int where
>   -- toList :: Int -> [Int]
>   toList x = [x]
>
> instance Listable Bool where
>   toList True  = [1]
>   toList False = [0]

Kita tidak perlu repot untuk mengubah sebuah list `Int` ke list `Int`:

> instance Listable [Int] where
>   toList = id

Terakhir, kita ubah sebuah *binary tree* menjadi list dengan *flattening*:

> data Tree a = Empty | Node a (Tree a) (Tree a)
>
> instance Listable (Tree Int) where
>   toList Empty        = []
>   toList (Node x l r) = toList l ++ [x] ++ toList r

Jika kita membuat fungsi baru dengan menggunakan `toList`, fungsi tersebut
akan mendapatkan `constraint` `Listable` juga. Sebagai contoh:

> -- to compute sumL, first convert to a list of Ints, then sum
> sumL x = sum (toList x)

`ghci` akan memberitahukan kita bahwa tipe dari `sumL` adalah

~~~~ {.haskell}
sumL :: Listable a => a -> Int
~~~~

Masuk akal karena `sumL` hanya akan bekerja untuk tipe yang merupakan
anggota dari `Listable`, karena menggunakan `toList`. Bagaimana dengan
yang ini?

> foo x y = sum (toList x) == sum (toList y) || x < y

`ghci` memberitahukan bahwa tipe dari `foo` adalah

~~~~ {.haskell}
foo :: (Listable a, Ord a) => a -> a -> Bool
~~~~

`foo` bekerja untuk tipe yang merupakan anggota dari `Listable` dan `Ord`
, karena menggunakan `toList` dan perbandingan argumen-argumennya.

Sebagai contoh terakhir yang lebih rumit:

> instance (Listable a, Listable b) => Listable (a,b) where
>   toList (x,y) = toList x ++ toList y

Perhatikan bahwa kita bisa meletakkan *type class constraint* pada
tipe anggota (*instance*) dan juga pada tipe fungsi. Contoh tersebut
menunjukkan bahwa sebuah *pair* bertipe `(a,b)` adalah anggota dari
`Listable` selama `a` dan `b` juga anggota dari `Listable`. Lalu
kita bisa menggunakan `toList` untuk `a` dan `b` di dalam definisi
`toList` untuk *pair*. Definisi ini tidaklah rekursif! Versi `toList`
untuk *pair* memanggil versi `toList` yang *berbeda*, bukan yang
didefinisikan di dirinya sendiri.

 <!--

Local Variables:
mode:markdown
compile-command:"make explec"
End:

-->
