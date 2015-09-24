
 <!-- CLASS

> {-# OPTIONS_GHC -Wall #-}
>
> import Prelude hiding (Functor(..))

-->

*Functor*
=========

Bacaan tambahan:

  * *Learn You a Haskell*, [*The Functor typeclass*](http://learnyouahaskell.com/making-our-own-types-and-typeclasses#the-functor-typeclass)
  * [*Typeclassopedia*](http://www.haskell.org/haskellwiki/Typeclassopedia)

Motivasi
--------

Dalam beberapa minggu terakhir kita telah melihat beberapa fungsi yang
dirancang untuk "memetakan" (*map*) sebuah fungsi ke tiap elemen di dalam
sesuatu yang menyerupai *container*. Seperti:

  * `map :: (a -> b) -> [a] -> [b]`

  * `treeMap :: (a -> b) -> Tree a -> Tree b`

  * Di tugas 5 banyak yang melakukan hal yang serupa ketika harus
    menerapkan `eval :: ExprT -> Int` ke sebuah `Maybe ExprT` untuk
    mendapatkan sebuah `Maybe Int`.

    `maybeEval :: (ExprT -> Int) -> Maybe ExprT -> Maybe Int`

    `maybeMap :: (a -> b) -> Maybe a -> Maybe b`

Terdapat pengulangan pola di sini, dan sebagai programer Haskell yang baik
kita harus tahu bagaimana cara menggeneralisirnya! Jadi yang mana yang serupa
dari tiap contoh, dan mana yang berbeda?

Yang berbeda tentu *container* yang dipetakan terhadap fungsi:

~~~~ {.haskell}
thingMap :: (a -> b) -> f a -> f b
~~~~

Akan tetapi apa sebenarnya *container* ini? Bisakah kita *assign* variabel
tipe seperti `f` ke *container* tersebut?

Bahasan singkat tentang *kind*
------------------------------

Seperti halnya ekspresi yang memiliki tipe, tipe juga memiliki "tipe",
yang disebut *kind* (translasi: jenis). Sebelum kalian bertanya: tidak,
tak ada lagi tingkat di atas *kind* -- paling tidak, tidak di Haskell.
Di `ghci` kita bisa menanyakan *kind* dari tipe dengan menggunakan `:kind`.
Sebagai contoh, mari kita cari tahu apa *kind* dari `Int`:

    Prelude> :k Int
    Int :: *

Kita lihat bahwa `Int` memiliki *kind* `*`. Sebenarnya tiap tipe dari
sebuah nilai memiliki *kind* `*`.

    Prelude> :k Bool
    Bool :: *
    Prelude> :k Char
    Char :: *
    Prelude> :k Maybe Int
    Maybe Int :: *

Jika `Maybe Int` memiliki *kind* `*`, bagaimana dengan `Maybe`? Perhatikan
bahwa tidak ada nilai untuk tipe `Maybe`. Terdapat nilai untuk tipe `Maybe Int`,
dan tipe `Maybe Bool`, tapi tidak untuk tipe `Maybe`. Tapi `Maybe` merupakan
sesuatu yang menyerupai tipe yang valid. Jadi apa dong? Apa *kind* yang
dimilikinya? Mari tanyakan `ghci`:

    Prelude> :k Maybe
    Maybe :: * -> *

`ghci` barkata bahwa `Maybe` memiliki *kind* `* -> *`. `Maybe` bisa dikatakan
sebagai sebuah fungsi terhadap tipe. Kita biasa menyebutnya *type constructor*
(konstruktor tipe). `Maybe` menerima input tipe dengan *kind* `*`, dan
menghasilkan sebuah tipe lain dengan *kind* `*`. Sebagai contoh, dia bisa
menerima input `Int :: *` dan menghasilkan tipe baru `Maybe Int :: *`.

Adakah konstruktor tipe lain dengan *kind* `* -> *`?  Tentunya. Contohnya
`Tree`, atau konstruktor tipe list yang ditulis sebagai `[]`.

    Prelude> :k []
    [] :: * -> *
    Prelude :k [] Int
    [] Int :: *
    Prelude> :k [Int]  -- special syntax for [] Int
    [Int] :: *
    Prelude> :k Tree
    Tree :: * -> *

Bagaimana dengan konstruktor tipe dengan *kind* lainnya?  Bagaimana dengan
`JoinList` dari tugas 7?

> data JoinList m a = Empty
>                   | Single m a
>                   | Append m (JoinList m a) (JoinList m a)

    Prelude> :k JoinList
    JoinList :: * -> * -> *

Masuk akal. `JoinList` menerima *dua* tipe sebagai parameter dan memberikan
sebuah tipe baru. Tentunya, `JoinList` *curried*, jadi kita juga bisa
menganggapnya menerima sebuah tipe dan menghasilkan sesuatu dengan *kind*
`* -> *`. Berikut satu contoh lagi:

    Prelude> :k (->)
    (->) :: * -> * -> *

Konstruktor tipe fungsi menerima dua tipe sebagai argumen. Seperti operator
kita bisa menggunakannya *infix*:

    Prelude> :k Int -> Char
    Int -> Char :: *

Bisa juga tidak:

    Prelude> :k (->) Int Char
    (->) Int Char :: *

OK, bagaimana dengan ini?

> data Funny f a = Funny a (f a)

    Prelude> :k Funny
    Funny :: (* -> *) -> * -> *

`Funny` menerima dua argumen, yang pertama tipe dengan *kind* `* -> *`,
dan yang kedua tipe dengan *kind* `*`, dan menghasilkan sebuah tipe.
Bagaimana GHCi tahu apa *kind* dari `Funny`?  Dia bisa melakukan *kind inference*,
serupa dengan *type inference*. `Funny` adalah sebuah konstruktor tipe
*higher-order*, serupa dengan `map` yang merupakansebuah fungsi *higher-order*.
Perhatikan bahwa tipe juga bisa diterapkan sebagian (*partially applied*)
seperti fungsi:

    Prelude> :k Funny Maybe
    Funny Maybe :: * -> *
    Prelude> :k Funny Maybe Int
    Funny Maybe Int :: *

*Functor*
---------

Inti dari pola pemetaan yang kita lihat adalah fungsi *higher-order*
yang bertipe seperti

~~~~ {.haskell}
thingMap :: (a -> b) -> f a -> f b
~~~~

di mana `f` ialah variabel tipe yang mewakili tipe dengan *kind* `* -> *`.
Jadi, bisakah kita menulis fungsi bertipe demikian untuk semuanya?

~~~~ {.haskell}
thingMap :: (a -> b) -> f a -> f b
thingMap h fa = ???
~~~~

Tidak juga.  Tidak banyak yang bisa kita lakukan jika kita tidak tahu apa
`f` tersebut. `thingMap` harus bekerja secara berbeda untuk tiap `f` apapun.
Solusinya adalah dengan membuat *type class*, yang secara tradisi disebut `Functor`:

> class Functor f where
>   fmap :: (a -> b) -> f a -> f b

*Catatan*: `Functor` didefinisikan di *Prelude*. Perhatikan bahwa nama "*functor*"
berasal dari *category theory*, dan *tidak sama* dengan functors di C++
(yang merupakan fungsi *first-class*).

Sekarang kita bisa implemen *type class* tersebut secara spesifik untuk tiap `f`.
Perhatikan bahwa `Functor` melakukan abstraksi terhadap tipe dengan *kind*
`* -> *`. Jadi mustahil untuk menulis

~~~~ {.haskell}
instance Functor Int where
  fmap = ...
~~~~

Jika kita coba, kita akan mendapatkan *kind mismatch error*:

    [1 of 1] Compiling Main             ( 09-functors.lhs, interpreted )

    09-functors.lhs:145:19:
        Kind mis-match
        The first argument of `Functor' should have kind `* -> *',
        but `Int' has kind `*'
        In the instance declaration for `Functor Int'

Jika kita sudah memahami *kind*, pesan di atas cukup jelas.

Cukup beralasan untuk menjadikan `Maybe` sebagai anggota dari `Functor`.
Mari kita lakukan. Dengan mengikuti tipe, ini menjadi sangat mudah:

> instance Functor Maybe where
>   fmap _ Nothing  = Nothing
>   fmap h (Just a) = Just (h a)

Bagaimana dengan list?

> instance Functor [] where
>   fmap _ []     = []
>   fmap f (x:xs) = f x : fmap f xs
>   -- or just
>   -- fmap = map

Gampang!  Bagaimana dengan `IO`? Bisakah menjadikan `IO` sebagai anggota
`Functor`?

Tentu.  `fmap :: (a -> b) -> IO a -> IO b` menghasilkan sebuah *IO*
yang akan menjalankan aksi `IO a`, lalu menerapkan fungsi tersebut untuk
mengubah hasilnya sebelum dikembalikan. Kita bisa mengimplementasikannya
tanpa masalah:

~~~~ {.haskell}
instance Functor IO where
  fmap f ioa = ioa >>= (\a -> return (f a))
~~~~

atau

> instance Functor IO where
>   fmap f ioa = ioa >>= (return . f)

Sekarang mari kita coba sesuatu yang lebih memutar otak:

> instance Functor ((->) e) where

Apa!? Mari kita ikuti tipenya: jika `f = (->) e` maka kita ingin agar

> fmap :: (a -> b) -> (->) e a -> (->) e b

atau, jika `(->)` ditulis dalam bentuk infix:

> fmap :: (a -> b) -> (e -> a) -> (e -> b)

Hmm, tipe *signature* tersebut terlihat familiar...

> instance Functor ((->) e) where
>   fmap = (.)

Gila!  Apa artinya? Kita bisa mengibaratkan nilai bertipe `(e -> a)`
adalah *container* berindeks `e` dengan nilai `a` untuk tiap nilai `e`.
Pemetaan sebuah fungsi ke tiap nilai di dalam *container* tersebut
sangatlah persis dengan komposisi fungsi. Untuk mengambil elemen dari
*container* hasilnya, kita pertama menerapkan fungsi `(e -> a)` untuk
mendapatkan `a` dari *container* asal, lalu menerapkan fungsi `(a -> b)`
untuk mengubah elemen yang didapat.

 <!--

Local Variables:
mode:markdown
compile-command:"mk pre"
End:

-->
