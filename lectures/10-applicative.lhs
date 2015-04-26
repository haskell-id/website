
 <!-- CLASS

> {-# LANGUAGE GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}

-->

*Applicative functor*, Bagian I
===============================

Bacaan tambahan:

  * [*Applicative Functors*](http://learnyouahaskell.com/functors-applicative-functors-and-monoids#applicative-functors) dari *Learn You a Haskell*
  * [*Typeclassopedia*](http://www.haskell.org/haskellwiki/Typeclassopedia)

Motivasi
--------

Perhatikan tipe `Employee` berikut:

> type Name = String
>
> data Employee = Employee { name    :: Name
>                          , phone   :: String }
>                 deriving Show

Tentunya konstruktor `Employee` bertipe

~~~~ {.haskell}
Employee :: Name -> String -> Employee
~~~~

Jika kita memiliki sebuah `Name` dan sebuah `String`, kita bisa terapkan
(*apply*) konstruktor `Employee` untuk mendapatkan sebuah `Employee`.

Misalkan kita tidak memiliki sebuah `Name` dan `String`, melainkan
`Maybe Name` dan `Maybe String`. Mungkin karena kita mendapatkannya dengan
melakukan *parsing* berkas yang penuh *error*, atau dari *form* yang tidak
sepenuhnya diisi, atau kasus-kasus lainnya. Mungkin kita tidak bisa membuat
`Employee`, tapi paling tidak kita bisa membuat `Maybe Employee`.

Kita akan mengubah fungsi `(Name -> String -> Employee)` menjadi fungsi
`(Maybe Name -> Maybe String -> Maybe Employee)`. Bisakah kita membuat
sesuatu bertipe seperti ini?

~~~~ {.haskell}
(Name -> String -> Employee) ->
(Maybe Name -> Maybe String -> Maybe Employee)
~~~~

Tentu saja bisa. Saya pun yakin kalian sudah bisa membuatnya sambil tidur
sekarang. Kita bisa membayangkan bagaiman fungsi tersebut bekerja. Jika
salah satu dari *name* atau string berupa `Nothing`, kita mendapatkan
`Nothing`. Jika keduanya berupa `Just`, kita mendapatkan `Employee` yang
dibuat dengan konstruktor `Employee` (terbungkus dengan `Just`). Mari
kita lanjutkan...

Sekarang begini: bukannya kita memiliki sebuah `Name` dan `String`, namun
kita punya `[Name]` dan `[String]`. Mungkin kita bisa mendapatkan `[Employee]`
di sini? Sekarang kita mau

~~~~ {.haskell}
(Name -> String -> Employee) ->
([Name] -> [String] -> [Employee])
~~~~

Kita bisa bayangkan dua cara untuk ini. Kita bisa memasangkan satu `Name`
untuk satu `String` untuk membuat `Employee`, atau kita bisa memasangkan
`Name` dan `String` dengan segala kemungkinannya.

Atau bagaimana kalau begini: kita punya `(e -> Name)` dan `(e -> String)`
untuk `e` apapun.  Sebagai contoh, `e` mungkin sebuah struktur data yang
besar dan kita punya fungsi untuk mengekstrak `Name` dan `String` darinya.
Bisakah kita membuatnya menjadi `(e -> Employee)`, yang merupakan resep
untuk mengekstrak `Employee` dari struktur tersebut?

~~~~ {.haskell}
(Name -> String -> Employee) ->
((e -> Name) -> (e -> String) -> (e -> Employee))
~~~~

Tidak masalah, dan kali ini hanya ada satu cara untuk menulis fungsi tersebut.

Generalisir
-----------

Setelah melihat kegunaan pola seperti di atas, mari kita sedikit menggeneralisir.
Tipe fungsi yang kita inginkan adalah seperti berikut:

~~~~ {.haskell}
(a -> b -> c) -> (f a -> f b -> f c)
~~~~

Hmm, terlihat familiar... serupa dengan tipe dari `fmap`!

~~~~ {.haskell}
fmap :: (a -> b) -> (f a -> f b)
~~~~

Satu-satunya perbedaan adalah sebuah argumen tambahan. Kita bisa menyebut
fungsi baru ini sebagai `fmap2`, karena menerima sebuah fungsi dengan dua
argumen. Mungkin kita bisa menuliskannya dalam bentuk `fmap`, sehingga kita
hanya memerlukan *constraint* `Functor` pada `f`:

> fmap2 :: Functor f => (a -> b -> c) -> (f a -> f b -> f c)
> fmap2 h fa fb = undefined

Setelah mencoba, `Functor` tidak cukup membantu kita untuk membuat `fmap2`.
Apa yang salah? Kita memiliki

~~~~ {.haskell}
h  :: a -> b -> c
fa :: f a
fb :: f b
~~~~

Perhatikan bahwa kita bisa menuliskan tipe `h` sebagai `(a -> (b -> c))`.
Jadi kita memiliki sebuah fungsi yang menerima `a`, dan sebuah nilai bertipe
`f a`. Kita tinggal "mengangkat" fungsi tersebut melewati `f` dengan `fmap`
yang akan menghasilkan:

~~~~ {.haskell}
h         :: a -> (b -> c)
fmap h    :: f a -> f (b -> c)
fmap h fa :: f (b -> c)
~~~~

Oke, sekarang kita memiliki sesuatu bertipe `f (b -> c)` dan `f b`...
dan di sinilah kita *stuck*! `fmap` tidak bisa membantu lebih jauh.
`fmap` memberikan cara untuk menerapkan fungsi ke nilai-nilai yang berada
di dalam konteks `Functor`, tapi yang kita butuhkan sekarang adalah
penerapan fungsi yang juga berada di dalam konteks `Functor` ke nilai-nilai
yang berada di konteks `Functor`.

*Applicative*
-------------

*Functor* yang memiliki karakter seperti di atas (penerapan fungsi
berdasarkan konteks, *contextual application*) disebut *applicative*.
Kelas `Applicative` (didefinisikan di [`Control.Applicative`]
(http://haskell.org/ghc/docs/latest/html/libraries/base/Control-Applicative.html))
berpola seperti berikut ini.

> class Functor f => Applicative f where
>   pure  :: a -> f a
>   (<*>) :: f (a -> b) -> f a -> f b

Operator `(<*>)` (biasa disebut "ap", versi singkat dari  *apply*,
terjemahan: terap) mewakili prinsip penerapan kontekstual
(*contextual application*). Perhatikan bahwa kelas `Applicative`
mewajibkan anggotanya untuk juga menjadi anggota `Functor`, sehingga
kita selalu bisa menggunakan `fmap` terhadap anggota `Applicative`.
`Applicative` juga memiliki *method* lain bernama `pure` yang
memungkinkan kita untuk memasukkan nilai `a` ke sebuah *container*.
Untuk saat ini, kita bisa bisa menyebut `pure` sebagai `fmap0`:

~~~~ {.haskell}
pure  :: a             -> f a
fmap  :: (a -> b)      -> f a -> f b
fmap2 :: (a -> b -> c) -> f a -> f b -> f c
~~~~

Setelah kita memiliki `(<*>)`, kita bisa mengimplemen `fmap2`, yang
disebut `liftA2` di pustaka standar:

> liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
> liftA2 h fa fb = (h `fmap` fa) <*> fb

Bahkan, pola ini cukup umum sehingga `Control.Applicative` mendefinisikan
`(<$>)` sebagai sinonim untuk `fmap`,

> (<$>) :: Functor f => (a -> b) -> f a -> f b
> (<$>) = fmap

sehingga kita bisa menulis

~~~~ {.haskell}
liftA2 h fa fb = h <$> fa <*> fb
~~~~

Bagaimana dengan `liftA3`?

> liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
> liftA3 h fa fb fc = ((h <$> fa) <*> fb) <*> fc

(Perhatikan bahwa prioritas dan sifat asosiatif dari `(<$>)` dan `(<*>)`
didefinisikan sedemikian rupa sehingga semua tanda kurung di atas menjadi
tidak diperlukan.)

Ringkas!  Tidak seperti perpindahan dari `fmap` ke `liftA2` (yang
membutuhkan generalisasi dari `Functor` ke `Applicative`), dari `liftA2` ke
`liftA3` (dan dari situ ke `liftA4`, ... dan seterusnya) tidak memerlukan
usaha tambahan. `Applicative` sudah cukup.

Sebenarnya, ketika kita memiliki semua argumen, kita tidak perlu untuk
menyebutnya `liftA2`, `liftA3`, dan seterusnya. Cukup gunakan pola `f <$>
x <*> y <*> z <*> ...` langsung. (`liftA2` dan lainnya berguna ketika saat
aplikasi parsial.)

Bagaimana dengan `pure`?  `pure` digunakan ketika kita ingin menerapkan
sebuah fungsi ke beberapa argumen yang berada di dalam konteks *functor* `f`,
tetapi salah satu argumennya tidak berada di dalam `f`. Argumen tersebut bisa
disebut "*pure*" (murni). Kita bisa menggunakan `pure` untuk mengangkat mereka
ke `f` sebelum melakukan penerapan. Sebagai contoh:

> liftX :: Applicative f => (a -> b -> c -> d) -> f a -> b -> f c -> f d
> liftX h fa b fc = h <$> fa <*> pure b <*> fc

Hukum-hukum *applicative*
-------------------------

Hanya ada satu hukum yang benar-benar menarik untuk `Applicative`:

~~~~ {.haskell}
f `fmap` x === pure f <*> x
~~~~

Memetakan sebuah fungsi `f` pada *container* `x` harus memberikan hasil yang
sama dengan memasukkan fungsi tersebut ke *container* lalu menerapkannya ke `x`
dengan `(<*>)`.

Ada hukum-hukum lainnya, tetapi mereka tidak begitu instruktif. Kalian bisa
membacanya sendiri jika mau.

Contoh *applicative*
--------------------

**Maybe**

Mari tulis beberapa anggota dari `Applicative`, dimulai dengan `Maybe`. `pure`
bekerja dengan memasukkan sebuah nilai ke bungkus `Just`. `(<*>)` adalah aplikasi/
penerapan fungsi dengan kemungkinan gagal, yang akan menghasilkan `Nothing` jika
salah satu dari fungsi atau argumennya berupa `Nothing`.

> instance Applicative Maybe where
>   pure              = Just
>   Nothing <*> _     = Nothing
>   _ <*> Nothing     = Nothing
>   Just f <*> Just x = Just (f x)

Mari lihat contohnya:

> m_name1, m_name2 :: Maybe Name
> m_name1 = Nothing
> m_name2 = Just "Brent"
>
> m_phone1, m_phone2 :: Maybe String
> m_phone1 = Nothing
> m_phone2 = Just "555-1234"
>
> exA = Employee <$> m_name1 <*> m_phone1
> exB = Employee <$> m_name1 <*> m_phone2
> exC = Employee <$> m_name2 <*> m_phone1
> exD = Employee <$> m_name2 <*> m_phone2


 <!--

Local Variables:
mode:markdown
compile-command:"mk pre"
End:

-->
