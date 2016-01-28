
 <!-- CLASS
 
> {-# OPTIONS_GHC -Wall #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}

-->

Folds and monoids
=================

Bacaan tambahan:

  * *Learn You a Haskell*, [*Only folds and horses*](http://learnyouahaskell.com/higher-order-functions#folds)
  * *Learn You a Haskell*, [*Monoids*](http://learnyouahaskell.com/functors-applicative-functors-and-monoids#monoids)
  * [*Fold*](http://haskell.org/haskellwiki/Fold) dari wiki Haskell
  * [*Monoids and Finger Trees*](http://apfelmus.nfshost.com/articles/monoid-fingertree.html), Heinrich Apfelmus
  * [*Haskell Monoids and their Uses*](http://blog.sigfpe.com/2009/01/haskell-monoids-and-their-uses.html), Dan Piponi
  * [Dokumentasi Data.Monoid](http://haskell.org/ghc/docs/latest/html/libraries/base/Data-Monoid.html)
  * [Dokumentasi Data.Foldable](http://haskell.org/ghc/docs/latest/html/libraries/base/Data-Foldable.html)

*Fold* lagi
-----------

Kita telah melihat bagaimana mendefinisikan fungsi *fold* untuk list,
tapi kita masi bisa membuatnya lebih umum sehingga bisa digunakan untuk
tipe data lainnya.

Perhatikan tipe data *binary tree* berikut, yang menyimpan data di dalam
*node* internal:

> data Tree a = Empty
>             | Node (Tree a) a (Tree a)
>   deriving (Show, Eq)
>
> leaf :: a -> Tree a
> leaf x = Node Empty x Empty

Mari kita buat sebuahfungsi untuk menghitung besarnya *tree* tersebut (banyaknya
*node*):

> treeSize :: Tree a -> Integer
> treeSize Empty        = 0
> treeSize (Node l _ r) = 1 + treeSize l + treeSize r

Bagaimana dengan jumlah keseluruhan data dalam *tree* yang berisikan `Integer`?

> treeSum :: Tree Integer -> Integer
> treeSum Empty     = 0
> treeSum (Node l x r)  = x + treeSum l + treeSum r

Atau kedalaman sebuah *tree*?

> treeDepth :: Tree a -> Integer
> treeDepth Empty        = 0
> treeDepth (Node l _ r) = 1 + max (treeDepth l) (treeDepth r)

Atau meratakan semua elemen di *tree* menjadi sebuah list?

> flatten :: Tree a -> [a]
> flatten Empty        = []
> flatten (Node l x r) = flatten l ++ [x] ++ flatten r

Dapatkah kalian melihat polanya? Tiap fungsi di atas:

  1. menerima sebuah `Tree` sebagai input
  1. cocokkan pola pada `Tree` tersebut
  1. jika `Empty`, berikan hasil sederhana
  1. jika berisi `Node`:
     1. panggil dirinya sendiri secara rekursif di kedua *subtree*
     1. gabungkan hasil dari rekursif tersebut dengan data `x` untuk
        mendapatkan hasil akhir

Sebagai programer yang baik, kita harus selalu berjuang mengabstraksikan
pola yang berulang. Mari kita generalisasi. Kita perlu menjadikan
bagian-bagian dari contoh diatas sebagai parameter, yang berbeda dari contoh
satu ke lainnya:

  1. Tipe hasil
  1. Jawaban untuk kasus `Empty`
  1. Cara untuk menggabungkan pemanggilan rekursifnya 

Kita sebut tipe data yang terkandung di dalam *tree* sebagai `a`, dan tipe
hasilnya sebagai `b`.

> treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
> treeFold e _ Empty        = e
> treeFold e f (Node l x r) = f (treeFold e f l) x (treeFold e f r)

Sekarang kita bisa mendefinisikan `treeSize`, `treeSum` dan contoh lainnya
dengan lebih sederhana. Mari kita lihat:

> treeSize' :: Tree a -> Integer
> treeSize' = treeFold 0 (\l _ r -> 1 + l + r)
>
> treeSum' :: Tree Integer -> Integer
> treeSum' = treeFold 0 (\l x r -> l + x + r)
> 
> treeDepth' :: Tree a -> Integer
> treeDepth' = treeFold 0 (\l _ r -> 1 + max l r)
>
> flatten' :: Tree a -> [a]
> flatten' = treeFold [] (\l x r -> l ++ [x] ++ r)

Kita juga bisa membuat fungsi *fold tree* baru dengan mudah:

> treeMax :: (Ord a, Bounded a) => Tree a -> a
> treeMax = treeFold minBound (\l x r -> l `max` x `max` r)

***Fold* pada ekspresi**

Di mana lagi kita telah melihat *fold*?

Ingat tipe `ExprT` dan fungsi `eval` dari tugas 5:

> data ExprT = Lit Integer
>            | Add ExprT ExprT
>            | Mul ExprT ExprT
>
> eval :: ExprT -> Integer
> eval (Lit i)     = i
> eval (Add e1 e2) = eval e1 + eval e2
> eval (Mul e1 e2) = eval e1 * eval e2

Hmm... terlihat familiar! Bagaimanakah bentuk *fold* untuk `ExprT`?

> exprTFold :: (Integer -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExprT -> b
> exprTFold f _ _ (Lit i)     = f i
> exprTFold f g h (Add e1 e2) = g (exprTFold f g h e1) (exprTFold f g h e2)
> exprTFold f g h (Mul e1 e2) = h (exprTFold f g h e1) (exprTFold f g h e2)
>
> eval2 :: ExprT -> Integer
> eval2 = exprTFold id (+) (*)

Sekarang kita bisa melakukan hal lain dengan mudah, seperti menghitung jumlah
literal dalam sebuah ekspresi:

> numLiterals :: ExprT -> Int
> numLiterals = exprTFold (const 1) (+) (+)

***Fold* secara umum**

Intinya, kita bisa membuat sebuah *fold* untuk banyak tipe data (meski tidak
semua). *Fold* dari tipe `T` akan menerima satu (*higher-order*) argumen untuk
tiap konstruktor dari `T`, dan menjabarkan bagaimana mengubah nilai di dalam
konstruktor tersebut menjadi nilai yang bertipe sama dengan tipe hasil akhirnya
(dengan asumsi tiap rekursif tehadap `T` sudah di-*fold* ke hasil akhir tersebut).
Hal ini menyebabkan fungsi-fungsi yang kita akan buat untuk `T` bisa diekspresikan
dalam bentuk *fold*.

Monoid
------

Berikut adalah satu lagi *type class* yang patut kalian ketahui, yang terdapat di
modul [`Data.Monoid`](http://haskell.org/ghc/docs/latest/html/libraries/base/Data-Monoid.html):

> class Monoid m where
>     mempty  :: m
>     mappend :: m -> m -> m
>
>     mconcat :: [m] -> m
>     mconcat = foldr mappend mempty
>
> (<>) :: Monoid m => m -> m -> m
> (<>) = mappend

`(<>)` didefinisikan sebagai sinonim untuk `mappend` (sejak GHC 7.4.1) karena
menuliskan `mappend` cukup merepotkan.

Tipe yang merupakan anggota dari `Monoid` memiliki elemen spesialyang disebut
`mempty`, dan sebuah operasi biner `mappend` (disingkat menjadi `(<>)`) yang
menerima dua nilai dari tipe tersebut dan mengembalikan satu. `mempty` merupakan
identitas untuk `<>`, dan `<>` bersifat asosiatif. Dengan kata lain, untuk semua
`x`, `y`, and `z`,

1. `mempty <> x == x`
2. `x <> mempty == x`
3. `(x <> y) <> z == x <> (y <> z)`

Asosiatif berarti kita bisa menulis seperti ini tanpa ambigu:

`a <> b <> c <> d <> e`

karena kita akan mendapatkan hal yang sama tidak peduli di mana kita meletakkan
tanda kurung.

Ada pula `mconcat`, yang digunakan untuk menggabungkan nilai-nilaipada sebuah list.

Secara *default*, implementasi `mconcat` menggunakan `foldr`, tapi dia dimasukkan
ke dalam kelas `Monoid` karena beberapa anggota `Monoid` mungkin saja memiliki
implementasi yang lebih efisien.

Jika kalian perhatikan, `monoid` ada di mana-mana. Mari kita buat beberapa
anggotanya sebagai latihan (semua ini sudah ada di pustaka bawaan).

List, dengan *concatenation*, merupakan sebuah monoid:

> instance Monoid [a] where
>   mempty  = []
>   mappend = (++)

Bisa dilihat sebelumnya bahwa penjumlahan merupakan monoid pada bilangan bulat
(atau bilangan rasional, atau bilangan riil..). Akan tetapi, begitu pula dengan
perkalian! Jadi bagaimana? Kita tidak bisa membuat satu tipe menjadi dua anggota
yang berbeda dari *type class* yang sama. Kita akan membuat dua buah `newtype`,
satu untuk tiap anggota *type class*:

> newtype Sum a = Sum a
>   deriving (Eq, Ord, Num, Show)
>
> getSum :: Sum a -> a
> getSum (Sum a) = a
>
> instance Num a => Monoid (Sum a) where
>   mempty  = Sum 0
>   mappend = (+)
>
> newtype Product a = Product a
>   deriving (Eq, Ord, Num, Show)
>
> getProduct :: Product a -> a
> getProduct (Product a) = a
>
> instance Num a => Monoid (Product a) where
>   mempty  = Product 1
>   mappend = (*)

Perhatikan bahwa misalnya untuk menemukan *product* dari sebuah list `Integer`
dengan menggunakan `mconcat`, kita harus mengubahnya dulu menjadi nilai-nilai
bertipe `Product Integer`:

> lst :: [Integer]
> lst = [1,5,8,23,423,99]
>
> prod :: Integer
> prod = getProduct . mconcat . map Product $ lst

(Tentu contoh ini konyol karena kita bisa langsung menggunakan fungsi bawaan
`product`, tapi pola seperti ini bisa berguna nantinya.)

*Pair* merupakan sebuah monoid selama komponen-komponennya juga monoid:

> instance (Monoid a, Monoid b) => Monoid (a,b) where
>   mempty = (mempty, mempty)
>   (a,b) `mappend` (c,d) = (a `mappend` c, b `mappend` d)

Tantangan: bisakah kalian membuat anggota `Monoid` untuk `Bool`? Ada berapa anggota
berbeda?

Tantangan: bagaimana kalian membuat tipe fungsi sebagai anggota `Monoid`?


 <!--

Local Variables:
mode:markdown
compile-command:"make explec"
End:

-->
