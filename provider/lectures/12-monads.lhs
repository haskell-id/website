
 <!-- CLASS

> import Control.Monad
> import Control.Applicative

-->

Monad
======

Bacaan tambahan:

  * [*Typeclassopedia*](http://www.haskell.org/haskellwiki/Typeclassopedia)
  * [LYAH Bab 12: *A Fistful of Monads*](http://learnyouahaskell.com/a-fistful-of-monads)
  * [LYAH Bab 9: *Input and Output*](http://learnyouahaskell.com/input-and-output)
  * [RWH Bab 7: *I/O*](http://book.realworldhaskell.org/read/io.html)
  * [RWH Bab 14: *Monads*](http://book.realworldhaskell.org/read/monads.html)
  * [RWH Bab 15: *Programming with monads*](http://book.realworldhaskell.org/read/programming-with-monads.html)

Motivasi
--------

Dalam beberapa minggu terakhir, kita telah melihat bagaimana `Applicative`
memungkinkan kita melakukan komputasi di dalam sebuah "konteks khusus".
Contohnya antara lain, mengatasi kemungkinan gagal dengan `Maybe`, hasil
yang lebih dari satu dengan `[]`, melihat semacam "lingkungan" menggunakan
`((->) e)`, atau membuat parser dengan pendekatan kombinator seperti di tugas.

Akan tetapi, sejauh ini kita hanya melihat komputasi dengan struktur tetap,
seperti menerapkan konstruktor data ke sejumlah argumen yang telah diketahui.
Bagaimana jika kita tidak tahu struktur komputasinya di awal? Bagaimana
jika kita ingin memutuskan apa yang ingin dilakukan berdasarkan hasil-hasil
sebelumnya?

Sebagai contoh, ingat tipe `Parser` dari tugas, dan anggap kita telah
menjadikannya anggota `Functor` dan `Applicative`:

> newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

~~~~ {.haskell}
instance Functor Parser where
  ...

instance Applicative Parser where
  ...
~~~~

Ingat bahwa nilai bertipe `Parser a` berarti sebuah parser yang bisa
menerima sebuah `String` sebagai input dan mungkin menghasilkan nilai
bertipe `a` beserta sisa `String` yang belum di-*parse*. Sebagai contoh,
parser integer yang diberikan input `String` sebagai berikut

    "143xkkj"

akan menghasilkan

    Just (143, "xkkj")

Seperti yang kalian lihat di tugas, kita sekarang bisa menulis seperti

~~~~ {.haskell}
data Foo = Bar Int Int Char

parseFoo :: Parser Foo
parseFoo = Bar <$> parseInt <*> parseInt <*> parseChar
~~~~

dengan asumsi kita memiliki fungsi `parseInt :: Parser Int` dan
`parseChar :: Parser Char`.  Anggota `Applicative` akan mengatasi kemungkinan
kegagalan (jika *parsing* salah satu komponen gagal, *parsing* seluruh
`Foo` akan gagal). Selain itu, dia juga akan melanjutkan ke bagian `String`
yang belum di-*parse*  untuk dijadikan input ke komponen selanjutnya.

Nah, sekarang kita akan parsing berkas yang mengandung deret angka sebagai
berikut:

    4 78 19 3 44 3 1 7 5 2 3 2

Dengan catatan, angka pertama menandakan panjang "grup" angka berikutnya.
Angka setelah grup tersebut adalah panjang grup berikutnya, dan seterusnya.
Jadi contoh di atas bisa dipecah menjadi grup-grup sebagai berikut:

    78 19 3 44   -- grup pertama
    1 7 5        -- grup kedua
    3 2          -- grup ketiga

Contoh ini memang terlihat aneh, tapi ada kasus nyata di mana format suatu
berkas mengikuti prinsip yang sama. Kita membaca semacam "*header*" yang
memberitahu panjang dari suatu blok, atau di mana kita bisa menemukan
sesuatu di dalam berkas, dan lain sebagainya.

Kita ingin menulis parser bertipe

~~~~ {.haskell}
parseFile :: Parser [[Int]]
~~~~

Sayangnya, ini tidak mungkin dengan `Applicative`. Masalahnya ialah `Applicative`
tidak memberikan kita cara untuk memutuskan apa yang akan dilakukan berdasarkan
hasil sebelumnya. Kita harus memutuskan di awal operasi parsing seperti apa yang
kita jalankan sebelum kita bisa melihat hasilnya.

Akan tetapi, tipe `Parser` *bisa* mendukung hal seperti ini. Hal ini diabstraksi
dengan *type class* `Monad`.

Monad
-----

*Type class* `Monad` didefinisikan sebagai berikut:

~~~~ {.haskell}
class Monad m where
  return :: a -> m a

  (>>=) :: m a -> (a -> m b) -> m b

  (>>)  :: m a -> m b -> m b
  m1 >> m2 = m1 >>= \_ -> m2
~~~~

Terlihat familiar! Kita telah melihat method-method ini di dalam konteks `IO`,
tapi sebenarnya mereka tidak spesifik hanya untuk `IO` saja.

`return` juga terlihat familiar karena bertipe sama dengan `pure`. Sebenarnya, tiap
`Monad` harusnya juga merupakan `Applicative`, dengan `pure = return`. Alasan kita
mempunyai keduanya ialah `Applicative` diciptakan *setelah* `Monad` ada cukup lama.

`(>>)` adalah versi khusus dari `(>>=)` (dimasukkan ke dalam `Monad` jikalau
ada anggota yang ingin menyediakan implementasi yang lebih efisien, meskipun
biasanya implementasi *default* sudah cukup). Jadi untuk memahami `(>>)`,
kita harus memahami `(>>=)` terlebih dahulu.

Sebenarnya ada *method* lagi bernama `fail`, tapi hal ini adalah sebuah kesalahan.
Kalian sebaiknya jangan pernah memakainya, jadi saya tak perlu menjelaskan (kalian
[bisa membacanya di * Typeclassopedia*](http://www.haskell.org/haskellwiki/Typeclassopedia#do_notation)
jika tertarik).

`(>>=)` (disebut "*bind*") adalah di mana semua aksi berada! Mari perhatikan
tipenya dengan seksama:

    (>>=) :: m a -> (a -> m b) -> m b

`(>>=)` menerima dua argumen. Yang pertama ialah nilai bertipe `m a`.
Nilai tersebut biasa disebut "nilai monadik" (*monadic values*), atau
"komputasi". Ada juga yang menyarankan untuk disebut *mobit*. Kalian
*tidak* boleh menyebutnya "monad", karena itu salah (konstruktor tipe `m`
lah yang merupakan monad.) Intinya, mobit bertipe `m a` melambangkan
sebuah komputasi yang menghasilkan sebuah (atau beberapa, atau tidak ada)
nilai bertipe `a`, dan juga memiliki semacam "efek":

  * `c1 :: Maybe a` adalah komputasi yang mungkin gagal, atau menghasilkan nilai
    bertipe `a` jika sukses.

  * `c2 :: [a]` adalah komputasi yang menghasilkan (beberapa) `a`.

  * `c3 :: Parser a` adalah komputasi yang mengkonsumsi bagian dari sebuah
    `String` dan (mungkin) menghasilkan sebuah `a`.

  * `c4 :: IO a` adalah komputasi yang mungkin memiliki efek *I/O* lalu
    menghasilkan sebuah `a`.

Dan lain sebagainya. Sekarang, bagaimana dengan argumen kedua dari `(>>=)`?
Sebuah fungsi bertipe `(a -> m b)` yang akan *memilih* komputasi berikutnya
berdasarkan hasil dari komputasi pertama. Inilah yang dimaksud dengan `Monad`
bisa merangkum beberapa komputasi yang bisa memilih apa yang akan dilakukan
tergantung dari hasil komputasi sebelumnya.

Jadi apa yang `(>>=)` lakukan ialah menggabungkan dua mobit menjadi satu. Mobit
hasilnya yang lebih besar ini akan menjalankan yang pertama lalu yang kedua, dan
mengembalikan hasil dari yang kedua. Hal penting yang perlu diingat ialah kita
bisa menentukan mobit kedua mana yang akan dijalankan berdasarkan hasil dari yang
pertama.

Implementasi *default* dari `(>>)` tentunya menjadi jelas sekarang:

    (>>)  :: m a -> m b -> m b
    m1 >> m2 = m1 >>= \_ -> m2

`m1 >> m2` menjalankan `m1` lalu `m2`, mengabaikan hasil dari `m1`.

Contoh
------

Mari mulai dengan membuat anggota `Monad` untuk `Maybe`:

~~~~ {.haskell}
instance Monad Maybe where
  return  = Just
  Nothing >>= _ = Nothing
  Just x  >>= k = k x
~~~~

`return`, tentunya, hanyalah `Just`. Jika argumen pertama dari `(>>=)` adalah
`Nothing`, maka seluruh komputasi akan gagal. Sebaliknya, jika `Just x`, kita
terapkan argumen kedua pada `x` untuk memutuskan apa yang akan dilakukan
berikutnya.


Kebetulan, cukup umum memakai huruf `k` untuk argumen kedua dari `(>>=)` karena
`k` merupakan singkatan dari "kontinuasi". *I wish I was joking*.

Beberapa contoh:

> check :: Int -> Maybe Int
> check n | n < 10    = Just n
>         | otherwise = Nothing
>
> halve :: Int -> Maybe Int
> halve n | even n    = Just $ n `div` 2
>         | otherwise = Nothing
>
> exM1 = return 7 >>= check >>= halve
> exM2 = return 12 >>= check >>= halve
> exM3 = return 12 >>= halve >>= check

Bagaimana anggota `Monad` untuk konstruktor list `[]`?

~~~~ {.haskell}
instance Monad [] where
  return x = [x]
  xs >>= k = concat (map k xs)
~~~~

Contoh sederhana:

> addOneOrTwo :: Int -> [Int]
> addOneOrTwo x = [x+1, x+2]
>
> exL1 = [10,20,30] >>= addOneOrTwo

*Monad combinator*
------------------

Satu hal bagus dari `Monad` ialah dengan hanya menggunakan `return` dan
`(>>=)` kita bisa membuat banyak kombinator umum untuk menulis program
dengan monad. Mari kita lihat beberapa contohnya.

Pertama, `sequence` menerima list berisi nilai-nilai monadik dan menghasilkan
satu nilai monadik yang merupakan gabungan dari semuanya. Artinya,
masing-masing monad memiliki sifat yang berbeda. Sebagai contoh, untuk `Maybe`
ini berarti seluruh komputasi sukses hanya jika semua komputasi yang membangunnya
sukses. Untuk kasus `IO`, ini berarti menjalankan komputasi secara berurutan.
Untuk kasus `Parser` ini berarti menjalankan semua parser terhadap bagian-bagian
input secara berurutan (dan sukses hanya jika semuanya sukses).

~~~~ {.haskell}
sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (ma:mas) =
  ma >>= \a ->
  sequence mas >>= \as ->
  return (a:as)
~~~~

Dengan menggunakan `sequence` kita juga bisa menulis kombinator lainnya seperti

~~~~ {.haskell}
replicateM :: Monad m => Int -> m a -> m [a]
replicateM n m = sequence (replicate n m)
~~~~

Dan akhirnya kita bisa menulis parser yang kita inginkan, hanya dengan

~~~~ {.haskell}
parseFile :: Parser [[Int]]
parseFile = many parseLine

parseLine :: Parser [Int]
parseLine = parseInt >>= \i -> replicateM i parseInt
~~~~

(`many` juga disebut sebagai `zeroOrMore` di dalam tugas).

 <!--

Local Variables:
mode:markdown
compile-command:"make explec"
End:

-->
