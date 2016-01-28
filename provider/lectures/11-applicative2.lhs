*Applicative functor*, Bagian II
================================

Bacaan tambahan:

  * [*Applicative Functor*](http://learnyouahaskell.com/functors-applicative-functors-and-monoids#applicative-functors) di *Learn You a Haskell*
  * [*The Typeclassopedia*](http://www.haskell.org/haskellwiki/Typeclassopedia)

Mari kita perhatikan kembali *type class* `Functor` dan `Applicative`:

> class Functor f where
>   fmap :: (a -> b) -> f a -> f b
>
> class Functor f => Applicative f where
>   pure  :: a -> f a
>   (<*>) :: f (a -> b) -> f a -> f b

Tiap `Applicative` juga merupakan `Functor`, bisakah kita implemen `fmap`
dalam bentuk `pure` dan `(<*>)`?  Mari kita coba!

> fmap g x = pure g <*> x

Setidaknya, tipenya cocok! Akan tetapi, bukan mustahil untuk membuat
anggota `Functor` and `Applicative` yang tidak cocok satu sama lain.
Karena hal ini menimbulkan keraguan, kita membuat perjanjian bahwa
anggota `Functor` dan `Applicative` untuk tipe apapun harus cocok
satu sama lain.

Sekarang, mari kita lihat beberapa contoh lain dari anggota `Applicative`.

Contoh-contoh *Applicative* Lain
--------------------------------

**Lists**

Bagaimana list menjadi anggota `Applicative`? Ada dua kemungkinan: satu
yang memasangkan elemen dari list fungsi ke list argumen secara berurutan
("zip"), dan satu lagi yang menggabungkan fungsi dan argumennya dengan
seluruh kemungkinan kombinasi.

Mari kita buat anggota yang melakukan seluruh kombinasi dahulu. (Dengan
alasan yang akan diketahui minggu depan, ini merupakan anggota *default*).
Dari sisi ini, list bisa dilihat sebagai hal yang "non-deterministik":
nilai bertipe `[a]` bisa diibaratkan dengan sebuah nilai dengan beberapa
kemungkinan. Jika demikian, maka `(<*>)` bisa disebut sebagai aplikasi
fungsi non-deterministik, yaitu aplikasi sebuah fungi yang non-deterministik
ke argumen yang non-deterministik.

> instance Applicative [] where
>   pure a        = [a]          -- a "deterministic" value
>   [] <*> _      = []
>   (f:fs) <*> as = (map f as) ++ (fs <*> as)

Ini contohnya:

> names  = ["Joe", "Sara", "Mae"]
> phones = ["555-5555", "123-456-7890", "555-4321"]
>
> employees1 = Employee <$> names <*> phones

Mungkin contoh ini tidak masuk akal, tapi tak sulit untuk membayangkan
situasi di mana kalian butuh menggabungkan hal dengan semua kemungkinan.
Misalnya, aritmatika non-deterministik sebagai berikut:

> (.+) = liftA2 (+)    -- penjumlahan diangkat ke konteks Applicative
> (.*) = liftA2 (*)    -- juga perkalian
>
> -- nondeterministic arithmetic
> n = ([4,5] .* pure 2) .+ [6,1] -- (4 atau 5) dikali 2, plus (6 atau 1)
>
> -- aritmatika yang mungkin gagal
> m1 = (Just 3 .+ Just 5) .* Just 8
> m2 = (Just 3 .+ Nothing) .* Just 8

Selanjutnya, kita akan menulis anggota yang melakukan pemasangan
sesuai urutan. Pertama, kita harus menjawab pertanyaan penting: bagaimana
mengatasi list dengan panjang yang berbeda? Hal yang paling masuk akal
ialah memotong list yang lebih panjang sehingga sama panjangnya dengan
yang lebih pendek, dan membuang elemen yang tidak diperlukan. Tentu
saja ada cara lainnya. Mungkin kita bisa memanjangkan list yang lebih
pendek dengan menyalin elemen terakhirnya (tapi bagaimana jika salah satu
list kosong?). Atau bisa juga memanjangkan list dengan elemen "netral"
(tapi tentu kita memerlukan anggota `Monoid`, atau sebuah argumen "*default*"
tambahan).

Keputusan ini mendikte bagaimana kita mengimplemen `pure`, karena kita harus
mematuhi hukum berikut

~~~~ {.haskell}
pure f <*> xs === f <$> xs
~~~~

Perhatikan bahwa list di sisi kanan sama panjangnya dengan yang di sisi kiri.
Satu-satunya cara kita bisa membuat sisi kiri sepanjang itu ialah dengan
`pure` yang menciptakan list `f` tak berhingga, karena kita tidak tahu `xs`
akan seberapa panjang.

Kita buat anggota dengan menggunakan pembungkus `newtype` untuk membedakannya
dengan anggota yang lain. Fungsi `zipWith` yang terdapat di *Prelude* juga
membantu kita.

> newtype ZipList a = ZipList { getZipList :: [a] }
>   deriving (Eq, Show, Functor)
>
> instance Applicative ZipList where
>   pure = ZipList . repeat
>   ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)

Sebagai contoh:

> employees2 = getZipList $ Employee <$> ZipList names <*> ZipList phones

***Reader/environment***

Contoh terakhir untuk `(->) e`.  Ini disebut *applicative* *reader* (pembaca)
atau *environment* (lingkungan), karena membuat kita bisa "membaca" dari
"lingkungan" `e`. Implementasi anggota ini tidak begitu sulit kalau kita
mengikuti tipe-tipenya:

> instance Functor ((->) e) where
>   fmap = (.)
>
> instance Applicative ((->) e) where
>   pure = const
>   f <*> x = \e -> (f e) (x e)

Contoh `Employee` (pegawai):

> data BigRecord = BR { getName         :: Name
>                     , getSSN          :: String
>                     , getSalary       :: Integer
>                     , getPhone        :: String
>                     , getLicensePlate :: String
>                     , getNumSickDays  :: Int
>                     }
>
> r = BR "Brent" "XXX-XX-XXX4" 600000000 "555-1234" "JGX-55T3" 2
>
> getEmp :: BigRecord -> Employee
> getEmp = Employee <$> getName <*> getPhone
>
> exQ = getEmp r

Tingkat Abstraksi
-----------------

`Functor` cukup berguna dan jelas. Pada awalnya, sepertinya `Applicative`
tidak begitu membawa perubahan berarti dari apa yang telah dimiliki `Functor`.
Akan tetapi, perubahan kecil ini memiliki efek yang besar. `Applicative`
(dan yang akan kita lihat minggu depan, `Monad`) bisa disebut sebagai
"model komputasi", sementara `Functor` tidak demikian.

Ketika bekerja dengan `Applicative` dan `Monad`, perlu selalu diingat
bahwa ada beberapa tingkat abstraksi. Kasarnya, abstraksi ialah sesuatu
yang menyembunyikan detil tingkat di bawahnya dan menyediakan antarmuka
yang bisa digunakan (idealnya) tanpa memikirkan tingkat di bawahnya tersebut,
meski terkadang ada detil dari tingkat bawah yang "bocor" di beberapa kasus.
Ide tentang beberapa tingkat abstraksi ini banyak digunakan. Contohnya,
program---OS---kernel---sirkuit terpadu (IC)---gerbang logika---*silicon*,
atau HTTP---TCP---IP---Ethernet, atau bahasa pemrograman---*bytecode*---
*assembly*---kode mesin.

Haskell memberi kita alat untuk membuat beberapa tingkat abstraksi *di dalam
program Haskell sendiri*. Dengan kata lain, kita bisa meng-*extend*
tingkat abstraksi dari bahasa pemrograman ke atas. Hal ini sangat kuat
dan berguna tapi juga bisa membingungkan. Kita harus belajar berpikir
di beberapa tingkat secara eksplisit, dan berpindah-pindah di antara
tingkat-tingkat tersebut.

Dalam `Applicative` dan `Monad`, terdapat dua tingkatan untuk dipahami.
Pertama ialah tingkat di mana implementasi anggota `Applicative` dan `Monad`.
Kalian mendapatkan pengalaman di tingkat ini di tugas sebelumnya, ketika
membuat `Parser` menjadi anggota `Applicative`.

Ketika `Parser` sudah menjadi angota `Applicative`, berikutnya kita
"naik satu tingkat" dan membuat program dengan `Parser` melalui
antarmuka `Applicative`, tanpa memikirkan bagaimana `Parser` dan
implementasi anggota `Applicative`nya dibuat. Kalian mendapatkan
pengalaman ini di tugas minggu lalu, dan akan mendapatkannya lagi
minggu ini. Pemrograman di tingkat ini memiliki "rasa" yang berbeda dengan
pengerjaan detil anggota. Mari kita lihat beberapa contoh.


*Applicative* API
-----------------

Salah satu keuntungan memiliki antarmuka seragam seperti `Applicative`
ialah kita bisa menulis perkakas umum dan struktur kontrol yang bisa bekerja
dengan *semua* anggota `Applicative`. Sebagai contoh, mari kita tulis

> pair :: Applicative f => f a -> f b -> f (a,b)

`pair` menerima dua nilai dan memasangkannya. Semua hal tersebut dilakukan
dalam konteks `Applicative f`. Percobaan pertama, ambil fungsi untuk
memasangkan, dan "angkat" melewati argumennya dengan menggunakan
`(<$>)` dan `(<*>)`:

> pair fa fb = (\x y -> (x,y)) <$> fa <*> fb

Ini sudah berfungsi, tapi masih bisa disederhanakan. Ingat di
Haskell kita bisa menggunakan sintaks untuk melambangkan konstruktor
*pair*, jadi kita bisa menuliskan

> pair fa fb = (,) <$> fa <*> fb

Sebenarnya kita sudah melihat pola ini sebelumnya. Ini adalah pola `liftA2`
yang membuat kita mulai belajar `Applicative`. Kita bisa menyederhanakannya
lebih jauh dengan

> pair fa fb = liftA2 (,) fa fb

Sekarang kita tidak perlu menuliskan argumen fungsi secara eksplisit, sehingga
kita bisa memperoleh versi final dari fungsi ini:

> pair = liftA2 (,)

Jadi, apa yang fungsi ini lakukan? Tergantung dari `f` yang diberikan. Mari
kita lihat beberapa contoh kasus:

  * `f = Maybe`: menghasilkan `Nothing` jika salah satu argumen berupa
    `Nothing`. Jika keduanya `Just`, hasilnya berupa `Just` dari pasangan
    tersebut.
  * `f = []`: menghasilkan produk Cartesian dari dua buah list.
  * `f = ZipList`: sama dengan fungsi `zip` standar.
  * `f = IO`: menjalankan dua `IO` berurutan, mengembalikan pasangan
    hasilnya.
  * `f = Parser`: menjalankan dua parser berurutan (parser-parser tersebut
    menerima input berurutan), mengembalikan hasil berupa pasangan. Jika
    satu gagal, semuanya gagal.

Bisakah kalian mengimplemen fungsi-fungsi berikut? Pertimbangkan apa yang tiap
fungsi lakukan jika `f` diganti dengan tipe-tipe di atas.

> (*>)       :: Applicative f => f a -> f b -> f b
> mapA       :: Applicative f => (a -> f b) -> ([a] -> f [b])
> sequenceA  :: Applicative f => [f a] -> f [a]
> replicateA :: Applicative f => Int -> f a -> f [a]

 <!--

Local Variables:
mode:markdown
compile-command:"mk pre"
End:

-->
