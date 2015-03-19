<!--
{-# OPTIONS_GHC -Wall #-}
-->

Evaluasi *lazy*
===============

Bacaan tambahan:

* [foldr foldl foldl'](http://haskell.org/haskellwiki/Foldr_Foldl_Foldl%27)
  dari Haskell wiki

Di hari pertama kelas, saya mengatakan bahwa Haskell bersifat *lazy*
(translasi: enggan, malas), dan berjanji akan menjelaskan lebih lanjut
apa artinya di lain waktu. Sekaranglah saatnya!


Evaluasi *strict*
-----------------

Sebelum membahas evaluasi *lazy* (literal: malas, enggan), kita akan
melihat beberapa contoh dari kebalikannya, evalusi *strict* (literal:
tegas, kaku)

Pada evaluasi *strict*, argumen fungsi akan terevaluasi *sebelum* diberikan
ke fungsi. Sebagai contoh, misalkan kita telah mendefinisikan

~~~~ {.haskell}
f x y = x + 2
~~~~

Di bahasa yang *strict*, `f 5 (29^35792)` pertama kali akan mengevaluasi
`5` (sudah selesai) dan `29^35792` (yang harus dikerjakan terlebih dulu)
sebelum memberikan hasil-hasil tersebut ke `f`.

Tentu dalam contoh tersebut hal ini terkesan bodoh, karena `f` mengabaikan
argumen kedua. Sehingga seluruh usaha untuk menghitung `29^35792` terbuang
percuma. Jadi kenapa kita memerlukannya?

Keuntungan dari evaluasi *strict* ialah kemudahan untuk menebak *kapan*
dan *urutan* sesuatu akan terjadi. Biasanya bahasa yang *strict*
menjelaskan urutan evaluasi argumen-argumen dari fungsi (dari kiri ke kanan
misalnya).

Misalkan kita menulis ini di Java

    f (release_monkeys(), increment_counter())
    
Kita tahu bahwa kera-kera akan dibebaskan, lalu *counter* akan bertambah,
dan hasil-hasil tersebut akan diberikan ke `f`. Tidak peduli apakah `f`
akan menggunakan hasil-hasil tersebut atau tidak.

Jika kedua argumen tersebut bisa dievaluasi secara independen atau tidak,
dengan urutan bebas, tergantung apakah `f` akan memakainya atau tidak,
maka ini akan menjadi sangat membingungkan. Ketika "efek samping" seperti
ini diperbolehkan, kita perlu evaluasi *strict*.


Efek samping dan *purity*
-------------------------

Jadi yang sebenarnya bermasalah adalah hadir tidaknya *efek samping*
(*side effects*). Yang dimaksud dengan efek samping ialah "apapun
yang dapat menyebabkan evaluasi sebuah ekspresi berinteraksi dengan
sesuatu di luar ekspresi itu sendiri". Akar permasalahanya adalah
interaksi dengan dunia luar tersebut sensitif terhadap waktu. Sebagai
contoh:

* Mengubah variabel global --- ini bermasalah karena bisa mempengaruhi
  evaluasi ekspresi-ekspresi lain
* Mencetak ke layar --- ini bermasalah karena bisa saja ini diperlukan
  dalam urutan tertentu terhadap ekspresi lainnya
* Membaca berkas atau jaringan --- ini bermasalah karena isi dari berkas
  bisa mempengaruhi hasil evaluasi ekspresi

Seperti yang kita lihat, evaluasi *lazy* membuat sulit untuk mengetahui
kapan evaluasi terjadi, sehingga menyertakan efek samping ke dalam
bahasa yang *lazy* menjadi sangat tidak intuitif. Secara sejarah, inilah
alasan mengapa Haskell *pure* (literal: murni). Pada awalnnya, perancang
Haskell berniat untuk membuat bahasa yang *lazy*, dan kemudian dengan
cepat menyadari bahwa hal tersebut mustahil jika tidak melarang efek
samping.

Tapi.. sebuah bahasa tanpa efek samping tidaklah begitu berguna. Kalian
hanya akan bisa me-*load* program di *interpreter* dan mengevaluasi
ekspresi-ekspresi. Kalian tidak akan bisa  mendapatkan masukan dari
pengguna, atau mencetak ke layar, atau membaca berkas. Tantangan
yang dihadapi para perancang Haskell adalah menyediakan cara untuk
mengizinkan efek-efek tersebut dengan terkontrol dan tidak mengganggu
*purity* dari bahasa itu sendiri. Akhirnya mereka berhasil menyediakannya
(bernama `IO` monad) yang akan kita bahas dalam beberapa minggu kemudian.

Evaluasi *lazy*
---------------

Setelah pembahasan evaluasi *strict*, sekarang mari kita lihat seperti
apa evaluasi *lazy*. Di dalam evaluasi *lazy*, evaluasi argumen-argumen
fungsi "ditunda selama mungkin". Argumen-argumen tersebut tidak akan
dievaluasi sampai benar-benar diperlukan. Ketika ekspresi diberikan
ke fungsi sebagai argumen, ekspresi tersebut disimpan sebagai ekspresi
yang belum dievaluasi (disebut "*thunk*", entah mengapa).

Sebagai contoh, ketika mengevaluasi `f 5 (29^35792)`, argumen kedua
akan disimpan sebagai *thunk* tanpa ada komputasi dan `f` akan langsung
dipanggil. Karena `f` tak pernah menggunakan argumen kedua, *thunk*
tersebut akan dibuang oleh *garbage collector*.


Evaluasi dengan pencocokkan pola
--------------------------------

Jadi kapankah kita perlu untuk mengevaluasi ekspresi? Contoh di atas
mengkhususkan pada penggunaan argumen pada sebuah fungsi, tetapi itu
bukanlah suatu perbedaan yang paling utama. Lihatlah contoh berikut:

> f1 :: Maybe a -> [Maybe a]
> f1 m = [m,m]
>
> f2 :: Maybe a -> [a]
> f2 Nothing  = []
> f2 (Just x) = [x]

`f1` dan `f2` menggunakan argumen-argumen tetapi ada perbedaan besar
di antara keduanya. Meskipun `f1` menggunakan argumen `m`, dia tidak
perlu tahu apapun tentang argumen tersebut. `m` bisa merupakan ekspresi
yang sepenuhnya tidak terevaluasi, dan letakkan ekspresi tersebut ke
dalam list. Dengan kata lain, hasil dari `f1 e` tidak bergantung pada
bentuk dari `e`.

Di sisi lain, `f2` perlu tahu sesuatu tentang argumennya untuk bekerja
lebih jauh. Apakah dikonstruksi dengan `Nothing` atau `Just`? Untuk
mengevaluasi `f2 e` kita perlu mengevaluasi `e` terlebih dahulu, karena
hasil dari `f2` bergantung pada bentuk dari `e`.

Satu hal lagi yang patut diingat ialah *thunks* hanya dievaluasi
**secukupnya**. Sebagai contoh, kita ingin mengevaluasi
`f2 (safeHead [3^500, 49])`. `f2` akan memaksa evaluasi
`safeHead [3^500, 49]`, yang akan menghasilkan `Just (3^500)`.
Perhatikan bahwa `3^500` tidak dievaluasi, karena `safeHead` tidak
memerlukannya, demikian juga dengan `f2`. Apakah `3^500` akan
dievaluasi nantinya bergantung pada penggunaan hasil dari `f2`.

Slogannnya ialah "*evaluasi dengan pencocokkan pola*". Hal-hal yang
perlu diingat:

* Ekspresi hanya dievaluasi ketika polanya cocok

* ...dan hanya seperlunya saja!

Mari kita lihat contoh yang lebih menarik: kita akan mengevaluasi
`take 3 (repeat 7)`. Sebagai referensi, berikut adalah definisi dari
`repeat` dan `take`:

~~~~ {.haskell}
repeat :: a -> [a]
repeat x = x : repeat x

take :: Int -> [a] -> [a]
take n _      | n <= 0 =  []
take _ []              =  []
take n (x:xs)          =  x : take (n-1) xs
~~~~

Evaluasi secara bertahap akan berjalan seperti ini:

      take 3 (repeat 7)
          { 3 <= 0 bernilai False, jadi kita lanjut ke klausa kedua, yang
	    perlu mencocokkan dengan argumen kedua. Kita perlu ekspansi
	    repeat 7 satu kali. }
    = take 3 (7 : repeat 7)
          { klausa kedia tidak cocok tapi klausa ketiga cocok. Perhatikan
            bahwa (3-1) belum terevaluasi! }
    = 7 : take (3-1) (repeat 7)
          { Untuk memutuskan pada klausa pertama, kita perlu mengecek (3-1)
            <= 0 yang memerlukan evaluasi (3-1). }
    = 7 : take 2 (repeat 7)
          { 2 <= 0 bernilai False, jadi kita perlu ekspansi repeat 7 lagi. }
    = 7 : take 2 (7 : repeat 7)
          { Sisanya serupa. }
    = 7 : 7 : take (2-1) (repeat 7)
    = 7 : 7 : take 1 (repeat 7)
    = 7 : 7 : take 1 (7 : repeat 7)
    = 7 : 7 : 7 : take (1-1) (repeat 7)
    = 7 : 7 : 7 : take 0 (repeat 7)
    = 7 : 7 : 7 : []

Catatan: Meski evaluasi bisa diimplementasikan seperti di atas, kebanyakan
*compiler* Haskell melakukan hal lebih kompleks. Contohnya, GHC menggunakan
teknik *graph reduction*, di mana ekspresi yang dievaluasi
direpresentasikan dalam bentuk *graph* sehingga sub-ekspresi bisa berbagi
*pointer* ke sub-ekspresi yang sama. Hal ini menjamin tak ada duplikasi
pengerjaan. Sebagai contoh, jika `f x = [x,x]`, evaluasi `f (1+1)`
hanya akan melakukan sekali penjumlahan karena sub-ekspresi `1+1` akan
hanya ada satu, terbagi untuk dua buah `x`.

Konsekuensi
-----------

*Laziness* memiliki konsekuensi yang menarik, mengganggu, dan tersembunyi.
Mari kita lihat beberapa di antaranya.

**Purity** (Kemurnian)

Seperti yang telah kita lihat, pemilihan evaluasi *lazy* *memaksa* kita
juga untuk memilih *purity* (dengan asumsi kita tak mau menyyulitkan
programer lain).

**Memahami penggunaan ruang**

*Laziness* tidak selalu memberikan keuntungan. Salah satu kekurangannya ialah
terkadang kita akan kesulitan untuk mengetahui penggunaan ruang (*space*) di
program. Perhatikan contoh berikut (yang terlihat biasa saja):

~~~~ {.haskell}
-- Fungsi di standard library foldl, disediakan sebagai referensi
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ z []     = z
foldl f z (x:xs) = foldl f (f z x) xs
~~~~

Mari kita lihat bagaimana evaluasi berjalan ketika kita mengevaluasi
`foldl (+) 0 [1,2,3]` (yang akan menjumlahkan semua bilangan di list):

      foldl (+) 0 [1,2,3]
    = foldl (+) (0+1) [2,3]
    = foldl (+) ((0+1)+2) [3]
    = foldl (+) (((0+1)+2)+3) []
    = (((0+1)+2)+3)
    = ((1+2)+3)
    = (3+3)
    = 6

Karena nilai dari akumulator tidak diperlukan hingga selesai rekursi
seluruh list, akumulator membangun dirinya tiap langkah menjadi satu
ekspresi besar yang belum terevaluasi `(((0+1)+2)+3)`. Pada akhirnya,
ekspresi tersebut tereduksi menjadi sebuah nilai. Setidaknya, ada dua
masalah dengan hal ini. Pertama, tidak efisien. Sebenarnya tidak perlu
untuk memindahkan semua bilangan dari list ke sesuatu yang menyerupai
list (*thunk akumulator*) sebelum menjumlahkan semuanya. Masalah kedua
lebih tersembunyi. Evaluasi `(((0+1)+2)+3)` membutuhkan `3` dan `2`
untuk dimasukkan ke `stack` terlebih dahulu sebelum menghitung `0+1`.
Hal ini tidak masalah untuk contoh sederhana, tapi bermasalah untuk
list yang sangat besar karena mungkin saja tidak ada ruang yang cukup
untuk `stack` sehingga menyebabkan `stack overflow`.

Solusinya adalah dengan menggunakan fungsi `foldl'`, bukan `foldl`.
`foldl'` memerlukan argumen kedua (akumulator) untuk dievaluasi sebelum
melanjutkan, sehingga *thunk* tidak bertambah besar.

      foldl' (+) 0 [1,2,3]
    = foldl' (+) (0+1) [2,3]
    = foldl' (+) 1 [2,3]
    = foldl' (+) (1+2) [3]
    = foldl' (+) 3 [3]
    = foldl' (+) (3+3) []
    = foldl' (+) 6 []
    = 6

Terlihat bahwa `foldl'` melakukan penjumlahan secara langsung. Terkadang
*laziness* menimbulkan masalah. Jika demikian kita harus membuat
program kita menjadi kurang *lazy*.

Jika tertarik mempelajari bagaimana `foldl'` melakukannya, kalian 
[membaca tentang `seq` di Haskell wiki](http://www.haskell.org/haskellwiki/Seq).

***Short-circuiting operators***

Di beberapa bahasa (Java, C++) operator *boolean* `&&` dan `||`
(AND dan OR) bersifat *short-circuiting*. Contohnya, jika evaluasi
argumen pertama dari `&&` bernilai `False`, maka seluruh ekspresi akan
langsung bernilai `False` tanpa menyentuh argumen kedua. Akan tetapi,
perilaku ini harus diatur dalam standar Java dan C++ sebagai kasus khusus.
Biasanya, di bahasa yang *strict*, kedua argumen akan dievaluasi
sebelum pemanggilan fungsi. Jadi sifat *short-circuiting* dari `&&` dan
`||` merupakan pengecualian dari semantik bahasa yang *strict*.

Di Haskell kita bisa mendefinisikan operator yang *short-circuiting* tanpa
perlakuan istimewa. Bahkan sebenarnya `(&&)` dan `(||)` hanyalah fungsi
pustaka biasa. Berikut definisi dari `(&&)`:

~~~~ {.haskell}
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False
~~~~

Perhatikan bahwa definisi dari `(&&)` tidak mencocokkan pola pada
argumen kedua. Terlebih lagi jika argumen pertama bernilai `False`, argumen
kedua akan diabaikan. Karena `(&&)` sama sekali tidak mencocokkan pola
pada argumen kedua, maka fungsi ini juga bersifat *short-circuiting* sama
seperti operator `&&` di Java atau C++.

Perhatikan bahwa `(&&)` bisa juga didefinisikan seperti ini:

> (&&!) :: Bool -> Bool -> Bool
> True  &&! True  = True
> True  &&! False = False
> False &&! True  = False
> False &&! False = False

Meskipun versi tersebut menerima dan menghasilkan nilai yang sama
seperti sebelumnya, terdapat perbedaan perilaku. sebagai contoh:

~~~~ {.haskell}
False &&  (34^9784346 > 34987345)
False &&! (34^9784346 > 34987345)
~~~~

Keduanya akan terevaluasi menjadi `False`, tapi yang kedua akan
menghabiskan waktu lebih lama. Bagaimana dengan yang berikut ini?

~~~~ {.haskell}
False &&  (head [] == 'x')
False &&! (head [] == 'x')
~~~~

Yang pertama akan bernilai `False` lagi, sedangkan yang kedua akan
*crash*. Cobalah!

Semua ini menunjukkan bahwa ada beberapa isu menarik berkaitan dengan
*lazyness* yang perlu diperhatikan ketika mendefinisikan sebuah fungsi.

**Struktur kontrol buatan pengguna**

Dengan mengembangkan ide operator *short-circuiting* lebih jauh, kita bisa
membuat *struktur kontrol* kita sendiri di Haskell.

Sebagian besar bahasa pemrograman memiliki konstruk `if` bawaan. Ini
sedikit mirip dengan operator *Boolean* yang *short-circuiting*. Berdasarkan
nilai yang dites, `if` hanya akan mengeksekusi atau mengevaluasi salah satu
dari dua cabang.

Di Haskell kita bisa mendefinisikan `if` sebagai sebuah fungsi pustaka!

> if' :: Bool -> a -> a -> a
> if' True  x _ = x
> if' False _ y = y

Tentu Haskell memiliki ekspresi `if` bawaan tapi saya tak pernah mengerti
mengapa. Mungkin para desainer Haskell beranggapan orang-orang akan
mengharapkannya. "Apa? Bahasa ini tidak punya *if*!?" Lagipula, `if` jarang
digunakan di Haskell. Kebanyakan kita lebih suka menggunakan pencocokkan
pola atau *guards*.

Kita juga bisa mendefinisikan struktur kontrol lain. Kita akan lihat
contohnya nanti ketika membahas monad.

**Struktur data tak terhingga**

Evaluasi *lazy* memungkinkan kita untuk memiliki struktur data tak terhingga.
Kita telah melihat beberapa contohnya, seperti `repeat 7` yang merupakan
list berisikan `7` sebanyak tak terhingga. Mendefinisikan struktur data tak
terhingga sebenarnya hanyalah menciptakan `thunk`, yang bisa kita ibaratkan
sebagai "benih" di mana seluruh struktur data tersebut bisa tumbuh sesuai
kebutuhan.

Contoh praktis lainnya ialah data yang *terlalu besar*, seperti *tree* yang
merepresentasikan *state* dari sebuah *game* (seperti catur atau go). Meski
*tree* tersebut terbatas secara teori, tapi bisa menjadi terlalu besar untuk
memori. Dengan Haskell, kita bisa mendefinisikan *tree* untuk semua kemungkinan
langkah di *game*, lalu membuat algoritma terpisah untuk menjelajahi *tree*
tersebut sesuai kemauan. Hanya bagian *tree* yang terjelajah yang akan
disediakan.

**Pemrograman *pipelining/wholemeal***

Seperti yang telah disebutkan sebelumnya, melakukan transformasi bertahap
(*pipelined*) terhadap struktur data yang besar bisa terjadi secara efisien
dalam penggunaan memori. Sekarang kita bis atahu mengapa: karena *lazy*,
tiap tahap dalam operasi terjadi sesuai kebutuhan. Hasil hanya diterima
hanya seperlunya saja jika dibutuhkan oleh proses selanjutnya dalam *pipeline*.

**Pemrograman dinamis**

Satu contoh menarik hasil dari evaluasi *lazy* ialah pemrograman dinamis
([*dynamic programming*](http://en.wikipedia.org/wiki/Dynamic_programming)).
Biasannya kita harus sangat berhati-hati dalam mengisi tabel pemrograman
dinamis sesuai urutan, sehingga tiap kali kita menghitung nilai dari sebuah
sel, dependensinya sudah terhitung. Jika urutannya salah, hasil yang didapat
juga salah.

Dengan evaluasi *lazy* di Haskell, kita bisa menggunakan Haskell *runtime*
untuk mengerjakan pengurutan evaluasi tersebut! Sebagai contoh, berikut kode
Haskell untuk memecahkan 
[0-1 knapsack problem](http://en.wikipedia.org/wiki/Knapsack_problem).
Perhatikan bagaimana kita mendefinisikan *array* `m` dalam bentuk dirinya
sendiri (rekursif), dan membiarkan evaluasi *lazy* mencari urutan yang benar
untuk menghitung sel-selnya.

~~~~ {.haskell}
import Data.Array

knapsack01 :: [Double]   -- values 
           -> [Integer]  -- nonnegative weights
           -> Integer    -- knapsack size
           -> Double     -- max possible value
knapsack01 vs ws maxW = m!(numItems-1, maxW)
  where numItems = length vs
        m = array ((-1,0), (numItems-1, maxW)) $
              [((-1,w), 0) | w <- [0 .. maxW]] ++
              [((i,0), 0) | i <- [0 .. numItems-1]] ++
              [((i,w), best) 
                  | i <- [0 .. numItems-1]
                  , w <- [1 .. maxW]
                  , let best
                          | ws!!i > w  = m!(i-1, w)
                          | otherwise = max (m!(i-1, w)) 
                                            (m!(i-1, w - ws!!i) + vs!!i)
              ]

example = knapsack01 [3,4,5,8,10] [2,3,4,5,9] 20
~~~~
