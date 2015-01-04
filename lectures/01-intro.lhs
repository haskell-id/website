<!--
{-# OPTIONS_GHC -Wall #-}
-->

Dasar Haskell
=============

Bacaan tambahan:

-   [*Learn You a Haskell for Great Good*, bab 2](http://learnyouahaskell.com/starting-out)
-   [*Real World Haskell*](http://book.realworldhaskell.org/),
    bab 1 dan 2


Apa itu Haskell?
----------------

Haskell adalah bahasa pemrograman yang *lazy* dan fungsional yang
diciptakan pada akhir tahun 80-an oleh komite akademis. Pada saat itu,
ada banyak bahasa pemrograman fungsional berseliweran dan setiap orang punya
favoritnya sendiri-sendiri sehingga mempersulit pertukaran ide.
Sekelompok orang akhirnya berkumpul bersama dan mendesain bahasa baru
dengan mengambil beberapa ide terbaik dari bahasa yang sudah ada (dan menambah
beberapa ide baru milik mereka sendiri). Lahirlah Haskell.

Jadi, seperti apa Haskell? Haskell itu:

**Fungsional**

Tidak ada pengertian tepat dan baku untuk istilah "fungsional". Tapi ketika kita
mengatakan bahwa Haskell adalah bahasa pemrograman fungsional, kita biasanya
mengingat dua hal ini:

* Fungsi-nya *first-class*, yakni fungsi adalah nilai yang bisa
  digunakan layaknya nilai-nilai yang lain.

* Program Haskell lebih bermakna *mengevaluasi ekspresi* ketimbang
  *mengeksekusi instruksi*.

Perpaduan keduanya menghasilkan cara berfikir pemrograman yang sepenuhnya berbeda.
Kebanyakan waktu kita di semester ini akan dihabiskan mengeksplorasi cara berpikir ini.

***Pure***

Ekspresi di Haskell selalu *referentially transparent*, yakni:

* Tanpa mutasi! Semuanya (variable, struktur data…) immutable

* Ekspresi tidak memiliki "efek samping" (seperti memperbarui variabel
  global atau mencetak ke layar).

* Memanggil fungsi yang sama dengan argumen yang sama selali menghasilkan output
  yang sama setiap waktu.

Hal ini mungkin terdengar gila. Bagaimana mungkin bisa mengerjakan sesuatu
tanpa mutasi dan efek samping? Tentunya ini memerlukan perubahan cara berpikir 
(jika kalian terbiasa dengan paradigma pemrograman berbasis objek).
Tapi setelah kalian bisa berubah, akan ada beberapa keuntungan menakjubkan:

* *Equational reasoning* dan *refactoring*. Di Haskell kita bisa "mengganti
  equals dengan equals", seperti yang kita pelajari di aljabar.

* *Parallelism*. Mengevaluasi ekspresi secara paralel amatlah mudah ketika
  mereka dijamin tidak mempengaruhi yang lain.

* Lebih sedikit sakit kepala. Sederhananya, "efek tanpa batas" dan "aksi di 
  kejauhan" membuat program sulit di-debug, di-maintain, dan dianalisa.

***Lazy***

Di Haskell, ekspresi tidak akan dievaluasi sampai hasilnya benar-benar
dibutuhkan. Hal ini adalah keputusan sederhana dengan konsekuensi yang
merambat kemana-mana, yang akan kita eksplorasi sepanjang semester ini.
Beberapa konsekuensinya antara lain:

* Mendefinisikan *control structure* baru lewat pendefinisian fungsi menjadi mudah.

* Memungkinkan definisi dan pengerjaan dengan struktur data tak hingga.

* Mengakibatkan model pemrograman yang lebih komposisional (lihat *wholemeal
  programming* di bawah).

* Salah satu akibat negatif utamanya adalah analisa terhadap
  penggunaan ruang dan waktu menjadi lebih rumit.

***Statically typed***

Setiap ekspresi di Haskell memiliki tipe, dan tipe-tipe tersebut semuanya
diperiksa pada waktu kompilasi. Program dengan kesalahan tipe tidak akan
dikompilasi, apalagi dijalankan.

Tema
----

Selama kuliah ini, kita akan fokus pada tiga tema utama.

**Tipe**

*Static type system* bisa terlihat mengganggu. Faktanya, di bahasa
seperti C++ dan Java, mereka memang mengganggu. Tapi bukan *static type
system*-nya yang mengganggu, melainkan *type system* di C++ dan Java
yang kurang ekspresif! Semester ini kita akan melihat lebih dekat pada
*type system* di Haskell yang:


* *Membantu mengklarifikasi pemikiran dan ekspresi struktur program*

  Langkah pertama dalam menulis program Haskell biasanya adalah dengan
  menulis semua tipenya. Karena *type system* Haskell sangat ekspresif,
  langkah desain non-trivial ini akan sangat membantu dalam mengklarifikasi
  pemikiran seseorang tentang programnya.


* *Menjadi salah satu bentuk dokumentasi*

  Dengan type system yang ekspresif, hanya dengan melihat tipe pada suatu
  fungsi mampu memberitahu kalian tentang apa yang mungkin dikerjakan fungsi
  tersebut dan bagaimana ia bisa digunakan, bahkan sebelum kalian membaca
  dokumentasinya satu kata pun.


* Mengubah *run-time errors* menjadi *compile-time errors*

  Jauh lebih baik jika kita bisa memperbaiki kesalahan di depan
  daripada harus menguji sebanyak mungkin dan berharap yang terbaik.
  "Jika program ini berhasil di-compile, maka program tersebut pasti benar"
  sering dianggap candaan (karena masih mungkin untuk memiliki kesalahan
  di logika meskipun programnya *type-correct*), tetapi hal tersebut
  sering terjadi di Haskell ketimbang bahasa lain.


**Abstraksi**

"Don’t Repeat Yourself" adalah mantra yang sering didengar di dunia pemrograman.
Juga dikenal sebagai "Prinsip Abstraksi", idenya adalah tidak ada yang perlu diduplikasi:
setiap ide, algoritma, dan potongan data harus muncul tepat satu kali di kode kalian.
Mengambil potongan kode yang mirip dan memfaktorkan kesamaannya sering disebut sebagai proses abstraksi.

Haskell sangatlah bagus dalam abstraksi: fitur seperti *parametric
polymorphism*, fungsi *higher-order*, dan *type class* semuanya membantu
melawan pengulangan yang tak perlu. Perjalanan kita dalam semester ini
sebagian besar akan merupakan perjalanan dari yang spesifik menuju ke yang abstrak

***Wholemeal programming***

Satu lagi tema yang akan kita eksplorasi ialah *wholemeal programming*.
Berikut adalah sebuah kuotasi dari Ralf Hinze:

 > "Bahasa pemrograman fungsional unggul di *wholemeal programming*,
istilah yang diciptakan oleh Geraint Jones. *Wholemeal programming* berarti
berpikir besar dan menyeluruh. Bekerja dengan seluruh list secara utuh
ketimbang barisan elemen-elemennya; mengembangkan ruang solusi ketimbang
solusi individual; membayangkan sebuah *graph* ketimbang *path* tunggal.
Pendekatan *wholemeal* seringkali menawarkan perspektif baru terhadap
masalah yang diberikan. Hal ini juga dengan sempurna dilengkapi dengan ide
dari pemrograman proyektif: pertama selesaikan masalah yang lebih umum,
lalu ekstrak bagian dan potongan yang menarik dengan mentransformasikan
masalah umum tadi ke yang masalah yang lebih spesifik."

Sebagai contoh, perhatikan *pseudocode* berikut ini di bahasa C/Java-ish:

    int acc = 0;
    for ( int i = 0; i < lst.length; i++ ) {
      acc = acc + 3 * lst[i];
    }

Kode ini menderita apa yang dikatakan Richard Bird dengan istilah "indexities",
yakni kita harus khawatir terhadap detail *low-level* dari iterasi array dengan
tetap mencatat indeks saat ini. Kode tersebut juga menggabungkan apa yang
baiknya dipisahkan sebagai dua operasi berbeda: mengalikan setiap item dengan 3,
dan menjumlahkan semua hasilnya.

Di Haskell, kita cukup menuliskan

    sum (map (3*) lst)

Semester ini kita akan mengeksplorasi pergeseran cara berpikir dengan
cara pemrograman seperti ini, dan mememeriksa bagaimana dan mengapa
Haskell membuatnya menjadi mungkin.

*Literate Haskell*
------------------

File ini adalah "dokumen *literate* Haskell". Baris yang diawali dengan
\> dan spasi (lihat dibawah) merupakan kode. Lainnya (seperti paragraf ini)
adalah komentar. Tugas pemrograman kalian tidak harus berupa *literate* haskell,
meskipun diperbolehkan jika kalian mau. Dokumen *literate* Haskell berekstensi
.lhs, sedangkan kode sumber *non-literate* Haskell berekstensi .hs.

Deklarasi dan variabel
----------------------

Berikut ini adalah kode Haskell:

> x :: Int
> x = 3
> 
> -- Perhatikan bahwa komentar (non-literate) normal diawali dengan dua tanda strip
> {- atau diapit dalam pasangan
>    kurung kurawal/strip. -}

Kode diatas mendeklarasikan variabel `x` dengan tipe `Int` (:: diucapkan "memiliki tipe")
dan mendeklarasikan nilai `x` menjadi `3`. Perhatikan bahwa nilai ini akan menjadi nilai
`x` selamanya (paling tidak dalam program kita saja). Nilai dari `x` tidak akan bisa diganti kemudian.


Coba *uncomment* baris dibawah ini; kalian akan mendapati kesalahan
yang berbunyi ``Multiple declarations of `x'``.
    
> -- x = 4

Di Haskell, variabel bukanlah kotak mutable yang bisa diubah-ubah;
mereka hanyalah nama untuk suatu nilai.

Dengan kata lain, `=` tidak menyatakan "assignment" seperti di bahasa lain.
Alih-alih, `=` menyatakan definisi seperti di matematika. `x = 4`
tidak seharusnya dibaca "`x` memperoleh `4`" atau "*assign* `4` ke `x`",
tetapi harus dibaca "`x` *didefinisikan sebagai* `4`".

Menurut kalian apa arti dari kode berikut?

> y :: Int
> y = y + 1

Basic Types
-----------

> -- Machine-sized integers
> i :: Int
> i = -78

`Int` dijamin oleh standar bahasa Haskell untuk mengakomodasi nilai
paling tidak sebesar \\(\\pm 2^{29}\\), tapi ukuran pastinya bergantung
pada arsitektur kalian. Sebagai contoh, di mesin 64-bit saya kisarannya
sampai 2^63. Kalian bisa mencari tahu kisarannya dengan mengevaluasi kode
dibawah ini:

> intTerbesar, intTerkecil :: Int
> intTerbesar  = maxBound
> intTerkecil = minBound

(Perhatikan bahwa Haskell idiomatik mengunakan camelCase untuk nama
*identifier*. Jika kalian tidak menyukainya, terimalah saja.)

Di sisi lain, tipe Integer hanya dibatasi oleh kapasitas memori di mesin kalian.

> -- Arbitrary-precision integers
> n :: Integer
> n = 1234567890987654321987340982334987349872349874534
>
> sangatBesar :: Integer
> sangatBesar = 2^(2^(2^(2^2)))
>
> banyaknyaDigit :: Int
> banyaknyaDigit = length (show sangatBesar)

Untuk angka *floating-point*, ada `Double`:

> -- Double-precision floating point
> d1, d2 :: Double
> d1 = 4.5387
> d2 = 6.2831e-4

Ada juga tipe angka *single-precision floating point*, `Float`.

Akhirnya, kita juga punya *boolean*, karakter, dan string:

> -- Booleans
> b1, b2 :: Bool
> b1 = True
> b2 = False
> 
> -- Karakter unicode
> c1, c2, c3 :: Char
> c1 = 'x'
> c2 = 'Ø'
> c3 = 'ダ'
> 
> -- String adalah list dari karakter dengan sintaks khusus
> s :: String
> s = "Hai, Haskell!"

GHCi
----

GHCi adalah sebuah REPL (*Read-Eval-Print-Loop*) Haskell interaktif
yang satu paket dengan GHC. Di *prompt* GHCi, kalian bisa mengevaluasi
ekspresi, memuat berkas Haskell dengan `:load` (`:l`) (dan memuat ulang
mereka dengan `:reload` (`:r`)), menanyakan tipe dari suatu ekspresi
dengan `:type` (`:t`), dan banyak hal lainnya (coba `:?` untuk melihat
perintah-perintahnya).

Aritmatika
----------

Coba evaluasi ekspresi-ekspresi ini di GHCi:

> ex01 = 3 + 2
> ex02 = 19 - 27
> ex03 = 2.35 * 8.6
> ex04 = 8.7 / 3.1
> ex05 = mod 19 3
> ex06 = 19 `mod` 3
> ex07 = 7 ^ 222
> ex08 = (-3) * (-7)

Perhatikan bagaimana \`backticks\` membuat fungsi menjadi operator *infix*.
Perhatikan juga angka negatif seringkali harus diapit tanda kurung,
untuk mencegah tanda negasi di-*parse* sebagai operasi pengurangan.
(Ya, ini terlihat jelek. Mohon maaf.)

Sedangkan berikut ini akan menghasilkan *error*:

> -- badArith1 = i + n

Penjumlahan hanya berlaku untuk penjumlahan nilai bertipe numerik sama,
dan Haskell tidak melakukan perubahan secara implisit. Kalian harus
secara eksplisit mengubahnya dengan:

-   `fromIntegral`: mengubah dari tipe integral apapun (`Int` atau
    `Integer`) ke tipe numerik lainnya.

-   `round`, `floor`, `ceiling`: mengubah angka *floating-point* ke
    `Int` atau `Integer`.


Sekarang coba ini:

> -- badArith2 = i / i

Ini juga menghasilkan *error* karena `/` melakukan pembagian hanya
untuk angka *floating-point*. Untuk pembagian integer kita menggunakan
`div`.

> ex09 = i `div` i
> ex10 = 12 `div` 5

Jika kalian terbiasa dengan bahasa lain yang melakukan perubahan tipe
numerik secara implisit, ini terkesan sangat mengganggu pada awalnya.
Akan tetapi, saya jamin kalian akan terbiasa, dan bahkan pada akhirnya
akan menghargainya. Perubahan numerik secara implisit membuat pemikiran
kita tentang kode numerik menjadi tidak rapi.

Logika *boolean*
----------------

Seperti yang kalian duga, nilai *Boolean* bisa digabung satu sama lain
dengan `(&&)` (*logical and*), `(||)` (*logical or*), dan `not`.
Sebagai contoh,

> ex11 = True && False
> ex12 = not (False || True)

Nilai juga bisa dibandingkan kesamaannya satu sama lain dengan
`(==)` dan `(/=)`, atau dibandingkan urutannya dengan menggunakan
`(<)`, `(>)`, `(<=)`, dan `(>=)`.

> ex13 = ('a' == 'a')
> ex14 = (16 /= 3)
> ex15 = (5 > 3) && ('p' <= 'q')
> ex16 = "Haskell" > "C++"

Haskell juga memiliki ekspresi `if`: `if b then t else f` adalah sebuah
ekspresi yang mengevaluasi `t` jika ekspresi *boolean* `b` bernilai
`True`, dan `f` jika `b` bernilai `False`. Perhatikan bahwa ekspresi `if`
berbeda dengan *statement* `if`. Di dalam *statement* `if` bagian `else` adalah
opsional, jika tidak ada maka berarti "jika tes bernilai `False`, jangan
lakukan apapun". Sedangkan pada ekspresi `if`, bagian `else` wajib ada karena
ekspresi `if` harus menghasilkan sebuah nilai.

Haskell yang idiomatik tidak banyak menggunakan ekspresi `if`, kebanyakan
menggunakan *pattern-matching* atau *guard* (lihat bagian berikut).

Mendefinisikan fungsi
---------------------

Kita bisa menulis fungsi untuk integer secara per kasus.

> -- Jumlahkan integer dari 1 sampai n.
> sumtorial :: Integer -> Integer
> sumtorial 0 = 0
> sumtorial n = n + sumtorial (n-1)

Perhatikan sintaks untuk tipe fungsi: `sumtorial :: Integer ->
Integer` yang berarti `sumtorial` adalah sebuah fungsi yang
menerima sebuah `Integer` sebagai input dan menghasilkan `Integer`
sebagai output.

Tiap klausa dicek berurutan dari atas ke bawah dan yang pertama kali
cocok akan digunakan. Sebagai contoh, evaluasi `sumtorial 0` akan
menghasilkan `0`, karena cocok dengan klausa pertama. `sumtorial 3`
tidak cocok dengan klausa pertama (`3` tidak sama dengan `0`) sehingga
klausa kedua dicoba. Sebuah variabel seperti `n` cocok dengan apapun
sehingga klausa kedua cocok dan `sumtorial 3` dievaluasi menjadi
`3 + sumtorial (3 -1)` (yang juga bisa dievaluasi lebih lanjut).

Pilihan juga bisa dibuat dengan ekspresi Boolean menggunakan *guards*.
Contoh:

> hailstone :: Integer -> Integer
> hailstone n
>   | n `mod` 2 == 0 = n `div` 2
>   | otherwise      = 3*n + 1

Tiap *guard* yang berupa ekspresi Boolean bisa diasosiasikan dengan klausa
di definisi fungsi. Jika pola klausa cocok, *guards* akan dievaluasi
berurutan dari atas ke bawah dan yang pertama dievaluasi `True` akan dipilih.
Jika tidak ada yang `True`, pencocokan akan dilanjutkan ke klausa berikutnya.

Sebagai contoh, berikut adalah evaluasi `hailstone 3`. `3` dicocokkan dengan `n`
dan cocok (karena variabel cocok dengan apapun). Lalu, ``n `mod` 2`` dievaluasi,
hasilnya `False` karena `n = 3`  tidak menghasilkan sisa `0` ketika dibagi
`2`. `otherwise` hanyalah sinonim untuk `True`, sehingga *guard* kedua dipilih
dan hasil dari `hailstone 3` ialah `3*3 + 1 = 10`.

Contoh yang lebih rumit:

> foo :: Integer -> Integer
> foo 0 = 16
> foo 1 
>   | "Haskell" > "C++" = 3
>   | otherwise         = 4
> foo n
>   | n < 0            = 0
>   | n `mod` 17 == 2  = -43
>   | otherwise        = n + 3

Apa hasil dari `foo (-3)`? `foo 0`? `foo 1`? `foo 36`? `foo 38`?

Misalkan kita ingin membawa test genapnya bilangan keluar dari definisi
`hailstone`, berikut adalah contohnya:

> isEven :: Integer -> Bool
> isEven n 
>   | n `mod` 2 == 0 = True
>   | otherwise      = False

Seperti ini juga bisa, tapi lebih rumit. Terlihat jelas kan?

*Pairs*
-------

Kita bisa membuat hal berpasangan seperti berikut:

> p :: (Int, Char)
> p = (3, 'x')

Perhatikan bahwa notasi `(x,y)` digunakan untuk **tipe** dari *pair* dan **nilai**
dari *pair*.

Elemen dari sebuah *pair* bisa diekstrak dengan mencocokkan pola (*pattern matching*):

> sumPair :: (Int,Int) -> Int
> sumPair (x,y) = x + y

Haskell juga memiliki *triple*, *quadruple*, tapi sebaiknya jangan kalian
gunakan. Akan kita lihat minggu depan, ada cara lebih baik untuk menyatukan
tiga atau lebih informasi.

Menggunakan fungsi dengan beberapa argumen
------------------------------------------

Untuk aplikasi fungsi ke beberapa argumen, cukup letakkan argumen-argumen
tersebut setelah fungsi, dipisahkan dengan spasi seperti ini: 

> f :: Int -> Int -> Int -> Int
> f x y z = x + y + z
> ex17 = f 3 17 8

Contoh di atas menerapkan fungsi `f` ke tiga argumen: `3`, `17`, dan
dan `8`. Perhatikan juga sintaks tipe untuk fungsi dengan beberapa argumen
seperti: `Arg1Type -> Arg2Type -> ... -> ResultType`. Ini mungkin terlihat
aneh (memang sudah seharusnya). Mengapa semuanya tanda panah? Bukannya lebih
wajar kalau tipe untuk `f` berupa `Int Int Int -> Int`? Sebenarnya, sintaks
ini memang disengaja dan memiliki alasan yang mendalam dan indah, yang akan kita
pelajari beberapa minggu lagi. Untuk sementara ini, kalian percaya
saja dulu.
 
Perhatikan bahwa **aplikasi fungsi memiliki prioritas (*precedence*) lebih tinggi
ketimbang operator *infix***. Jadi penulisan seperti ini

`f 3 n+1 7`

adalah salah jika kalian ingin memberi `n+1` sebagai argumen kedua ke `f`
karena akan di*parse* sebagai

`(f 3 n) + (1 7)`.

Penulisan yang benar adalah:

`f 3 (n+1) 7`.

List
----

List adalah satu tipe data dasar di Haskell.

> nums, range, range2 :: [Integer]
> nums   = [1,2,3,19]
> range  = [1..100]
> range2 = [2,4..100]

Haskell (seperti Python) juga memiliki *list comprehensions*. Kalian bisa
mempelajarinya di [LYAH](http://learnyouahaskell.com/starting-out).

*String* hanyalah list karakter. Dengan kata lain, `String` hanyalah
singkatan dari `[Char]`, dan sintak literal string (teks di dalam tanda
kutip ganda) hanyalah singkatan untuk literal list `Char`.

> -- hello1 dan hello2 adalah sama.
>
> hello1 :: [Char]
> hello1 = ['h', 'e', 'l', 'l', 'o']
>
> hello2 :: String
> hello2 = "hello"
>
> helloSame = hello1 == hello2

Ini berarti semua fungsi di librari standar untuk memproses list juga bisa
digunakan untuk memproses `String`.


Membangun list
--------------

List yang paling sederhana ialah list kosong:

> emptyList = []

List lainnya dibangun dari list kosong dengan menggunakan operator *cons*
, `(:)`. *Cons* menerima argumen sebuah elemen dan sebuah list, dan mengembalikan
list baru dengan elemen tersebut ditambahkan ke depan list.

> ex17 = 1 : []
> ex18 = 3 : (1 : [])
> ex19 = 2 : 3 : 4 : []

> ex20 = [2,3,4] == 2 : 3 : 4 : []

Kita bisa melihat bahwa notasi `[2,3,4]` hanyalah singkatan untuk
`2 : 3 : 4 : []`. Perhatikan juga bahwa ini adalah
*singly linked lists*, BUKAN arrays.

> -- Buat barisan dari iterasi hailstone dari bilangan awal.
> hailstoneSeq :: Integer -> [Integer]
> hailstoneSeq 1 = [1]
> hailstoneSeq n = n : hailstoneSeq (hailstone n)

Kita stop barisan *hailstone* ketika mencapai 1. Barisan *hailstone*
untuk `n` terdiri dari `n` itu sendiri, diikuti dengan barisan dari
`hailstone n` yang merupakan bilangan yang didapat dari menerapkan
fungsi `hailstone` ke `n`.


Fungsi pada list
----------------

Kita bisa menulis fungsi pada list menggunakan pencocokan pola
(*pattern matching*). 

> -- Hitung panjang sebuah list Integer.
> intListLength :: [Integer] -> Integer
> intListLength []     = 0
> intListLength (x:xs) = 1 + intListLength xs

Klausa pertama menyatakan bahwa panjang dari sebuah list kosong adalah 0.
Klausa kedua menyatakan jika input list berbentuk seperti `(x:xs)`,
yaitu elemen pertama `x` disambung (*cons*) ke sisa list `xs`, maka
panjang dari list tersebut ialah lebih dari satu panjangnya `xs`.
       
Karena kita tidak menggunakan `x` sama sekali, kita bisa menggantinya
dengan *underscore*: `intListLength (_:xs) = 1 + intListLength xs`.

Kita juga bisa menggunakan pola bertumpuk (*nested patterns*):

> sumEveryTwo :: [Integer] -> [Integer]
> sumEveryTwo []         = []     -- Biarkan list kosong
> sumEveryTwo (x:[])     = [x]    -- Biarkan list dengan elemen tunggal
> sumEveryTwo (x:(y:zs)) = (x + y) : sumEveryTwo zs

Perhatikan bagaimana klausa terakhir mencocokkan list yang dimulai dengan
`x` lalu diikuti dengan `y` dan diikuti dengan list `zs`. Kita sebenarnya
tidak memerlukan tanda kurung tambahan, jadi bisa juga ditulis menjadi
 `sumEveryTwo (x:y:zs) = ...`.


Kombinasi fungsi
----------------

Menggabungkan fungsi-fungsi sederhana untuk membangun fungsi yang kompleks
merupakan cara memprogram Haskell yang baik.

> -- Jumlah hailstone yang dibutuhkan untuk mencapai 1
> -- dari bilangan awal.
> hailstoneLen :: Integer -> Integer
> hailstoneLen n = intListLength (hailstoneSeq n) - 1

Ini mungkin terlihat tidak efisien bagi kalian. Fungsi tersebut membangun
seluruh barisan *hailstone* lalu menghitung panjangnya. Tentunya boros
memori, bukan? Ternyata tidak! Karena Haskell dievaluasi secara *lazy*,
tiap elemen hanya akan dibangun ketika dibutuhkan. Jadi, pembuatan barisan
dan penghitungan panjang dilakukan secara berselingan. Seluruh komputasi
hanya memakai memori O(1), tak peduli sepanjang apapun barisannya (Sebenarnya
ini sedikit dusta tapi penjelasannya dan cara mengoreksinya harus menunggu
beberapa minggu). 

Kita akan belajar lebih jauh mengenai evaluasi *lazy* di Haskell beberapa
minggu lagi. Untuk saat ini cukup ketahui: jangan takut untuk menulis
fungsi kecil yang mengubah seluruh struktur data, dan menggabungkan
fungsi-fungsi tersebut untuk membangun fungsi yang lebih kompleks. Mungkin
akan terasa ganjil pada awalnya, tapi beginilah cara menulis program Haskell
yang idiomatis dan efisien. Lagipula, setelah terbiasa, kalian akan
merasa nyaman dengannya.

Sepatah kata tentang pesan kesalahan (*error message*)
----------------------------------------------

Sebenarnya, lima kata:

**Jangan takut dengan pesan kesalahan**

Pesan kesalahan dari GHC bisa panjang dan terlihat menakutkan.
Biasanya pesan tersebut panjang bukan karena tidak jelas, tapi karena
mengandung banyak informasi. Sebagai contoh:

    Prelude> 'x' ++ "foo"

    <interactive>:1:1:
        Couldn't match expected type `[a0]' with actual type `Char'
        In the first argument of `(++)', namely 'x'
        In the expression: 'x' ++ "foo"
        In an equation for `it': it = 'x' ++ "foo"

Pesan pertama: "Couldn't match expected type `[a0]` with actual
type `Char`", yang berarti tidak bisa mencocokkan tipe yang diharapkan `[a0]` dengan
tipe yang ada `Char`. Ini berarti *sesuatu* diharapkan bertipe list, tapi
malah bertipe `Char`. *Sesuatu* apa? Baris berikutnya berkata, argumen
pertama dari `(++)` yang salah, bernama `x`. Baris berikutnya lagi
membuat semakin jelas. Masalahnya adalah: `x` bertipe `Char` seperti yang
dikatakan oleh baris pertama. Mengapa diharapkan bertipe list? Karena itu
digunakan sebagai argumen pertama `(++)`, yang menerima list sebagai argumen pertama.

Ketika menerima pesan kesalahan yang panjang, janganlah takut. Ambil nafas panjang,
dan baca dengan seksama. Mungkin kalian tidak akan mengerti seluruhnya, tapi
kalian akan belajar banyak dan mungkin bisa mendapatkan informasi
bagaimana cara mengatasinya.
