
*Algebraic data types*
======================

Bacaan tambahan:

  * [*Real World Haskell*](http://book.realworldhaskell.org/),
    bab 2 dan 3

*Enumeration types*
-------------------

Seperti bahasa pemrograman lain, Haskell membolehkan programer membuat
tipe enumerasi (*enumeration types*) sendiri. Contoh sederhana:

> data Thing = Shoe 
>            | Ship 
>            | SealingWax 
>            | Cabbage 
>            | King
>   deriving Show

Kita mendeklarasikan tipe baru bernama `Thing` dengan lima konstruktor
data (*data constructors*): `Shoe`, `Ship`, dan seterusnya, yang
merupakan nilai dari tipe `Thing`. `deriving Show` ialah mantra yang
memberitahu GHC untuk membuat kode konversi dari `Thing` ke `String`
secara otomatis. Hal tersebut digunakan ketika `ghci` mencetak nilai
dari ekspresi bertipe `Thing`.

> shoe :: Thing
> shoe = Shoe
>
> listO'Things :: [Thing]
> listO'Things = [Shoe, SealingWax, King, Cabbage, King]

Kita bisa menulis fungsi terhadap `Thing`s dengan *pattern-matching*.

> isSmall :: Thing -> Bool
> isSmall Shoe       = True
> isSmall Ship       = False
> isSmall SealingWax = True
> isSmall Cabbage    = True
> isSmall King       = False

Mengingat klausa fungsi dicoba dari atas ke bawah, kita bisa menyingkat
definisi dari `isSmall` seperti berikut:

> isSmall2 :: Thing -> Bool
> isSmall2 Ship = False
> isSmall2 King = False
> isSmall2 _    = True

Lebih jauh tentang *enumerations*
---------------------------------

`Thing` bertipe enumerasi *enumeration type*, mirip dengan yang ada
di bahasa lain seperti Java atau C++. Sebenearnya, enumerasi hanyalah
kasus spesifik dari sesuatu yang lebih umum di Haskell: tipe data *algebraic*
(*algebraic data types*).
Sebagai contoh tipe data yang bukan sekedar enumerasi, perhatikan
definisi dari `FailableDouble` berikut ini:

> data FailableDouble = Failure
>                     | OK Double
>   deriving Show

Di sini, tipe `FailableDouble` memiliki dua konstruktor data.
Yang pertama, `Failure`, tidak memerlukan argumen. Jadi `Failure` itu
sendiri ialah nilai yang bertipe `FailableDouble`. Yang kedua, `OK`,
menerima satu argumen bertipe `Double`. `OK` yang berdiri sendiri bukanlah
bertipe `FailableDouble`, kita harus memberinya sebuah `Double`. Sebagai
contoh, `OK 3.4` ialah nilai bertipe `FailableDouble`.

> exD1 = Failure
> exD2 = OK 3.4

Coba tebak: `OK` sendiri bertipe apa?

> safeDiv :: Double -> Double -> FailableDouble
> safeDiv _ 0 = Failure
> safeDiv x y = OK (x / y)

*Pattern-matching* lagi! Perhatikan pada kasus `OK`, kita bisa
memberi nama kepada `Double`-nya.

> failureToZero :: FailableDouble -> Double
> failureToZero Failure = 0
> failureToZero (OK d)  = d

*Data constructors* bisa memiliki lebih dari satu argumen.

> -- Simpan nama, umur, dan Thing favorit dari seseorang.
> data Person = Person String Int Thing
>   deriving Show
>
> brent :: Person
> brent = Person "Brent" 31 SealingWax
>
> stan :: Person
> stan  = Person "Stan" 94 Cabbage
>
> getAge :: Person -> Int
> getAge (Person _ a _) = a

Perhatikan bahwa *type constructor* dan *data constructor* sama-sama
bernama `Person`, tetapi mereka berada di *namespace* yang berbeda dan
dua hal yang berbeda. Hal ini, konstruktor tipe dan data bernama sama,
cukup umum, dan cukup membingungkan jika belum terbiasa.

*Algebraic data types* secara umum
----------------------------------

Pada umumnya, sebuah *algebraic data type* memiliki satu atau lebih
*data constructor*, dan tiap *data constructor* bisa memiliki nol
atau lebih argumen.

    data AlgDataType = Constr1 Type11 Type12
                     | Constr2 Type21
                     | Constr3 Type31 Type32 Type33
                     | Constr4

Ini menyatakan bahwa nilai dari tipe `AlgDataType` bisa dibangun dengan
empat cara: menggunakan `Constr1`, `Constr2`, `Constr3`, atau `Constr4`.
Tergantung dari konstruktor yang digunakan, nilai `AlgDataType` bisa
mengandung nilai-nilai lainnya. Misalnya, jika dibangun dengan menggunakan
`Constr1` maka nilai tersebut mengandung dua nilai lainnya, satu bertipe
`Type11` dan satu lagi bertipe `Type12`.

Perlu diingat: nama konstruktor tipe dan data harus selalu dimulai dengan
huruf besar, sedangkan variabel (termasuk nama fungsi) harus selalu dimulai
dengan huruf kecil. Hal ini untuk memudahkan parser Haskell mengetahui
mana nama yang merepresentasikan variabel, dan mana yang merepresentasikan
konstruktor.


*Pattern-matching*
------------------

Kita sudah melihat *pattern-matching* (pencocokkan pola) di beberapa kasus.
Kali ini kita akan melihat bagaimana *pattern-matching* bekerja secara umum.
Pada dasarnya, *pattern-matching* ialah memisahkan nilai berdasarkan
konstruktor yang membangunnya. Informasi tersebut bisa digunakan sebagai
penentu apa yang harus dilakukan. Ini adalah satu-satunya cara di Haskell.

Sebagai contoh, untuk menentukan apa yang harus dilakukan dengan nilai bertipe
`AlgDataType` (tipe yang kita buat sebelumnya), kita bisa menulis seperti

    foo (Constr1 a b)   = ...
    foo (Constr2 a)     = ...
    foo (Constr3 a b c) = ...
    foo Constr4         = ...

Perhatikan kita juga memberikan nama ke nilai-nilai di dalam tiap konstruktor.
Perhatikan juga tanda kurung diperlukan untuk pola (*patterns*) yang terdiri dari lebih
dari konstruktor tunggal.

Itulah ide utama dari pola, tapi ada beberapa hal lain yang perlu diperhatikan.

  1. Sebuah *underscore* `_` bisa digunakan sebagai "wildcard pattern" yang
    cocok dengan apapun.

  1. Sebuah pola berbentuk `x@pat` bisa digunakan untuk mencocokkan nilai dengan
    pola `pat`, tapi *juga* memberikan nama `x` ke seluruh nilai yang dicocokkan.
    Sebagai contoh:

    > baz :: Person -> String
    > baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

        *Main> baz brent
        "The name field of (Person \"Brent\" 31 SealingWax) is Brent"

  1. Pola bisa bertumpuk (*nested*). Sebagai contoh:

    > checkFav :: Person -> String
    > checkFav (Person n _ SealingWax) = n ++ ", you're my kind of person!"
    > checkFav (Person n _ _)          = n ++ ", your favorite thing is lame."

        *Main> checkFav brent
        "Brent, you're my kind of person!"
        *Main> checkFav stan
        "Stan, your favorite thing is lame."

    Perhatikan bagaimana kita menumpuk pola `SealingWax` di dalam pola `Person`.

*Grammar* berikut mendifinisikan apa yang bisa digunakan sebagai pola:    

    pat ::= _
         |  var
         |  var @ ( pat )
         |  ( Constructor pat1 pat2 ... patn )

Baris pertama menyatakan bahwa *underscore* adalah sebuah pola (*pattern*). Baris kedua
menyatakan sebuah variabel juga merupakan sebuah pola, yang cocok dengan
apapun dan memberikan nama variabel tersebut ke nilai yang dicocokkan.
Baris ketiga adalah pola `@`. Baris terakhir menyatakan bahwa nama konstruktor
yang diikuti oleh barisan pola juga merupakan sebuah pola. Pola ini cocok
dengan nilai yang dibangun dengan konstruktor tersebut, *dan* semua dari `pat1`
sampai `patn` cocok dengan nilai-nilai di dalam konstruktor secara rekursif.
       
(Sebenarnya masih banyak yang terkandung di pola *grammar*, tapi kita tidak
perlu sejauh itu.) 

Nilai seperti `2` atau `'c'` bisa dibayangkan sebagai konstruktor tanpa
argumen. Dengan kata lain, bagaikan tipe `Int` dan `Char` didefinisikan
seperti

    data Int  = 0 | 1 | -1 | 2 | -2 | ...
    data Char = 'a' | 'b' | 'c' | ...

yang berarti kita bisa mencocokkan pola dengan nilai literal. (Tentu saja
sebenarnya `Int` dan `Char` tidak didefinisikan seperti demikian.)

Ekspresi *case*
---------------

Konstruksi dasar untuk pencocokkan pola di Haskell ialah ekspresi `case`.
Pada umumnya, sebuah ekspresi `case` berbentuk

    case exp of
      pat1 -> exp1
      pat2 -> exp2
      ...

Ketika dievaluasi, ekspresi `exp` dicocokkan dengan tiap pola `pat1`,
`pat2`, dan seterusnya secara bergantian. Pola yang pertama cocok akan
dipilih, dan seluruh ekspresi `case` akan terevaluasi menjadi ekspresi
yang bersesuaian dengan pola yang cocok tersebut. Sebagai contoh,

> exCase = case "Hello" of
>            []      -> 3
>            ('H':s) -> length s
>            _       -> 7

terevaluasi menjadi `4` (pola kedua yang terpilih, pola ketiga juga
cocok tapi tidak terjangkau).

Sintaks untuk mendefinisikan fungsi yang telah kita lihat sebelumnya
hanyalah singkatan untuk mendefinisikan ekspresi `case`. Sebagai
contoh, definisi `failureToZero` yang telah kita temui sebelumnya
bisa ditulis ulang menjadi

> failureToZero' :: FailableDouble -> Double
> failureToZero' x = case x of
>                      Failure -> 0
>                      OK d    -> d

Tipe data rekursif
------------------

Tipe data bisa *rekursif*, yaitu didefinisikan dalam bentuk dirinya
sendiri. Kita sudah melihat sebuah tipe rekursif, list.
List bisa kosong, atau berisi satu elemen diikuti list sisanya.
Kita bisa mendefinisikan list kita sendiri seperti:

> data IntList = Empty | Cons Int IntList

*Built-in* list di Haskell cukup serupa. List-list tersebut menggunakan
sintaks bawaan, `[]` dan `:`. Mereka juga berfungsi pada tipe elemen apapun
, tidak sekedar `Int` (akan dibahas lebih jauh di bab berikutnya).

Kita sering menggunakan fungsi rekursif untuk memproses tipe data rekursif:

> intListProd :: IntList -> Int
> intListProd Empty      = 1
> intListProd (Cons x l) = x * intListProd l

Contoh lainnya, kita bisa mendefinisikan sebuah tipe pohon biner (*binary tree*)
dengan sebuah nilai `Int` tersimpan di tiap *node* internal, dan `Char` di tiap
*leaf*:

> data Tree = Leaf Char
>           | Node Tree Int Tree
>   deriving Show

(Jangan bertanya untuk apa pohon tersebut, ini hanyalah contoh. Ok?)

Contohnya:

> tree :: Tree
> tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))
