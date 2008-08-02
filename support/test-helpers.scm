(expect #f      char-index "" #\X)
(expect #f      char-index "floob" #\X)
(expect 0       char-index "X" #\X)
(expect 0       char-index "XgoofX" #\X)
(expect 4       char-index "goofX" #\X)
(expect 4       char-index "goofXXfloXo" #\X)

(expect ""      string-join '() "X")
(expect "glub"  string-join '("glub") "X")
(expect "aXbb"  string-join '("a" "bb") "X")
(expect "aXbbXccc" string-join '("a" "bb" "ccc") "X")
(expect "abcd"  string-join '("a" "b" "c" "d") "")

(expect (string #\" #\") string-quotify #\" "")
(expect (string #\" #\x #\") string-quotify #\" "x")
(expect (string #\" #\x #\y #\") string-quotify #\" "xy")
(expect (string #\" #\\ #\n #\") string-quotify #\" (string #\newline))

(expect '()     outer-product '() '())
(expect '()     outer-product '(a) '())
(expect '()     outer-product '()  '(x))
(expect '((a . x)) outer-product '(a)  '(x))
(expect '((a . x) (a . y) (a . z) (b . x) (b . y) (b . z))
                outer-product '(a b)  '(x y z))

(expect '(())   outer-product* '())
(expect '((a c x) (a c y) (a c z) (b c x) (b c y) (b c z))
                outer-product* '((a b) (c) (x y z)))

(expect '()     list-head '() 0)
(expect '()     list-head '(a b c) 0)
(expect '(a)    list-head '(a b c) 1)
(expect '(a b)  list-head '(a b c) 2)
(expect '(a b c) list-head '(a b c) 3)

(expect '((x . y)) acons 'x 'y '())

(expect 'bleah  concat-symbol 'bleah)
(expect 'foo-42 concat-symbol 'foo "-" 42)


(expect '()     butlast '(x))
(expect '(x)    butlast '(x y))
(expect '(x y)  butlast '(x y z))

(expect 'x      last '(x))
(expect 'y      last '(x y))
(expect 'z      last '(x y z))

(expect '()     remove-nth '(x) 0)
(expect '(y)    remove-nth '(x y) 0)
(expect '(x)    remove-nth '(x y) 1)
(expect '(y z)  remove-nth '(x y z) 0)
(expect '(x z)  remove-nth '(x y z) 1)
(expect '(x y)  remove-nth '(x y z) 2)

(expect #T      char-printable? #\A)
(expect #T      char-printable? #\?)
(expect #T      char-printable? #\space)
(expect #F      char-printable? #\tab)
(expect #F      char-printable? #\newline)

(expect #T      starts-with? 'foo '(foo goo))
(expect #F      starts-with? 'goo '(foo goo))
(expect #F      starts-with? 'foo 'howdy)

(expect '()     iota 0)
(expect '(1)    iota 1)
(expect '(1 2)  iota 2)

(expect "0"     integer->string 0)
(expect "-1"    integer->string -1)
(expect "4294967296" integer->string (expt 2 32))

(expect "foo"   trim-trailing "foo" #\X)
(expect "Xfoo"  trim-trailing "Xfoo" #\X)
(expect "foXXo" trim-trailing "foXXo" #\X)
(expect "bar"   trim-trailing "barX" #\X)
(expect "bar"   trim-trailing "barXX" #\X)
(expect "baXr"  trim-trailing "baXrXX" #\X)
(expect ""      trim-trailing "" #\X)
(expect ""      trim-trailing "X" #\X)
(expect ""      trim-trailing "XX" #\X)

