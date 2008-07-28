; Miasma: x86 Scheme Assembler.
; Copyright (C) 2000, Darius Bacon, Brian Spilsbury, Alycat
; Refer to legal/License.txt

(expect 0 bits->bytes 0)
(expect 1 bits->bytes 8)
(expect 2 bits->bytes 16)

(expect #T fits-in-signed? 8 -128)
(expect #T fits-in-signed? 8    0)
(expect #T fits-in-signed? 8  127)
(expect #F fits-in-signed? 8 -129)
(expect #F fits-in-signed? 8  128)

(expect #F fits-in-unsigned? 8 -128)
(expect #F fits-in-unsigned? 8   -1)
(expect #T fits-in-unsigned? 8    0)
(expect #T fits-in-unsigned? 8  127)
(expect #T fits-in-unsigned? 8  255)
(expect #F fits-in-unsigned? 8  256)

