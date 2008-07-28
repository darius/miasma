; Miasma: x86 Scheme Assembler.
; Copyright (C) 2000, Darius Bacon, Brian Spilsbury, Alycat
; Refer to legal/License.txt

(expect '(ew e 2)  expand-abbrev 'ew)
(expect 'foo       expand-abbrev 'foo)

(expect "aad"      spec.stem (find-spec 'aad))
(expect "adc"      spec.stem (find-spec 'adc.%AL.Ib))
