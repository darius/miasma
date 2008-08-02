; Miasma: x86 Scheme Assembler.
; Copyright (C) 2000, Darius Bacon, Brian Spilsbury, Alycat
; Refer to legal/License.txt

;(expect "[blah]" embrace #\[ "blah" #\])

;(expect "foo"   argument->gas 'foo)
;(expect "$-42"  argument->gas -42)
;(expect (string #\" #\")     argument->gas "")
;(expect (string #\" #\x #\") argument->gas "x")

;(expect (string #\" #\\ #\n #\")
;       argument->gas (string #\newline))
;(expect (string #\" #\\ #\n #\\ #\t #\")
;       argument->gas (string #\newline #\tab))
;(expect (string #\" #\x #\\ #\t #\") 
;       argument->gas (string #\x #\tab))
;(expect (string #\" #\\ #\x #\8 #\x #\\ #\t #\") 
;       argument->gas (string (byte->char 8) #\x #\tab))

(expect "mov"           gas-mnemonic (find-spec 'MOV.Gv.Iv) '(%eax 42))

(expect "mov $42, %eax" insn->gas '(MOV.Gv.Iv %eax 42))
