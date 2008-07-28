; Miasma: x86 Scheme Assembler.
; Copyright (C) 2000, Darius Bacon, Brian Spilsbury, Alycat
; Refer to legal/License.txt

(expect #T             legal-register? '%eax 4)
(expect #F             legal-register? '%eax 1)
(expect #F             legal-register? '%al  4)
(expect #T             legal-register? '%al  1)
(expect #F             legal-register? 42    4)

(expect 0              register-number '%eax)
(expect 4              register-number '%sp)

(expect #T             condition-code? '?G)
(expect #F             condition-code? '%eax)

(define p0 (opcode-byte-param 42))
(define p1 (register-param '%ebx))
(define p2 (signed-immediate-param (expand-abbrev 'Ib)))

(define test-params (list p0 p1 p2))

(expect 0              arg-count p0)
(expect 0              arg-count p1)
(expect 1              arg-count p2)

(expect '(())          param-examples p0)
(expect '(())          param-examples p1)
(expect '((-128) (127)) param-examples p2)

(expect '()            distribute-args '() '())
(expect '(() () (137)) distribute-args test-params '(137))

(expect '()            static-bytes-after-each '())
(expect '(1 1 0)       static-bytes-after-each test-params)
