; Miasma: x86 Scheme Assembler.
; Copyright (C) 2000, Darius Bacon, Brian Spilsbury, Alycat
; Refer to legal/License.txt

; NOTE 
; This won't actually work anymore, since it's superseded by
; ../miasma-system.scm

(define loud-load
  (lambda (filename)
    (display filename)
    (newline)
    (load filename)))

;; Nonstandard Scheme defs (change this to call your own version)
(loud-load "portability-mz.scm")
;(loud-load "portability-uts.scm")

;; Load the modules
(loud-load "helpers.scm")
(loud-load "object.scm")
(loud-load "bits.scm")
(loud-load "sinks.scm")
(loud-load "parse.scm")
(loud-load "x86.scm")
(loud-load "assemble.scm")
(loud-load "gas.scm")

;; Unit tests
(loud-load "test-helpers.scm")
(loud-load "test-bits.scm")

(setup-spec-table)

(loud-load "test-parse.scm")
(loud-load "test-x86.scm")
(loud-load "test-assemble.scm")
(loud-load "test-gas.scm")

;; Functional tests
;; (Call (try-all) to actually run them.)
(loud-load "fun-tests.scm")

(loud-load "java.scm")
