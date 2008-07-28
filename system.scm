; Miasma: x86 Scheme Assembler.
; Copyright (C) 2000,2002 Darius Bacon, Brian Spilsbury, Alycat
; See the COPYING file

(define in-directory
  (lambda path
    (lambda (filename)
      (apply make-pathname (append path (list filename))))))

(define load-from
  (lambda path
    (lambda (filename)
      (write filename)
      (newline)
      (load ((apply in-directory path) filename)))))

(define support-files
  '("helpers.scm"
    "macro.scm"
    "random-macros.scm"
    "mword.scm"
    "test-helpers.scm"
    ))

(for-each (load-from "support") support-files)

(define source-files
  '(
     "bits.scm"
     "registers.scm"
     "parse.scm"
     "param.scm"
     "c-gen.scm"
     "python-gen.scm"
     "walk.scm"
    ))

(for-each macroexpand-file 
	  (map (in-directory "src") source-files)
	  (map (in-directory "expanded") source-files))
(for-each (load-from "expanded") source-files)

(setup-spec-table)

(define unit-test-files
  '(
     "test-bits.scm"
     "test-registers.scm"
     "test-parse.scm"
     "test-param.scm"
     "test-c-gen.scm"
     "test-walk.scm"
     ))
(for-each (load-from "src") unit-test-files)
