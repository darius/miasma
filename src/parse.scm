; Miasma: x86 Scheme Assembler.
; Copyright (C) 2000,2002 Darius Bacon, Brian Spilsbury, Alycat
; See the COPYING file

; Return a new spec.
(define (make-spec mnemonic stem params doc-string uses)
  (list mnemonic
	stem 
	(filter (lambda (x) (not (equal? x '(bytes u 0 0))))
		params)
	doc-string
	uses
	#f))

(define spec.mnemonic   car)
(define spec.stem       cadr)
(define spec.params     caddr)
(define spec.doc-string cadddr)
(define spec.uses       (lambda (l) (car (cddddr l))))
(define spec.takes      (lambda (l) (cadr (cddddr l))))

; (string param-list) -> symbol
; Return an unambiguous mnemonic, given the name of an overloaded one
; and the params that resolve the overloading.
(define (make-mnemonic stem stuff-list)
  (make-combined-mnemonic stem
			  (map coerce-string (suffixes stuff-list))))

; (string string-list) -> symbol
; Return an instruction mnemonic formed out of STEM and SPECS.
(define (make-combined-mnemonic stem specs)
  (string->symbol 
   (string-join (cons stem specs) ".")))

(define (suffixes stuff-list)
  (define (suffixes1 x)
    (cond ((memq x '(Sreg cr dr))
	   '())
	  ((memq x '(=16 =32 + /r /0 /1 /2 /3 /4 /5 /6 /7))
	   '())
	  ((symbol? x)
	   (list x))
	  ((not (pair? x))
	   '())
	  (else
	   (flatmap suffixes1 x))))
  (flatmap suffixes1 stuff-list))


; A list of all specs.
(define the-specs '())

; Load the specs from the i386 instruction table.
(define (setup-spec-table)
  (set! the-specs
;	  (cons ascii-spec
;		(cons asciiz-spec
;		      (cons data-spec
	(map parse-spec
	     (snarf (make-pathname "src" "tables" "i386.scm")))))
;)))

; Return the spec with mnemonic MNEMONIC.
(define (find-spec mnemonic)
  (or (assq mnemonic the-specs)
      (panic "Unknown instruction" mnemonic)))


;;
;; Parse instruction table entries.
;;

(define (unparse-spec spec)
  `(,(spec.mnemonic spec) 
    ,@(map unparse-param (spec.params spec))
    ,(spec.doc-string spec)))

(define (parse-spec spec)
  (insist "Spec has everything"
	  (and (list? spec)
	       (<= 3 (length spec))
	       (symbol? (car spec))))
  ; ok we copy (cdr spec) until we hit the string
  ; this is then our params, and the next two are
  ; the doc, and optional reg/flag list
  (let loop ((p (cdr spec)) (r '()))
    (if (pair? p)
	(if (string? (car p))
	    (make-spec (make-mnemonic (symbol->string (car spec))
				      (reverse r))
		       (symbol->string (car spec))
		       (map parse-param (reverse r))
		       (car p)
		       (if (pair? (cdr p))
			   (cadr p)
			   #f))
	    (loop (cdr p) (cons (car p) r)))
	(impossible))))

(define (parse-param param)
  (let ((param (expand-abbrev param)))
    (cond
     ((byte? param)
      (opcode-byte-param param))
     ((register? param)
      (register-param param))
     ((memq param '(=16 =32))
      (size-mode-param (case param ((=16) 16) ((=32) 32))))
     ((operand? 'I param)
      (signed-immediate-param param))
     ((operand? 'U param)
      (unsigned-immediate-param param))
     ((operand? 'J param)
      (relative-jump-param param))
     ((operand? 'O param)
      (offset-param param))
     ((pair? param)
      (let* ((ls (map expand-abbrev (cdr param)))
	     (L (length ls)))
	(case (car param)
	  ((?) 
	   (insist "? syntax" 
		   (and (= L 1)
			(byte? (car ls))))
	   (condition-param (car ls)))
	  ((+) 
	   (insist "+ syntax" 
		   (and (= L 2)
			(byte? (car ls))
			(operand? 'G (cadr ls))))
	   (opcode+register-param (car ls) (cadr ls)))
	  ((/r) 
	   (insist "/r syntax"
		   (and (= L 2)
			(or (and (operand? 'E (car ls))
				 (operand? 'G (cadr ls)))
			    (and (operand? 'G (car ls))
				 (operand? 'E (cadr ls))))))
	   (if (operand? 'E (car ls))
	       (Ex.Gx-param (car ls) (cadr ls))
	       (Gx.Ex-param (car ls) (cadr ls))))
	  ((/0 /1 /2 /3 /4 /5 /6 /7)
	   (insist "/n syntax"
		   (and (= L 1)
			(operand? 'E (car ls))))
	   (let ((extended-opcode
		  (cadr (assq (car param) 
			      '((/0 0) (/1 1) (/2 2) (/3 3)
				(/4 4) (/5 5) (/6 6) (/7 7))))))
	     (Ex-param extended-opcode (car ls))))
	  (else (impossible)))))
     (else (impossible)))))

(define abbrevs
  (map (lambda (abbrev-pair expanded-pair)
	 (list (concat-symbol (car abbrev-pair) (cdr abbrev-pair))
	       (car expanded-pair)
	       (cdr expanded-pair)))
       (outer-product '(E G I U M R J O S) '(b w v d))
       (outer-product '(E G I U E E J O S) '(1 2 4 4))))
; FIXME: preserve semantics of M, R, v

(define (expand-abbrev x)
  (or (assq x abbrevs) x))

; FIXME: find some better name for `operand'
(define (operand? tag x)
  (and (pair? x)
       (symbol? (car x))
       (starts-with? tag (cdr x))
       (null? (cdddr x))
       (memv (caddr x) '(1 2 4))))

(define operand.symbol car)
(define operand.tag    cadr)
(define operand.size   caddr)
