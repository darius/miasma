; Miasma: x86 Scheme Assembler.
; Copyright (C) 2000, Darius Bacon, Brian Spilsbury, Alycat
; Refer to legal/License.txt


;;;
;;; Symbol tables
;;;

; a symbol-table is of the form ((sym . addr)) where addr is the address of
; the symbol

(define resolve
  (lambda (arg symbol-table default)
    (cond ((and (symbol? arg)
		(not (register?? arg))
		(not (condition-code? arg)))
	   (cond ((assq arg symbol-table) => cdr)
		 (else default)))
	  ((starts-with? 'at arg)
	   (list 'at (resolve (cadr arg) symbol-table default)))
	  (else arg))))

; FIXME: also disallow binding the register names
(define bind-symbol
  (lambda (symbol addr table)
    (if (assq symbol table)
	(panic "Multiply-defined symbol" symbol)
	(acons symbol addr table))))


;;;
;;; Assembly
;;;

(define make-insn      cons)
(define insn.mnemonic  car)
(define insn.arguments cdr)

;; Output the code for INSN to SINK, assuming it starts at address ADDR,
;; using symbol table RESOLVED.  
;; Return the address of the following instruction.
(define assemble-1
  (lambda (insn addr resolved sink)
    (let ((mnemonic (insn.mnemonic insn))
	  ; Note below that the default for unresolved symbols is the
	  ; current address -- this is a presumption that jump targets
	  ; will be within range.
	  (args (map (lambda (arg) (resolve arg resolved addr))
		     (insn.arguments insn))))
      ((get-handler mnemonic) args sink addr))))

;; Output absolute machine code for SRC to SINK, starting at address
;; ASSEMBLY-ORIGIN.  SRC is a list of instructions and labels.
;; Return the address following the last byte emitted.
(define (assemble src sink assembly-origin)

  ;; Assemble SRC to SINK, looking up symbols in SYMBOL-TABLE and
  ;; incrementally extending it with (UPDATE-SYMBOL-TABLE symbol addr table).
  ;; Return (RECEIVER updated-symbol-table addr).
  (define assembly-pass
    (lambda (sink symbol-table update-symbol-table receiver)
      (let loop ((code src) 
		 (addr assembly-origin)
		 (resolved symbol-table))
;(display "resolved = ") (print resolved)
	(if (null? code)
	    (receiver resolved addr)
	    (let ((insn (car code)))
	      (cond ((symbol? insn)
		     (loop (cdr code)
			   addr
			   (update-symbol-table insn addr resolved)))
		    ((pair? insn)
		     (loop (cdr code)
			   (assemble-1 insn addr resolved sink)
			   resolved))
		    (else 
		     (panic "Bad instruction syntax" insn))))))))

  (let ((symbol-table (assembly-pass (make-fake-sink) 
				     '() 
				     bind-symbol
				     (lambda (symtab addr) symtab))))
    (assembly-pass sink
		   symbol-table
		   (lambda (insn addr resolved) resolved)
		   (lambda (symtab addr) addr))))
