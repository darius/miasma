; Miasma: x86 Scheme Assembler.
; Copyright (C) 2000, Darius Bacon, Brian Spilsbury, Alycat
; Refer to legal/License.txt

;;;
;;; Translate to gas source format
;;; 

;; Write a gas-format assembler source file to PORT with the same meaning
;; as SRC.
(define translate-to-gas
  (lambda (src port origin)
    (say-to port "        .org  " origin)
    (say-to port "        .global _start")
    (say-to port "_start:")
    (say-to port "")
    (for-each (lambda (insn)
		(cond ((symbol? insn) (say-to port insn ":"))
		      ((pair? insn) (say-to port "        " (insn->gas insn)))
		      (else (impossible))))
	      src)))

;; Return a string, the gas instruction equivalent to INSN.
(define insn->gas
  (lambda (insn)
    (let ((spec (find-spec (insn.mnemonic insn)))
	  (args (insn.arguments insn)))
      (string-append (gas-mnemonic spec args)
		     " " 
		     (gas-arguments spec args)))))

;; Return a string, the gas-style mnemonic for an instruction of type
;; SPEC given the miasma-syntax arguments ARGS.  (We need to know the
;; arguments because condition codes end up in the gas mnemonic.)
(define gas-mnemonic
  (lambda (spec args)
    (string-append (stem->gas (spec.stem spec))
		   (if (and (pair? args)
			    (condition-code? (car args)))
		       (condition-name (car args))
		       "")
		   (gas-operand-size-suffix spec))))

;; Return the gas mnemonic for STEM (a string giving an instruction
;; mnemonic in our syntax, without argument-type suffixes).
(define stem->gas
  (let ((exceptions
	 '(;(movsx "...")          FIXME
	   ;(movzx "...")
	   (ascii  ".ascii")
	   (iretd  "iret")
	   (cmpsd  "cmpsl")
	   (insd   "insl")
	   (outsd  "outsl")
	   (int.3  "int $3")
	   (lodsd  "lodsl")
	   (movsd  "movsl")
	   (popad  "popal")
	   (popfd  "popfl")
	   (pushad "pushal")
	   (pushfd "pushfl")
	   (retf   "lret")
	   (scasd  "scasl")
	   (stosd  "stosl"))))
    (lambda (stem)
      ; (We don't compare strings directly because symbol case is unportable.)
      (cond ((assq (string->symbol stem) exceptions) => cadr)
	    (else stem)))))

;; Return a string suffix to tack onto the end of the gas mnemonic for
;; instructions of type SPEC.  This is a b, w, or l depending on (the
;; sometimes unobvious notion of) the size of the destination.
(define gas-operand-size-suffix
  (lambda (spec)
    (let ((operand (any param-operand (spec.params spec))))
      (if operand
	  (case (operand.size operand)
	    ((1) "b")
	    ((2) "w")
	    ((4) "l")
	    (else (impossible)))
	  ""))))

;; Return a string with the gas-style argument list for ARGS to an instruction
;; of type SPEC.
(define gas-arguments
  (lambda (spec args)
    (let* ((params (spec.params spec))
	   (arglists (distribute-args params args)))
      (commaify (gas-reorder-args (spec.mnemonic spec)
				  (flatmap (lambda (pair)
					     (param->gas (car pair) (cdr pair)))
					   (map cons params arglists)))))))

;; Convert STRINGS, a list of gas-style arguments in Intel order, to 
;; at&t order as expected by gas for the instruction named MNEMONIC.
(define gas-reorder-args
  (lambda (mnemonic strings)
    (case mnemonic
      ((enter.Uw.Ub) strings)
      (else (reverse strings)))))

;; Concatenate STRINGS with commas between.
(define commaify
  (lambda (strings)
    (string-join strings ", ")))
