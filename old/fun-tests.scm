; Miasma: x86 Scheme Assembler.
; Copyright (C) 2000, Darius Bacon, Brian Spilsbury, Alycat
; Refer to legal/License.txt

;;;;
;;;; Functional tests.
;;;; To verify output:
;;;; $ cd test; make test
;;;;

;;;
;;; Test infrastructure
;;;

(define make-test cons)
(define test.name car)
(define test.body cdr)

(define try-all
  (lambda ()
    (for-each 
     try-test 
     (list trivial-test hello-test multihello-test
	   (make-combo-test)
	   ))))

(define try-test
  (lambda (test)
    (assemble-with-listing (test.body test) (test.name test) 0)))

;; Assemble source PROGRAM to binary file test/PROGRAM.bl, with supplementary
;; listing test/PROGRAM.s.
(define assemble-with-listing
  (lambda (program filename origin)
    (call-with-test-file filename ".s"
      (lambda (port)
	(translate-to-gas program port origin)))
    (call-with-test-file filename ".bl"
      (lambda (port)
	(assemble program (make-sink port) origin)))))

(define call-with-test-file
  (lambda (filename extension receiver)
    (call-with-new-output-file 
      (make-pathname "miasma" "test" (string-append filename extension))
      receiver)))

;;;
;;; The tests
;;;

(define trivial-test
  (make-test "trivial"
	     '(     (INT.Ub  #x80)
		    (RET)
		    )))

(define hello-world (string-append "Hello, world!" (string #\newline)))

(define hello-test 
  (make-test "hello"
	     `(	   (MOV.Gv.Iv %eax 4)
		   (MOV.Gv.Iv %ebx 1)
		   (MOV.Gv.Iv %ecx data)
		   (MOV.Gv.Iv %edx ,(string-length hello-world))
		   (INT.Ub    #x80)
		   (RET)
	     data  (ASCII     ,hello-world)
	     )))

(define multihello-test 
  (make-test "multi"
	     `(         (MOV.Gv.Iv %esi 10)			;loop count
		 again  (MOV.Gv.Iv %eax 4)
		        (MOV.Gv.Iv %ebx 1)
			(MOV.Gv.Iv %ecx data)
			(MOV.Gv.Iv %edx ,(string-length hello-world))
			(INT.Ub    #x80)
			(DEC.Gv    %esi)
			(J.?.Jb    ?A again)
			(RET)
		 data   (ASCII     ,hello-world)
		 )))

;; Return a test that exercises many combinations of opcode and 
;; addressing mode.
(define make-combo-test
  (lambda ()
    (make-test "combo" (combo-insns))))

;; The same as above, only as a list of many tests, one per combination.
;; Results go in the breakdown/ directory.
(define make-combo-tests
  (lambda ()
    (let ((insns (combo-insns)))
      (map (lambda (insn i)
	     (make-test (string-append "breakdown/" (number->string i))
			(list insn)))
	   insns
	   (iota (length insns))))))

;; Return a list of example instructions.
(define combo-insns
  (lambda ()
    
    ;; Return true iff MNEMONIC is an instruction prefix, like rep: or ss:.
    (define prefix?
      (lambda (mnemonic)
	(let ((name (symbol->string mnemonic)))
	  (char=? #\: 
		  (string-ref name (- (string-length name) 1))))))

    (filter (complement (compose prefix? insn.mnemonic))
	    (filter gas-supported-insn?
		    (flatmap make-examples the-specs)))))

;; Return true iff the translate-to-gas of INSN should produce the same
;; code as we generate.
(define gas-supported-insn?
  (lambda (insn)
    (case (insn.mnemonic insn)
      ((aad.Ub aam.Ub) #f)
      ((push.Iv)
       ; gas will generate push.Ib instead, if possible -- so filter out
       (let ((arg (car (insn.arguments insn))))
	 (not (and (number? arg)
		   (fits-in-signed? 8 arg)))))
      ((out.%dx.%al out.%dx.%eax)	; gas seems buggy on these
       #f)
      (else #t))))
