;; Return true iff X is a general register of SIZE bytes.
(define legal-register?
  (lambda (x size)
    (memq? x (possible-registers size))))

;; Return a list of the general registers of SIZE bytes.
(define possible-registers
  (lambda (size)
    (case size
      ((1) '(%al %ah %bl %bh %cl %ch %dl %dh))
      ((2) '(%ax %bx %cx %dx %si %di %bp %sp))
      ((4) '(%eax %ebx %ecx %edx %esi %edi %ebp %esp))
      (else (impossible)))))

;; Return the 3-bit code for general register REG.
(define register-number
  (let ((table
	 '(
	   (%eax . 0) (%ecx . 1) (%edx . 2) (%ebx . 3)
	   (%esp . 4) (%ebp . 5) (%esi . 6) (%edi . 7)

	   (%ax . 0) (%cx . 1) (%dx . 2) (%bx . 3)
	   (%sp . 4) (%bp . 5) (%si . 6) (%di . 7)

	   (%al . 0) (%cl . 1) (%dl . 2) (%bl . 3)
	   (%ah . 4) (%ch . 5) (%dh . 6) (%bh . 7)

	   (%es . 0) (%cs . 1) (%ss . 2) (%ds . 3) (%fs . 4) (%gs . 5)

           (%cr0 . 0) (%cr2 . 2) (%cr3 . 3) (%cr4 . 4)
           (%dr0 . 0) (%dr1 . 1) (%dr2 . 2) (%dr3 . 3) (%dr6 . 6) (%dr7 . 7)
	   )))
    (lambda (reg)
      (cond ((assq reg table) => cdr)
	    (else (panic "Bad register" reg))))))

;; The set of all registers that can appear as instruction arguments.
(define registers
  (append (possible-registers 1)
	  (possible-registers 2)
	  (possible-registers 4)
	 `(%cs %ds %es %fs %gs %ss)
          ; control registers
         `(%cr0 %cr2 %cr3 %cr4)))
; FIXME: include debug registers, etc.

;; Return true iff X is a register.
(define register?
  (lambda (x)
    (memq? x registers)))
