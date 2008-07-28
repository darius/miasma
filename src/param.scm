;; A literal opcode byte to output as is.
(define (opcode-byte-param byte)
  `(bytes u 1 ,byte))

;; A literal register whose identity is implicit in the opcode.
(define (register-param register)
  `(bytes u 0 0))

;; There's a global assumption in this assembler that we're in 32-bit
;; mode.  So for 32 bits we do nothing, and for 16 bits we emit an
;; operand-size prefix.
(define (size-mode-param bits)
  (if (= bits 32)
      `(bytes u 0 0)
      `(bytes u 1 #x66)))

;; An immediate field as specified by OPERAND.
(define (signed-immediate-param operand)
  (let ((size (operand.size operand)))
    `(bytes i ,size (arg int))))

;; The other type of immediate field.
(define (unsigned-immediate-param operand)
  (let ((size (operand.size operand)))
    `(bytes u ,size (arg int))))

;; A pc-relative jump offset.
(define (relative-jump-param operand)
  (let ((size (operand.size operand)))
    `(bytes i ,size
	    (- (arg int) (hereafter)))))

;; A segment-relative offset field (if I understand this... probably not)
(define (offset-param operand)
  (let ((size (operand.size operand)))
    `(bytes u ,size (arg int))))

;; A condition code field.  In Intel syntax, condition codes are part
;; of the mnemonic, but in my syntax they're a separate argument.  The
;; encoding of the condition gets added to OPCODE-BYTE on emission.
(define (condition-param opcode-byte)
  `(bytes u 1 (+ ,opcode-byte (arg cc))))

;; A general-register field that's added to an extended-opcode byte.
;FIXME: confusing name
(define (opcode+register-param opcode-byte operand)
  (let ((size (operand.size operand)))
    `(bytes u 1 (+ ,opcode-byte
		   (arg reg ,size)))))

;; A pair of fields that go into a mod-r/m encoding.  Ex is effective
;; address, Gx is general register.
(define (Ex.Gx-param Ex Gx)
  `(swap-args
    (mod-r/m (arg reg ,(operand.size Gx))
	     (arg ,Ex))))

;; Like Ex.Gx, but with source arguments in the opposite order.
(define (Gx.Ex-param Gx Ex)
  `(mod-r/m (arg reg ,(operand.size Gx))
	    (arg ,Ex)))

;; Like Ex.Gx, this becomes a mod-r/m, but with 3 extended opcode bits
;; in place of the general register code.
(define (Ex-param extended-opcode operand)
  `(mod-r/m ,extended-opcode
	    (arg ,operand)))
