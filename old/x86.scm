; Miasma: x86 Scheme Assembler.
; Copyright (C) 2000, Darius Bacon, Brian Spilsbury, Alycat
; Refer to legal/License.txt

;;;
;;; The actual machinery for generating machine-code instruction fields.
;;; (And related methods for each parameter class.)
;;;


(define put-mod-r/m
  (lambda (sink reg/opcode ea)
    (parse-EA ea
              (lambda (mod r/m after-proc)
                (put-byte sink 
                           (+ (<< mod 6) 
                              (<< reg/opcode 3)
                              r/m))
                (+ 1 (after-proc sink))))))

;; Given an effective-address operand EA, return (RECEIVER mod r/m
;; proc) where MOD and R/M are fields of the mod-r/m byte for EA, and
;; PROC is a sink procedure that puts any bytes needed after the
;; mod-r/m byte.
(define parse-EA
  (lambda (ea receiver)
    ; FIXME: handle more addressing modes
    (cond 
     ((starts-with? 'at ea)
      (if (register?? (cadr ea))
          (receiver 0 
                    (register-number (cadr ea))
                    (lambda (sink) 0))
          (receiver 0 5 (lambda (sink)
                          (put-signed-bytes sink 4 (cadr ea))))))
     ((starts-with? 'at+b ea)
      (receiver 1 
                (register-number (cadr ea))
                (lambda (sink) 
                  (put-signed-bytes sink 1 (caddr ea)))))
     ((starts-with? 'at+d ea)
      (receiver 2
                (register-number (cadr ea))
                (lambda (sink)
                  (put-signed-bytes sink 4 (caddr ea)))))
     ((register?? ea)
      (receiver 3
                (register-number ea)
                (lambda (sink) 0)))
     ;; The remaining possibilities have a scale/index byte...
     (else (panic "Bad effective address syntax" ea)))))



;; Condition-code arguments and their encodings.
(define condition-table
  '((?O   . #x0)
    (?NO  . #x1)
    (?C   . #x2) (?B   . #x2) (?NAE . #x2)
    (?AE  . #x3) (?NB  . #x3) (?NC  . #x3)
    (?E   . #x4) (?Z   . #x4)
    (?NE  . #x5) (?NZ  . #x5)
    (?BE  . #x6) (?NA  . #x6)
    (?A   . #x7) (?NBE . #x7)
    (?S   . #x8)
    (?NS  . #x9)
    (?P   . #xA) (?PE  . #xA)
    (?PO  . #xB) (?NP  . #xB)
    (?L   . #xC) (?NGE . #xC)
    (?GE  . #xD) (?NL  . #xD)
    (?LE  . #xE) (?NG  . #xE)
    (?G   . #xF) (?NLE . #xF)))

(define conditions (map car condition-table))

;; Return true iff ARG is a condition-code.
(define condition-code?
  (lambda (arg)
    (memq? arg conditions)))

;; Return the machine-language encoding of condition-code CC.
(define condition-number
  (lambda (cc)
    (cdr (assq cc condition-table))))

;; Return a string giving the condition-code component of an Intel mnemonic,
;; for condition CC.
(define condition-name
  (lambda (cc)
    (let ((s (symbol->string cc)))
      (substring s 1 (string-length s)))))


;; Return an assembly procedure for instructions named MNEMONIC.
(define get-handler
  (lambda (mnemonic)
    (let ((params (spec.params (find-spec mnemonic))))
      (lambda (args sink addr)
        (assemble-params params args sink addr)))))

;; Return a list of sample instructions of type SPEC.
(define make-examples
  (lambda (spec)
    (map (lambda (args) 
           (make-insn (spec.mnemonic spec) 
                      (foldr append '() args)))
         (outer-product* (map param-examples (spec.params spec))))))

;; Output machine code for an instruction to SINK, and return the
;; first address after the instruction.
;;   PARAMS - instruction-type parameters for this instruction
;;   ARGS   - actual arguments to the instruction
;;   ADDR   - address of the start of the instruction
(define assemble-params
  (lambda (params args sink addr)
    ; This assumes that params that need to know how many bytes follow
    ; them are not followed by a variable-length param (that's why we
    ; can get away with using static bytes after each, for the actual
    ; count of bytes after each).
    (let ((afters (static-bytes-after-each params))
          (arglists (distribute-args params args)))
      (do ((params   params   (cdr params))
           (afters   afters   (cdr afters))
           (arglists arglists (cdr arglists))
           (addr     addr     (+ addr 
                                 (put-param (car params)
                                            sink
                                            addr
                                            (car afters) 
                                            (car arglists)))))
          ((null? params) addr)))))

; FIXME: I don't think we actually need this for any real x86 instructions...
; oops.
;; Machine-code instructions are built out of fixed- and variable-length 
;; fields, one field for each of PARAMS.  So each param has a count of
;; how many fixed bytes follow it in the instruction.  Return a list of
;; that count for each param.
(define static-bytes-after-each
  (lambda (params)
    (cdr (foldr (lambda (param afters)
                  (cons (+ (static-size param) (car afters))
                        afters))
                '(0)
                params))))

;; Divide up ARGS accoding to which param of PARAMS each is supplied to.
;; Return a list of arg lists, one for each param.
(define distribute-args
  (lambda (params args)
    (let ((arg-counts (map arg-count params)))
      (if (not (= (length args) (foldl + 0 arg-counts)))
          (panic "Arguments don't match parameters"
                 args
                 (map unparse-param params)))
      (do ((params  params     (cdr params))
           (counts  arg-counts (cdr counts))
           (args    args       (list-tail args (car counts)))
           (results '()        (cons (list-head args (car counts)) results)))
          ((null? params) (reverse results))))))

;;;
;;; Types of params
;;;

;; (param) -> list
;; Return a human-readable representation.  (For debugging, etc.)
(define unparse-param  (make-generic-function 'unparse))

;; (param sink addr after args) -> int
;; Output the machine code for this param to SINK, and return the count
;; of bytes it took.  
;;   ADDR  - the starting address it's output to
;;   AFTER - the static-size total for the params that follow this
;;   ARGS  - the relevant arguments from the instruction to assemble
(define put-param      (make-generic-function 'put))

;; (param argnames after) -> void
;; Write to stdout Java code to output machine code for this param, 
;; given that the param arguments are in Java variables named ARGNAMES.
;;   AFTER - the static-size total for the params that follow this
(define java-sink      (make-generic-function 'java-put))

;; FIXME: todo
(define putter         (make-generic-function 'putter))

;; (param) -> string-list
;; Return the Java argument type signature for this param's args.
(define java-arg-types (make-generic-function 'arg-types))

;; (param) -> int
;; Return how many instruction arguments a param of this type expects.
(define arg-count      (make-generic-function 'arg-count))

;; (param) -> int
;; Return how many bytes a param of this type assembles into.
;; (If it outputs a variable-length field, the variable component 
;; doesn't count.)
(define static-size    (make-generic-function 'static-size))

;; (param) -> string-list
;; Return suffixes to append to an overloaded mnemonic to generate an
;; unambiguous one.
(define name-suffix    (make-generic-function 'name-suffix))

;; (param) -> arg-list-list
;; Return a list of some valid arg lists that could be passed to put-param
;; with this param.  This is used for testing.
(define param-examples (make-generic-function 'examples))

;; (param arg-list) -> string-list
;; Convert the args in ARG-LIST to strings in gas syntax.
(define param->gas     (make-generic-function '->gas))

;; (param) -> optional-operand
;; Return this param's first operand specifier, or #f if none.
;; FIXME: explain what that's all about...
(define param-operand  (make-generic-function 'operand))

;; Concrete parameter types extend this.
(define default-param
  (lambda (arg-count static-size . symbols)
    (let ((suffixes (map symbol->string symbols)))
      (make #f
            `((name-suffix   ,(lambda (me) suffixes))
              (arg-count     ,(lambda (me) arg-count))
              (static-size   ,(lambda (me) static-size))
              (examples      ,(lambda (me) '(())))
              (operand       ,(lambda (me) #f))
              (->gas         ,(lambda (me args) '())))))))

;; Write to stdout Java code calling METHOD with arguments ARGS.
(define java-call
  (lambda (method . args)
    (say "  " method "(" (commaify (map coerce-string args)) ");")))

;; A literal opcode byte to output as is.
(define opcode-byte-param
  (lambda (byte)
    (make (default-param 0 1)
          `((unparse     ,(lambda (me) `(op ,byte)))
            (arg-types   ,(lambda (me) '()))
            (putter      ,(lambda (me)
                            `(unsigned 1 ,byte)))
            (java-put    ,(lambda (me names after)
                            (java-call "putByte" byte)))
            (put         ,(lambda (me sink addr after args)
                            (put-byte sink byte)))))))

;; A literal register whose identity is implicit in the opcode.
(define register-param
  (lambda (register)
    (make (default-param 0 0 register)
          `((unparse     ,(lambda (me) `(reg ,register)))
            (arg-types   ,(lambda (me) '()))
            (->gas       ,(lambda (me args) 
                            (list (symbol->string register))))
            (putter      ,(lambda (me)
                            `(unsigned 0)))
            (java-put    ,(lambda (me names after) 'pass))
            (put         ,(lambda (me sink addr after args)
                            0))))))

;; Restricts the instruction this appears in to either 16- or 32-bit mode.
;; This is hacked to presume we're normally in 32-bit mode, and 16-bit
;; mode instructions are exceptions. TODO: Fixme
;; (I think this was changed by Brian to stick in an implicit prefix
;; instead of requiring it to be explicit the way everything else is in
;; Miasma.  I'll leave it alone for now.)
(define size-mode-param
  (lambda (bits)
    (if (= bits 32)
        ; 32 bit prefix does nothing
        (make (default-param 0 0)
              `((unparse     ,(lambda (me) `(size ,bits)))
                (arg-types   ,(lambda (me) '()))
                (examples    ,(lambda (me) '(()))) ; one example
                (putter      ,(lambda (me) `(unsigned 0)))
                (java-put    ,(lambda (me names after) 'pass))
                (put         ,(lambda (me sink addr after args)
                                0))))
        ; 16 bit prefix emits that byte
        (make (default-param 0 1)
              `((unparse     ,(lambda (me) `(size ,bits)))
                (arg-types   ,(lambda (me) '()))
                (examples    ,(lambda (me) '())) ; no examples
                (putter      ,(lambda (me) `(unsigned 1 #x66)))
                (java-put    ,(lambda (me names after) 
                                (java-call "putByte" #x66)))
                (put         ,(lambda (me sink addr after args)
                                (put-byte sink #x66))))))))

;; Return a gas-syntax numeric literal.
(define number-arg->gas
  (lambda (me args)
    (list (string-append "$" (coerce-string (car args))))))

;; An immediate field as specified by OPERAND.
(define signed-immediate-param
  (lambda (operand)
    (let ((symbol (operand.symbol operand))
          (size   (operand.size operand)))
      (make (default-param 1 size symbol)
            `((unparse   ,(lambda (me) `(imm ,size)))
              (arg-types ,(lambda (me) '("int")))
              (examples  ,(lambda (me) 
                            (map list (signed-constant-examples (* 8 size)))))
              (->gas     ,number-arg->gas)
;             (operand   ,(lambda (me) operand))
              (putter    ,(lambda (me)
                            `(signed ,size arg)))
              (java-put  ,(lambda (me names after) 
                            (java-call "putSignedBytes" size (car names))))
              (put       ,(lambda (me sink addr after args)
                            (insist "Argument type [signed-immediate]"
                                    (integer? (car args)))
                            (put-signed-bytes sink size (car args)))))))))

;; The other type of immediate field.
(define unsigned-immediate-param
  (lambda (operand)
    (let ((symbol (operand.symbol operand))
          (size   (operand.size operand)))
      (make (default-param 1 size symbol)
            `((unparse   ,(lambda (me) `(uimm ,size)))
              (arg-types ,(lambda (me) '("int")))
              (examples  ,(lambda (me) 
                            (map list (unsigned-constant-examples (* 8 size)))))
              (->gas     ,number-arg->gas)
;             (operand   ,(lambda (me) operand))
              (putter    ,(lambda (me)
                            `(unsigned ,size arg)))
              (java-put  ,(lambda (me names after) 
                            (java-call "putUnsignedBytes" size (car names))))
              (put       ,(lambda (me sink addr after args)
                            (insist "Argument type [unsigned immediate]" 
                                    (integer? (car args)))
                            (put-bytes sink size (car args)))))))))

;; A pc-relative jump offset.
(define relative-jump-arg
  (lambda (operand)
    (let ((symbol (operand.symbol operand))
          (size   (operand.size operand)))
      (make (default-param 1 size symbol)
            `((unparse   ,(lambda (me) `(jump ,size)))
              (arg-types ,(lambda (me) '("int")))
              (examples  ,(lambda (me) '())) ;FIXME
              (->gas     ,(lambda (me args)
                            (list (string-append (coerce-string (car args))))))
              (putter    ,(lambda (me)
                            `(signed ,size
                                     (- arg hereafter))))
              (java-put  ,(lambda (me names after)
                            (java-call "putSignedBytes" 
                                       size
                                       (string-append (car names)
                                                      " - "
                                                      "(currentAddress()"
                                                      " + "
                                                      (number->string size)
                                                      " + "
                                                      (number->string after)
                                                      ")"))))
              (put       ,(lambda (me sink addr after args)
                            (insist "Argument type [relative-jump]" 
                                    (integer? (car args)))
                            (put-signed-bytes sink size
                                               (- (car args) 
                                                  (+ addr size after))))))))))

;; A segment-relative offset field (if I understand this... probably not)
(define offset-param
  (lambda (operand)
    (let ((symbol (operand.symbol operand))
          (size   (operand.size operand)))
      (make (default-param 1 size symbol)
            `((unparse   ,(lambda (me) `(offset ,size)))
              (arg-types ,(lambda (me) '("int")))
              (examples  ,(lambda (me) '())) ;FIXME
              (->gas     ,number-arg->gas) ;FIXME
              (operand   ,(lambda (me) operand))
              (putter    ,(lambda (me)
                            `(unsigned ,size arg)))
              (java-put  ,(lambda (me names after) 
                            (java-call "putUnsignedBytes" size (car names))))
              (put       ,(lambda (me sink addr after args)
                            (insist "Argument type [offset]" 
                                    (integer? (car args)))
                            (put-bytes sink size (car args)))))))))

;; A condition code field.  In Intel syntax, condition codes are part
;; of the mnemonic, but in my syntax they're a separate argument.  The
;; encoding of the condition gets added to OPCODE-BYTE on emission.
;; Note that the ->gas method is the default do-nothing one, because
;; in gas this gets tacked onto the mnemonic instead of the argument
;; list (by code in gas.scm).
(define condition-param
  (lambda (opcode-byte)
    (make (default-param 1 1 '?)
          `((unparse   ,(lambda (me) `(? ,opcode-byte)))
            (arg-types ,(lambda (me) '("Condition")))
            (examples  ,(lambda (me) (map list conditions)))
            (putter    ,(lambda (me)
                          `(unsigned 1 (+ ,opcode-byte (arg cc)))))
            (java-put  ,(lambda (me names after)
                          (java-call "putByte" 
                                     (string-append (number->string opcode-byte)
                                                    " + "
                                                    (car names)
                                                    ".conditionNumber()"))))
            (put       ,(lambda (me sink addr after args)
                          (insist "Argument type [condition]" 
                                  (condition-code? (car args)))
                          (put-byte sink
                                     (+ opcode-byte
                                        (condition-number (car args))))))))))

;; A general-register field that's added to an extended-opcode byte.
(define opcode+register-param           ;FIXME: confusing name
  (lambda (opcode-byte operand)
    (let ((symbol (operand.symbol operand))
          (size   (operand.size operand)))
      (make (default-param 1 1 symbol)
            `((unparse   ,(lambda (me) `(+reg ,opcode-byte ,size)))
              (arg-types ,(lambda (me) '("Register")))
              (examples  ,(lambda (me) 
                            ; al/ax/eax comes first; we skip it with
                            ; the cdr because it's often got a special
                            ; variant encoding that gas will use instead
                            ; (thus generating a spurious difference).
                            (map list (cdr (possible-registers size)))))
              (->gas     ,(lambda (me args) 
                            (list (symbol->string (car args)))))
              (putter    ,(lambda (me)
                            `(unsigned 1 (+ ,opcode-byte
                                            (arg reg ,size)))))
              (java-put  ,(lambda (me names after)
                            (java-call "putByte"
                                       (string-append
                                        (number->string opcode-byte)
                                        " + "
                                        (car names)
                                        ".registerNumber()"))))
              (put       ,(lambda (me sink addr after args)
                            (insist "Register argument"
                                    (legal-register? (car args) size))
                            (put-byte sink
                                       (+ opcode-byte
                                          (register-number (car args)))))))))))

;; A pair of fields that go into a mod-r/m encoding.  Ex is effective
;; address, Gx is general register.
(define Ex.Gx-param
  (lambda (Ex Gx)
    (make (default-param 2 1 (operand.symbol Ex) (operand.symbol Gx))
          `((unparse   ,(lambda (me) `(Ex.Gx ,Ex ,Gx)))
            (arg-types ,(lambda (me) '("EA_Operand" "Register")))
            (examples  ,(lambda (me) 
                          (outer-product* (list (Ex-examples Ex)
                                                (Gx-examples Gx)))))
            (operand   ,(lambda (me) Ex))
            (->gas     ,(lambda (me args) 
                          (list (Ex->gas (car args))
                                (symbol->string (cadr args)))))
            (putter    ,(lambda (me)
                          `(swap-args
                            (mod-r/m (arg reg ,(operand.size Gx))
                                     (arg ,Ex)))))
            (java-put  ,(lambda (me names after)
                          (java-call "putModRM" 
                                     (string-append (cadr names)
                                                    ".registerNumber()")
                                     (car names))))
            (put       ,(lambda (me sink addr after args)
                          ; add check for legality of car
                          (insist "Register argument"
                                  (legal-register? (cadr args) 
                                                   (operand.size Gx)))
                          (put-mod-r/m sink 
                                       (register-number (cadr args))
                                       (car args))))))))

;; Like Ex.Gx, but with source arguments in the opposite order.
(define Gx.Ex-param
  (lambda (Gx Ex)
    (make (default-param 2 1 (operand.symbol Gx) (operand.symbol Ex))
          `((unparse   ,(lambda (me) `(Gx.Ex ,Gx ,Ex)))
            (arg-types ,(lambda (me) '("Register" "EA_Operand")))
            (examples  ,(lambda (me) 
                          (outer-product* (list (Gx-examples Gx)
                                                (Ex-examples Ex)))))
            (examples  ,(lambda (me) '())) ;FIXME
            (operand   ,(lambda (me) Gx))
            (->gas     ,(lambda (me args) 
                          (list (symbol->string (car args))
                                (Ex->gas (cadr args)))))
            (putter    ,(lambda (me)
                          `(mod-r/m (arg reg ,(operand.size Gx))
                                    (arg ,Ex))))
            (java-put  ,(lambda (me names after)
                          (java-call "putModRM" 
                                     (string-append (car names)
                                                    ".registerNumber()")
                                     (cadr names))))
            (put       ,(lambda (me sink addr after args)
                          (insist "Register argument"
                                  (legal-register? (car args) 
                                                   (operand.size Gx)))
                          ; add check for legality of cadr
                          (put-mod-r/m sink 
                                       (register-number (car args))
                                       (cadr args))))))))

(define Ex->gas
  (lambda (arg)
;          ; FIXME: what about (at 42)?  is that ($42)?  don't think so.
;         ; TODO: cases for fancier addressing modes
    (if (and (pair? arg) (memq (car arg) '(at at+b at+d)))
        (string-append "(" (coerce-string (cadr arg)) ")")
        (symbol->string arg))))

;; Like Ex.Gx, this becomes a mod-r/m, but with 3 extended opcode bits
;; in place of the general register code.
(define Ex-param
  (lambda (extended-opcode operand)
    (let ((symbol (operand.symbol operand))
          (size   (operand.size operand)))
      (make (default-param 1 1 symbol)
            `((unparse   ,(lambda (me) `(Ex ,size)))
              (arg-types ,(lambda (me) '("EA_Operand")))
              (examples  ,(lambda (me) (map list (Ex-examples operand))))
              (operand   ,(lambda (me) operand))
              (->gas     ,(lambda (me args) 
                            (list (Ex->gas (car args)))))
              (putter    ,(lambda (me)
                            `(mod-r/m ,extended-opcode
                                      (arg ,operand))))
              (java-put  ,(lambda (me names after)
                            (java-call "putModRM" 
                                       extended-opcode
                                       (car names))))
              (put       ,(lambda (me sink addr after args)
                            ; add check for legality of car
                            (put-mod-r/m sink 
                                         extended-opcode
                                         (car args)))))))))

(define Sx-examples
  (lambda (Sx)
    '()))                               ;FIXME

(define Gx-examples
  (lambda (Gx)
    (register-examples (operand.size Gx))))

(define Ex-examples
  (lambda (Ex)
    (append '()  ; (register-examples (operand.size Ex))
            `((at ,(expt 2 31))))))

(define register-examples
  (lambda (size)
    (possible-registers size)))

;; An ASCII-encoded string field (width depending on the argument).
(define string-param
  (lambda ()
    (make (default-param 1 0)
          `((unparse   ,(lambda (me) `(string)))
            (arg-types ,(lambda (me) '("String")))
            (examples  ,(lambda (me) '(("Sample"))))
            (->gas     ,(lambda (me args) 
                          (list (string-quotify #\" (car args)))))
            (java-put  ,(lambda (me names after)
                          (java-call "putString" (car names))))
            (put       ,(lambda (me sink addr after args)
                          (let ((str (car args)))
                            (insist "Argument type [string]" (string? str))
                            (for-each (lambda (c)
                                        (put-byte sink (char->byte c)))
                                      (string->list str))
                            (string-length str))))))))

;; An ASCII-encoded string field, null-terminated.
(define asciiz-string-param
  (lambda ()
    (make (string-param)
          `((unparse   ,(lambda (me) `(asciiz-string)))
            ; FIXME: stuff for gas, java
            (put      ,(lambda (me sink addr after args)
                          (let ((str (car args)))
                            (insist "Argument type [asciiz]" (string? str))
                            (for-each (lambda (c)
                                        (put-byte sink (char->byte c)))
                                      (string->list str))
                            (put-byte sink 0)
                            (+ 1 (string-length str)))))))))

;; A constant datum argument.
(define datum-param
  (lambda ()
    (make (default-param 1 0)
          `((unparse   ,(lambda (me) `(datum)))
            (arg-types ,(lambda (me) '("int")))
            (examples  ,(lambda (me) '((42))))
            (->gas     ,(lambda (me args)
                          (list (integer->string (car args)))))
            (java-put  ,(lambda (me names after)
                          (java-call "putBytes" 4 (car names))))
            (put      ,(lambda (me sink addr after args)
                          (let ((arg (car args)))
                            (insist "Argument type [integer]" (integer? arg)) ;FIXME
                            (put-signed-bytes sink 4 arg))))))))


;; The ASCII pseudoinstruction assembles constant strings.
(define ascii-spec
  (make-spec 'ascii (list (string-param)) "Output a literal data string" #f))

;; The ASCIIZ pseudoinstruction assembles 0-terminated constant strings.
(define asciiz-spec
  (make-spec 'asciiz
             (list (asciiz-string-param))
             "Output a 0-terminated literal data string" #f))

;; The DATA pseudoinstruction assembles one signed 4-byte data word.
(define data-spec
  (make-spec 'data (list (datum-param)) "Output a literal data word" #f))
