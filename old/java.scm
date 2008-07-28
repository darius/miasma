; Miasma: x86 Scheme Assembler.
; Copyright (C) 2000, Darius Bacon, Brian Spilsbury, Alycat
; Refer to legal/License.txt

;;;
;;; Generate Java code sinks
;;;

(define write-java-sinks
  (lambda ()
    (for-each write-java-sink the-specs)))

(define write-java-sink
  (lambda (spec)
    (let ((stem (spec.stem spec))
	  (params (spec.params spec)))
      (let* ((types (flatmap java-arg-types params))
	     (names (java-arg-names types)))
	(say "void " (make-java-mnemonic stem params) 
	     "(" (commaify (map java-decl types names)) ")")
	(say "{")
	(write-java-param-sinks params (distribute-args params names))
	(say "}")))))

; FIXME: code copied from assemble-params
(define write-java-param-sinks
  (lambda (params arglists)
    ; This assumes that params that need to know how many bytes follow
    ; them are not followed by a variable-length param (that's why we
    ; can get away with using static bytes after each, for the actual
    ; count of bytes after each).
    (let ((afters (static-bytes-after-each params)))
      (for-each java-sink params arglists afters))))

(define java-arg-names
  (lambda (types)
    (map (lambda (type k)
	   (string-append (string (char-downcase (string-ref type 0)))
			  (number->string k)))
	 types
	 (iota (length types)))))

(define java-decl
  (lambda (type name)
    (string-append type " " name)))


;; (string param-list) -> symbol
;; Return an unambiguous mnemonic, given an overloaded one and the
;; params that resolve the overloading.
;; FIXME: code copied from make-mnemonic
(define make-java-mnemonic
  (lambda (stem params)
    (string->symbol
     (string-join `("x86" 
		    ,(as-legal-java-identifier stem)
		    ,@(map as-legal-java-identifier
			   (flatmap name-suffix
				    params)))
		  "_"))))

;; (string) -> string
;; Return STR, but munging out any characters that are used in our 
;; mnemonics but aren't legal in Java identifiers.
(define as-legal-java-identifier
  (lambda (str)
    (list->string
     (map (lambda (c)
	    (case c
	      ((#\-) #\_)
	      ((#\?) #\c)
	      (else c)))
	  (filter (lambda (c) (not (memq c '(#\: #\%))))
		  (string->list str))))))
