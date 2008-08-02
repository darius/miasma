; Miasma: x86 Scheme Assembler.
; Copyright (C) 2000, Darius Bacon, Brian Spilsbury, Alycat
; Refer to legal/License.txt

;;;
;;; Sinks
;;;

(define make-fake-sink
  (lambda ()
    (lambda (byte) byte)))

(define make-sink 
  (lambda (port)
    (lambda (byte)
      (write-char (byte->char byte) port))))


;;;
;;; Bytes and words
;;;
;;; These return the count of bytes output.
;;;

;; unsigned
(define put-byte
  (lambda (sink byte)
    (sink byte)
    1))

;; little-endian, unsigned
(define put-bytes
  (lambda (sink count word)
    (let loop ((k count) (w (or word 0)))
      (cond ((= 0 k)
             (if (not (= 0 w))
                 (panic "Value out of range" word))
             count)
            (else
             (sink (remainder w 256))
             (loop (- k 1) (quotient w 256)))))))

(define put-signed-bytes
  (lambda (sink count word)
    (put-bytes sink count (signed->unsigned (* 8 count) word))))
