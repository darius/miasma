; Miasma: x86 Scheme Assembler.
; Copyright (C) 2000, Darius Bacon, Brian Spilsbury, Alycat
; Refer to legal/License.txt

(define <<
  (lambda (n bits)
    (* n (expt 2 bits))))

(define signed-constant-examples
  (lambda (bits)
    (let ((b (expt 2 (- bits 1))))
      (list (- b) (- b 1)))))

(define unsigned-constant-examples
  (lambda (bits)
    (let ((b (expt 2 bits)))
      (list (/ b 2) (- b 1)))))

(define fits-in-signed?
  (lambda (bits k)
    (and (integer? k)
	 (let ((p (expt 2 (- bits 1))))
	   (<= (- p) k (- p 1))))))

(define fits-in-unsigned?
  (lambda (bits k)
    (and (integer? k)
	 (let ((p (expt 2 bits)))
	   (<= 0 k (- p 1))))))

(define signed->unsigned
  (lambda (bits k)
    (if (<= 0 k) 
	k
	(let ((unsigned (+ (expt 2 bits) k)))
	  (if (< unsigned 0)
	      (panic "Value out of range" k))
	  unsigned))))

;; Pre: b is a multiple of 8
(define bits->bytes
  (lambda (b)
    (/ b 8)))

;; Return true iff X is an unsigned byte.
(define byte?
  (lambda (x)
    (and (integer? x)
	 (fits-in-unsigned? 8 x))))
