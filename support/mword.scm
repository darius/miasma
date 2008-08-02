;;;
;;; Machine words
;;;
;;; 2's-complement ints, represented as integers between 0 and 2^bits.
;;;

(define bits 32)
(define wlimit (expt 2 bits))

(define integer->w 
  (lambda (n) (modulo n wlimit)))

(define w->integer 
  (let ((most-negative (expt 2 (- bits 1))))
    (lambda (w) 
      (if (< w most-negative)
          w
          (- (- wlimit w))))))

(define unsigned->w integer->w)
(define w->unsigned (lambda (w) w))

(define wneg (lambda (w) (integer->w (- wlimit w))))

(define w+   (lambda (w1 w2) (integer->w (+ w1 w2))))
(define w-   (lambda (w1 w2) (integer->w (- w1 w2))))

(define wu*   (lambda (w1 w2) (unsigned->w (* w1 w2))))
(define wu/   (lambda (w1 w2) (unsigned->w (quotient w1 w2))))
(define wu%   (lambda (w1 w2) (unsigned->w (remainder w1 w2))))

(define w*   (lambda (w1 w2) (integer->w (* (w->integer w1) (w->integer w2)))))
(define w/   (lambda (w1 w2) (integer->w (quotient (w->integer w1) 
                                                   (w->integer w2)))))
(define w%   (lambda (w1 w2) (integer->w (remainder (w->integer w1) 
                                                    (w->integer w2)))))

(define w=   =)
(define w<   (lambda (w1 w2) (< (w->integer w1) (w->integer w2))))
(define wu<  <)                         ;unsigned less-than
(define wu<= <=)

(define wnot (lambda (w) (integer->w (- (- wlimit 1) w))))

(define wxor
  (lambda (w1 w2)
    (cond ((= w1 0) w2)
          ((= w2 0) w1)
          (else 
           (let ((reduced (* 2 (wxor (quotient w1 2) (quotient w2 2)))))
             (if (= (remainder w1 2) (remainder w2 2))
                 reduced
                 (+ 1 reduced)))))))

(define wand
  (lambda (w1 w2)
    (if (or (= w1 0) (= w2 0))
        0
        (+ (* 2 (wand (quotient w1 2) (quotient w2 2)))
           (* (remainder w1 2) (remainder w2 2))))))

(define wor  (lambda (w1 w2) (wnot (wand (wnot w1) (wnot w2)))))

(define wshl 
  (lambda (w n) 
    (if (or (< n 0) (< 31 n))
        w-zero
        (integer->w (* w (expt 2 n))))))

(define wsrl 
  (lambda (w n) 
    (if (or (< n 0) (< 31 n))
        w-zero
        (quotient w (expt 2 n)))))

(define wsra 
  (lambda (w n) 
    (if (or (< n 0) (< 31 n))
        w-zero
        (let ((p (expt 2 n)))
          (if (w< w 0)
              (wneg (quotient (+ (wneg w) p -1) p))  ;right?
              (quotient w p))))))

(define wrol 
  (lambda (w n) 
    ; I think...
    (+ (integer->w (* w (expt 2 (modulo n bits))))
       (integer->w (quotient w (expt 2 (modulo (- n) bits)))))))

(define wror 
  (lambda (w n) 
    (wrol w (- n))))

;; Pre: (and (< 0 n-bits bits)
;;           (<= 0 n)
;;           (< n (expt 2 n-bits)))
(define sign-extend 
  (lambda (n n-bits)
    (let ((p (expt 2 (- n-bits 1))))
      (if (< n p)
          n
          (wneg (- (* 2 p) n))))))

(define w-zero (integer->w 0))
(define w-one  (integer->w 1))


