; Vapour: Compiler/OS
; Copyright (C) 2000, Darius Bacon, Brian Spilsbury, Alycat
; Refer to legal/License.txt

;;;
;;; Portability defs for UTS
;;;

(define panic
  (lambda x
    (@error x)))

(define error panic)

(define char->ascii char->integer)
(define byte->char integer->char)
(define char->byte char->integer)

;; Like isprint() in C.
(define char-printable?
  (let ((space (char->integer #\space))
	(tilde (char->integer #\~)))
    (lambda (c)
      (<= space (char->integer c) tilde))))

(define call-with-new-output-file call-with-output-file)

;; In 'doze, change to backslash.
(define file-separator "/")

;; Return a string denoting a file with the given directory/filename
;; components on this system.
(define make-pathname
  (lambda (component . components)
    (apply string-append
	   (cons component
		 (apply append
			(map (lambda (c) (list file-separator c))
			     components))))))

;; This isn't actually used in the code -- it's included only because
;; it's handy for benchmarking stuff.

;; Call RECEIVER with the time it took to compute (PROC), and the result.
(define timex
  (lambda (proc receiver)
    (let* ((start (@runtime))
	   (value (proc))
	   (interval (- (@runtime) start)))
      (receiver interval value))))

(define pretty-print 
  (lambda (x) (print x)))
