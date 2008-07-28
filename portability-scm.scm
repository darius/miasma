; Vapour: Compiler/OS
; Copyright (C) 2000, Darius Bacon, Brian Spilsbury, Alycat
; Refer to legal/License.txt

;;;
;;; Portability defs for MzScheme
;;;

(define (pretty-print x)		;FIXME: find the mz prettyprinter
  (write x)
  (newline))

(define panic error)

(define char->ascii char->integer)
(define byte->char integer->char)
(define char->byte char->integer)

;; Like isprint() in C.
(define char-printable?
  (let ((space (char->integer #\space))
	(tilde (char->integer #\~)))
    (lambda (c)
      (<= space (char->integer c) tilde))))

;; This has an obvious race condition...
(define call-with-new-output-file 
  (lambda (filename receiver)
    (delete-file filename)
    (call-with-output-file filename receiver)))

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

;; Call RECEIVER with the time it took to compute (PROC), and the result.
; FIXME: actually implement it...
(define timex
  (lambda (proc receiver)
    (receiver 0 (proc))))
