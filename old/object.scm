;;;
;;; A very simple prototype-based object system.
;;;

;; Return a generic function named MESSAGE.  Generic functions always
;; take a self object as their first argument.
(define make-generic-function
  (lambda (message)
    (lambda (object . args)
      (apply (object message) (cons object args)))))

;; Return a new object that knows the messages in METHODS (an a-list
;; from message names to functions) and delegates unknown messages to
;; SUPER.
(define make
  (lambda (super methods)
    (insist "Method table is well-formed"
	    (all (lambda (entry)
		   (and (pair? entry)
			(symbol? (car entry))
			(pair? (cdr entry))
			(procedure? (cadr entry))
			(null? (cddr entry))))
		 methods))
    (lambda (message)
      (cond ((assq message methods) => cadr)
	    (super (super message))
	    (else (panic "Message not understood" message))))))
