(define (walk-code code code-f exp-f)
  (let walking ((code code))
    (let ((rator (car code))
          (rands (cdr code))
          (f (code-f (car code))))
      (case rator
        ((bytes)
         (f (case (car rands) ((i) #t) ((u) #f))
            (cadr rands)
            (walk-exp (caddr rands) exp-f)))
        ((swap-args)
         (f (walking (car rands))))
        ((mod-r/m)
         (f (walk-exp (car rands) exp-f)
            (walk-exp (cadr rands) exp-f)))
        (else (impossible))))))

(define (walk-exp exp exp-f)
  (let walking ((exp exp))
    (cond
     ((integer? exp)
      ((exp-f 'literal) exp))
     (else
      (let ((rator (car exp))
            (rands (cdr exp)))
        (case rator
          ((+ -)
           ((exp-f 'op) rator
                        (walking (car rands))
                        (walking (cadr rands))))
          ((hereafter)
           ((exp-f rator)))
          ((arg)
           (apply (exp-f rator) rands))
          (else (impossible))))))))


(define (unit v)
  (lambda (args k)
    (k args v)))

(define (bind m2-proc m1)
  (lambda (state k)
    (m1 state
        (lambda (state v1)
          ((m2-proc v1) state k)))))

(define (swapping m)
  (lambda (args k)
    (m `(,(cadr args) ,(car args) ,@(cddr args))
       k)))

(define (eating m-proc)
  (lambda (args k)
    ((m-proc (car args)) (cdr args)
                         k)))
