;;;
;;; A Scheme macro package.
;;;
;;; Rewrites Scheme source files using defmacro-style macros defined
;;; in a separate file.
;;;

(define macroexpand-file
  (lambda (infile outfile)
    (transduce-file infile outfile read macroexpand write-exp)))

(define define-macro
  (lambda (symbol expander)
    (set! macroexpanders
          (cons (cons symbol expander) macroexpanders))))

(define macroexpand
  (lambda (form)
    (cond ((and (pair? form)
                (symbol? (car form))
                (get-macroexpander (car form)))
           => (lambda (expander)
                (macroexpand (apply expander (cdr form)))))
          (else (walk macroexpand form)))))

(define get-macroexpander
  (lambda (symbol)
    (cond ((assq symbol macroexpanders) => cdr)
          (else #f))))

(define macroexpanders '())


;; I/O help functions

(define write-exp 
  (lambda (exp out)
    (write exp out)                     ; Stick in a prettyprinter here.
;  (pretty-print exp out)
;  (p exp out)
    (newline out)
    (newline out)))

(define transduce-file
  (lambda (infile outfile reader transducer writer)
    (call-with-input-file infile
      (lambda (in)
        (call-with-new-output-file outfile
          (lambda (out)
            (let loop ((exp (reader in)))
              (cond ((not (eof-object? exp))
                     (writer (transducer exp) out)
                     (loop (reader in)))))))))))


;; Scheme syntax.

(define (with-define exp k)
  (if (symbol? (cadr exp))
      (k (cadr exp) (caddr exp))
      (k (caadr exp) `(lambda ,(cdadr exp) ,@(cddr exp)))))

(define (with-lambda exp k)
  (k (cadr exp) (cddr exp)))

(define (with-let exp named-let-k ordinary-k)
  (if (symbol? (cadr exp))
      (named-let-k (cadr exp) (caddr exp) (cdddr exp))
      (ordinary-k (cadr exp) (cddr exp))))

(define (with-let* exp k)
  (k (cadr exp) (cddr exp)))

(define (with-letrec exp k)
  (k (cadr exp) (cddr exp)))
  
(define (with-set! exp k)
  (k (cadr exp) (caddr exp)))

(define (with-do exp k)
  (k (map (lambda (clause)              ;fill in default values
            (if (= (length clause) 3)
                clause
                (list (car clause) 
                      (if (< (length clause) 2) #f (cadr clause))
                      (if (< (length clause) 3) (car clause) (caddr clause)))))
          (cadr exp))
     (caddr exp) 
     (cdddr exp)))

(define (with-if exp k)
  (k (cadr exp) 
     (caddr exp)
     (if (null? (cdddr exp)) #f (cadddr exp))))

(define (with-cond exp k)
  (k (cdr exp)))

(define (with-case exp k)
  (k (cadr exp) (cddr exp)))

(define (with-begin exp k)
  (k (cdr exp)))


;; Return EXP with each direct subexpression E replaced by (FN E).

(define (walk fn exp)

  (define (walk-let form)
    (lambda (decls body)
      `(,form ,(map (lambda (decl) 
                      `(,(car decl) ,(fn (cadr decl))))
                    decls)
         ,@(map fn body))))

  (cond
   ((not (pair? exp)) exp)
   (else
    (case (car exp)

      ((quote)
       exp)

      ((define)
       (with-define exp
         (lambda (name value)
           `(define ,name ,(fn value)))))

      ((lambda)
       (with-lambda exp 
         (lambda (formals body)
           `(lambda ,formals ,@(map fn body)))))

      ((let)
       (with-let exp
         (lambda (proc decls body)
           (let ((new-decls 
                  (map (lambda (decl) 
                         `(,(car decl) ,(fn (cadr decl))))
                         decls))
                 (new-body (map fn body)))
             `(let ,proc ,new-decls ,@new-body)))
         (walk-let 'let)))

      ((letrec)
       (with-letrec exp (walk-let 'letrec)))

      ((let*)
       (with-let* exp (walk-let 'let*)))

      ((set!)
       (with-set! exp
         (lambda (var exp)
           `(set! ,var ,(fn exp)))))

      ((do)     
       (with-do exp
         (lambda (clauses termination body)
           `(do ,(map (lambda (clause)
                        `(,(car clause) ,(fn (cadr clause))
                                        ,(fn (caddr clause))))
                      clauses)
                ,(map fn termination)
              ,@(map fn body)))))

      ((if and or begin)
       `(,(car exp) ,@(map fn (cdr exp))))

      ((cond)
       (with-cond exp
         (lambda (clauses)
           `(cond 
             ,@(map 
                (lambda (clause)
                  (cond ((eq? (car clause) 'else)
                         `(else ,@(map fn (cdr clause))))
                        ((and (pair? (cdr clause))
                              (eq? (cadr clause) '=>))
                         `(,(fn (car clause))
                           =>
                           ,(fn (caddr clause))))
                        (else
                         (map fn clause))))
                clauses)))))

      ((case)
       (with-case exp
         (lambda (test-exp clauses)
           `(case ,(fn test-exp)
              ,@(map (lambda (clause)
                       `(,(car clause) ,@(map fn (cdr clause))))
                     clauses)))))

      ((quasiquote)
       (list 'quasiquote (walk-quasiquote fn (cadr exp))))

      (else         ; application
       (cons (fn (car exp))
             (map fn (cdr exp))))))))

;;FIXME: doesn't handle nested quasiquotes.

(define (walk-quasiquote fn form)
  (cond
   ((not (pair? form)) form)
   (else
    (case (car form)
      ((unquote unquote-splicing)
       (list (car form) (fn (cadr form))))
      (else (cons (walk-quasiquote fn (car form))
                  (walk-quasiquote fn (cdr form))))))))
