;(with f ((var val)...) body...)
;==> (f (lambda (var...) body...) val...)
(define-macro 'with
  (lambda (f pairs . body)
    `(,f (lambda ,(map car pairs) ,@body)
         ,@(map cadr pairs))))


; (flambda ((tag . args) body ...) ...)
; (lambda (x) (case x ((tag) (lambda args body...)) ... (else (impossible))))
; modulo hygiene
(define-macro 'flambda
  (lambda cases
    (let ((vars (map (lambda (_) (generate-symbol 'flambda))
                     cases)))
      `(let ,(map (lambda (var pair)
                    `(,var (lambda ,(cdar pair) ,@(cdr pair))))
                  vars
                  cases)
         (lambda (tag)
           (case tag
             ,@(map (lambda (var pair)
                      `((,(caar pair)) ,var))
                    vars
                    cases)
             (else (impossible 'flambda tag))))))))



;; Return a new symbol with a prefix of NAME, different from all
;; others generated so far.
(define generate-symbol
  (lambda (name)
    (set! generate-symbol-counter (+ generate-symbol-counter 1))
    (concat-symbol name '- generate-symbol-counter)))

(define generate-symbol-counter 0)
