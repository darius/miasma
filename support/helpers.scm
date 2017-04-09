;;;
;;; Miscellaneous help functions
;;;

;; Return the smallest i where (string-ref STR i) = CH,
;; or #f if none.
(define char-index
  (lambda (str ch)
    (let ((L (string-length str)))
      (let loop ((i 0))
        (cond ((= i L) #f)
              ((char=? ch (string-ref str i)) i)
              (else (loop (+ i 1))))))))

;; Return string STR with special characters replaced by escape
;; sequences.  DELIMITER is one such special character.
(define string-escapify
  (lambda (delimiter str)
    (define escapify-one
      (lambda (c accum)
        (cond ((char=? c delimiter)
               `(#\\ ,c ,@accum))
              ((assv c '((#\newline #\\ #\n)
                         (#\tab     #\\ #\t)))
               => (lambda (pair) (append (cdr pair) accum)))
              ((char-printable? c)
               (cons c accum))
              (else
               `(#\\ #\x
                     ,@(string->list (number->string (char->integer c) 16))
                     ,@accum)))))
    (list->string (foldr escapify-one '() (string->list str)))))

;; Return string STR escapified and surrounded by the character DELIMITER.
(define string-quotify
  (lambda (delimiter str)
    (let ((q (string delimiter)))
      (string-append q (string-escapify delimiter str) q))))

;; Display a line to PORT, the concatenation of ARGS.
(define say-to
  (lambda (port . args)
    (for-each (lambda (arg) (display arg port))
              args)
    (newline port)))

;; SAY-TO stdout.
(define say
  (lambda args
    (apply say-to (cons (current-output-port) args))))

;; Add a pair to an association list.
(define acons
  (lambda (x y a-list)
    (cons (cons x y) a-list)))

;; Return a symbol whose name is the concatenation of ATOMS.
(define concat-symbol
  (lambda atoms
    (string->symbol 
     (foldr string-append "" (map coerce-string atoms)))))

;; Return a string that looks like X, an atom.
(define coerce-string
  (lambda (x)
    (cond ((symbol? x) (symbol->string x))
          ((string? x) x)
          ((integer? x) (integer->string x))
          ((number? x) (number->string x))
          (else (impossible)))))

(define identity (lambda (x) x))

;; Return the Kth cdr of list LS, or #f if there is none.
(define clamped-tail
  (lambda (ls k)
    (cond ((= k 0) ls)
          ((pair? ls) (clamped-tail (cdr ls) (- k 1)))
          (else #f))))

;; I wonder why the hell aly wants all this, but okay, here it is:
;; Return true iff (length LS) is =, >=, etc., to K.
(define length=?  (lambda (ls k) (null? (clamped-tail ls k))))
(define length>=? (lambda (ls k) (not (not (clamped-tail ls k)))))
(define length>?  (lambda (ls k) (pair? (clamped-tail ls k))))
(define length<=? (lambda (ls k) (not (pair? (clamped-tail ls k)))))
(define length<?  (lambda (ls k) (not (clamped-tail ls k))))

(define memq?     (lambda (x ls) (not (not (memq x ls)))))      
      
;; Return the composition of single-argument functions F and G.
(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

;; Return the complement of single-argument predicate F.
(define complement
  (lambda (f)
    (compose not f)))

;; Split LS at its first element where SPLIT-POINT? is true.
;; That is, return (RECEIVER head tail), where (append head tail) = LS,
;; and (SPLIT-POINT? h) is false for all h in head, and
;; either tail is () or (SPLIT-POINT? (car tail)) is true.
(define split-on
  (lambda (split-point? ls receiver)
    (if (or (null? ls)
            (split-point? (car ls)))
        (receiver '() ls)
        (split-on split-point?
                  (cdr ls)
                  (lambda (head tail)
                    (receiver (cons (car ls) head) 
                              tail))))))

;; This needs a better name -- and the sense of the predicate is
;; opposite that of split-on...
(define eat
  (lambda (include? ls receiver)
    (if (include? (car ls))
        (receiver (car ls) (cdr ls))
        (receiver #f       ls))))

;; The standard fold operator on lists.
(define foldr
  (lambda (fn id lst)
    (let folding ((lst lst))
      (if (null? lst)
          id
          (fn (car lst)
              (folding (cdr lst)))))))

;; Fold from the left instead of the right.
(define foldl
  (lambda (fn id lst)
    (let folding ((lst lst) (id id))
      (if (null? lst)
          id
          (folding (cdr lst)
                   (fn (car lst) id))))))

;; Like mapcan in Common Lisp.
(define flatmap
  (lambda (fn ls)
    (foldr append '() (map fn ls))))

;; Return true iff every element of (map TEST? LS) is true.
(define all
  (lambda (test? ls)
    (if (null? ls)
        #t
        ; We treat the last element specially for the sake of tail recursion.
        (let testing ((first (car ls)) (rest (cdr ls)))
          (if (null? rest)
              (test? first)
              (and (test? first)
                   (testing (car rest) (cdr rest))))))))

;; Return the first true result from mapping TEST? over LS,
;; or #f if none.
(define any
  (lambda (test? ls)
    (if (null? ls)
        #f
        (let testing ((first (car ls)) (rest (cdr ls)))
          (if (null? rest)
              (test? first)
              (or (test? first)
                  (testing (car rest) (cdr rest))))))))

;; Return the first element of LS for which TEST? is true, or #f if none.
(define find
  (lambda (test? ls)
    (let finding ((ls ls))
      (cond ((null? ls)
             #f)
            ((test? (car ls))
             (car ls))
            (else (finding (cdr ls)))))))

;; Return all elements of LS for which TEST? is true, in order.
(define filter
  (lambda (test? ls)
    (let loop ((ls ls))
      (cond ((null? ls)
             '())
            ((test? (car ls))
             (cons (car ls) (loop (cdr ls))))
            (else (loop (cdr ls)))))))

;; Return a copy of list LS with any elements eq? to OBJ deleted.
(define delq
  (lambda (obj ls)
    (filter (lambda (x) (not (eq? x obj)))
            ls)))

;; Return a list of the s-exprs in FILE.
(define snarf
  (lambda (file)
    (call-with-input-file file
      (lambda (port)
        (let loop ((exps '()))
          (let ((exp (read port)))
            (if (eof-object? exp)
                (reverse exps)
                (loop (cons exp exps)))))))))

;; Pre: SMALL is eq? to some tail of BIG.
;; Return the head of BIG before that tail.
(define list-difference
  (lambda (big small)
    (let loop ((ls big))
      (if (eq? ls small)
          '()
          (cons (car ls)
                (loop (cdr ls)))))))

;; Pre: XSS is a list of lists.
;; Return a list of all lists whose first element is in (car XSS),
;; second element is in (cadr XSS), etc.
(define outer-product*
  (lambda (xss)
    (foldr outer-product '(()) xss)))

;; Pre: XS and YS are lists.
;; Return a list of all pairs (x . y) where x in XS and y in YS.
(define outer-product
  (lambda (xs ys)
    (flatmap (lambda (x)
               (map (lambda (y) (cons x y)) ys))
             xs)))

;; Pre: 0 <= COUNT <= (length LS)
;; Return a list of the first COUNT elements of LS in order.
(define list-head
  (lambda (ls count)
    (if (= count 0)
        '()
        (cons (car ls)
              (list-head (cdr ls) (- count 1))))))

;; Call PROC with each element of VEC, in index order.
(define vector-for-each
  (lambda (vec proc)
    (let ((limit (vector-length vec)))
      (do ((k 0 (+ k 1)))
          ((= k limit))
        (proc (vector-ref vec k))))))

;; Return list LS minus its Nth element (counting from 0).
(define remove-nth
  (lambda (ls n)
    (if (= 0 n)
        (cdr ls)
        (cons (car ls) 
              (remove-nth (cdr ls) (- n 1))))))

;; Return list LS minus its first element equal? to ELEM.
(define remove
  (lambda (elem ls)
    (let loop ((ls ls))
      (cond ((null? ls) '())
            ((equal? (car ls) elem) (cdr ls))
            (else (cons (car ls) (loop (cdr ls))))))))

;; Return list LS minus its last element.
;; Pre: 0 < (length LS)
(define butlast
  (lambda (ls)
    (remove-nth ls (- (length ls) 1))))

;; Return the last element of list LS.
;; Pre: 0 < (length LS)
(define last
  (lambda (ls)
    (if (null? (cdr ls))
        (car ls)
        (last (cdr ls)))))

;; Call (PROC) COUNT times.
(define dotimes
  (lambda (count proc)
    (do ((c count (- c 1)))
        ((<= c 0))
      (proc))))

;; True iff OBJ is a list starting with KEY.
(define starts-with?
  (lambda (key obj)
    (and (pair? obj)
         (eq? key (car obj)))))

;; Return the list of integers 1..n.
(define iota
  (lambda (n)
    (let counting ((i n) (ls '()))
      (if (<= i 0)
          ls
          (counting (- i 1) (cons i ls))))))

;; Needed because a lame Scheme like UTS might not have 32-bit ints.
;; Pre: n is an integer.
(define integer->string
  (lambda (n)
    (trim-trailing (number->string n) #\.)))

;; Return string STR with any instances of CHAR at the end removed.
(define trim-trailing
  (lambda (str char)
    (do ((i (string-length str) (- i 1)))
        ((or (= i 0)
             (not (char=? char (string-ref str (- i 1)))))
         (substring str 0 i)))))

;; Pre: STRINGS is a list of strings, BETWEEN is a string.
;; Return the concatenation of the STRINGS with BETWEEN between
;; each successive element.
(define string-join
  (lambda (strings between)
    (if (null? strings)
        ""
        (list->string
         (let ((tween (string->list between)))
           (let appending ((ls strings))
             (let ((head (string->list (car ls))))
               (if (null? (cdr ls))
                   head
                   (append head tween (appending (cdr ls)))))))))))

;; Pre: IN and OUT are input and output ports.
;; Copy all remaining characters from IN to OUT.
(define copy-port
  (lambda (in out)
    (let copying ()
      (let ((char (read-char in)))
        (cond ((not (eof-object? char))
               (write-char char out)
               (copying)))))))
 

;;;
;;; Testing/debugging
;;;

(define impossible
  (lambda args
    (apply panic (cons "Impossible" args))))

(define (print x) 
  (write x) 
  (newline))

(define (make-traced tag f)
  (lambda args
    (print `(entering ,tag ,@args))
    (let ((result (apply f args)))
      (print `(exiting ,tag : ,result))
      result)))

(define expect 
  (lambda (result fn . args)
    (let ((actual (apply fn args)))
      (if (not (equal? actual result))
          (panic "Test failed" (cons fn args) actual "but expected" result)))))

(define insist
  (lambda (message ok?)
    (if (not ok?)
        (panic "Insist failed for:" message))))
