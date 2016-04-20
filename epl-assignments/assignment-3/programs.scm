(load "../assignment-2/programs.scm")
;; lambda expression in BNF form
;; LambdaExpression := identifier
;;                  := (lambda (identier) LambdaExpression)
;;                  := (LambdaExpression LambdaExpression)

;; (let ((a 1) (b 2)) (+ a b))

;; (let ((a 1)
;;       (b 2))
;;      (+ a b))

;(((lambda (a b)
    ;(+ a b))
  ;2)
 ;1)

(define extract-first-element
  (lambda (lst)
    (car lst)))

(define get-applications
  (lambda (lst)
    (map extract-first-element (map reverse-list lst))))

(define append-application-to-expression-one-at-a-time
  (lambda (list-of-applications expression)
    (if (null? list-of-applications)
      expression
      (append-application-to-expression-one-at-a-time (cdr list-of-applications)
                                                      (append (list expression)
                                                              (list (car list-of-applications)))))))

(define let-transform
  (lambda (let-expression)
    (append-application-to-expression-one-at-a-time (map car (cadr let-expression))
                                                    (append (append (list 'lambda)
                                                                    (list (map car (cadr let-expression))))
                                                            (cddr let-expression)))))


(define substitute-list-of-symbols
  (lambda (list-of-old-symbols list-of-new-symbols lst)
    (if (or (null? list-of-old-symbols) (null? lst))
      lst
      (substitute-list-of-symbols (cdr list-of-old-symbols)
                                  (cdr list-of-new-symbols)
                                  (substitute (car list-of-old-symbols)
                                              (car list-of-new-symbols)
                                              lst)))))

(define split-two-list
  (lambda (list-of-2-list)
    (cons (map car list-of-2-list) (map car (map cdr list-of-2-list)))))

(define _count-occurences
  (lambda (symbol list-of-nested-symbol count)
    (if (list? (car list-of-nested-symbol))
      (+ (_count-occurences symbol (car list-of-nested-symbol) count)
         (_count-occurences symbol (cdr list-of-nested-symbol) count))

      (if (eq? symbol (car list-of-nested-symbol))
        (_count-occurences symbol (cdr list-of-nested-symbol) (+ 1 count))
        (_count-occurences symbol (cdr list-of-nested-symbol) count)))))

(define count-occurences
  (lambda (symbol list-of-nested-symbol)
    (if (null? list-of-nested-symbol) list-of-nested-symbol
      (_count-occurences symbol list-of-nested-symbol 0))))

(define path
  (lambda (n bst)
    (cond ((null? bst)       bst)
          ((eq? (car bst) n) '())
          ((< (car bst) n)   (append (list 'right) (path n (caddr bst))))
          (else              (append (list 'left) (path n (cadr bst)))))))

(define free?
  (lambda (symbol lambda-expression)
    (cond
      ((symbol? lambda-expression) (eqv? symbol lambda-expression))
      ((eqv? (car lambda-expression) 'lambda)
       (and
         (not (eqv? symbol (car (cadr lambda-expression))))
         (free? symbol (caddr lambda-expression))))
      (else
        (or
          (free? symbol (car lambda-expression))
          (free? symbol (cadr lambda-expression)))))))



(define bound?
  (lambda (symbol lambda-expression)
    (cond
      ((symbol? lambda-expression) #f)
      ((eqv? 'lambda (car lambda-expression))
       (or (bound? symbol (caddr lambda-expression))
           (and
             (eqv? symbol (caadr lambda-expression))
             (free? symbol (caddr lambda-expression)))))
      (else
        (or
          (bound? symbol (car lambda-expression))
          (bound? symbol (cadr lambda-expression)))))))



(define ex1 '((lambda (x) (x y)) (lambda (y) (x z))))

(define return-free-set-of-variables
  (lambda (lambda-expression)
    (cond
      ((null? lambda-expression) lambda-expression)
      ((number? lambda-expression) '())
      ((symbol? lambda-expression) (list lambda-expression))
      ((eqv? 'lambda (car lambda-expression))
       (remove-all (caadr lambda-expression) (return-free-set-of-variables (caddr lambda-expression))))
      (else
        (append (return-free-set-of-variables (car lambda-expression))
                (return-free-set-of-variables (cadr lambda-expression)))))))

(define elem?
  (lambda (symbol lst)
    (cond
      ((null? lst) #f)
      ((eqv? symbol (car lst)) #t)
      (else (elem? symbol (cdr lst))))))


(define remove-duplicates
  (lambda (lst)
    (cond
      ((null? lst) lst)
      ((elem? (car lst) (cdr lst)) (remove-duplicates (cdr lst)))
      (else (cons (car lst) (remove-duplicates (cdr lst)))))))

(define free-set
  (lambda (lambda-expression)
    (remove-duplicates (return-free-set-of-variables lambda-expression))))


(define return-bound-set-of-variables
  (lambda (expression)
    (cond
      ((null? expression) '())
      ((symbol? expression) '())
      ((eqv? 'lambda (car expression))
       (append (cadr expression)
               (return-bound-set-of-variables (caddr expression))))
      (else
        (append (return-bound-set-of-variables (car expression))
                (return-bound-set-of-variables (cadr expression)))))))

(define bound-set
  (lambda (lambda-expression)
    (remove-duplicates (return-bound-set-of-variables lambda-expression))))



(define lexp '(lambda (x y) (lambda (z) (x (y z))) x))

(define add-symbol-list-to-env
  (lambda (lsymbols env)
    (define helper-add-symbol-list-to-env
      (lambda (lsymbols position env)
        (if (null? lsymbols) env
          (helper-add-symbol-list-to-env (cdr lsymbols)
                                         (+ 1 position)
                                         (cons (list (car lsymbols) 0 position)
                                               env)))))
    (helper-add-symbol-list-to-env lsymbols 0 env)))


(define increment-depth-of-symbol
  (lambda (tuple)
    (append (cons (car tuple) (list (succ (cadr tuple))))
            (cddr tuple))))


(define get-lambda-with-number-of-parameters
  (lambda (lst)
    (append (list 'lambda)
            (list (list (length lst))))))


(define convert-name-to-address
  (lambda (symbol env)
    (cond
      ((null? env) symbol)
      ((eq? symbol (caar env)) (cdar env))
      (else        (convert-name-to-address symbol (cdr env))))))


(define get-lexical-address
  (lambda (lexp env)
    (cond
      ((null? lexp) lexp)

      ((eqv? 'lambda (car lexp))
       (append (get-lambda-with-number-of-parameters (cadr lexp))
               (get-lexical-address (cddr lexp)
                                    (add-symbol-list-to-env (cadr lexp)
                                                            (map increment-depth-of-symbol env)))))

      ((symbol? (car lexp))
       (cons
         (convert-name-to-address (car lexp) env)
         (get-lexical-address (cdr lexp) env)))

      ((list? lexp) (cons
                      (get-lexical-address (car lexp) env)
                      (get-lexical-address (cdr lexp) env))))))

(get-lexical-address lexp '())

(define remove-first-n-elements
  (lambda (n lst)
    (if (or (null? lst) (= n 0)) lst
      (remove-first-n-elements (pred n) (cdr lst)))))

(define two-list?
  (lambda (lst)
    (and (number? (car lst)) (number? (cadr lst)))))

(define get-first-n-elements
  (lambda (n lst)
    (if (or (= n 0) (null? lst)) '()
      (cons (car lst) (get-first-n-elements (pred n) (cdr lst))))))

(define get-lambda-with-formal-parameters
  (lambda (n l-formal-parameters)
    (append (list 'lambda) (list (get-first-n-elements n l-formal-parameters)))))

;; if it is 2-list, then should you always be able to find it in the env??
(define lookup-symbol-name
  (lambda (two-list env)
    (cond
      ((null? env) two-list)
      ((equal? (cdar env) two-list) (caar env))
      (else (lookup-symbol-name two-list (cdr env))))))


(define un-lexical-address
  (lambda (lexp l-formal-parameters env)
    (cond
      ((null? lexp) lexp)

      ((eq? 'lambda (car lexp))
       (append
         (get-lambda-with-formal-parameters (caadr lexp) l-formal-parameters)
         (un-lexical-address (cddr lexp)
                             (remove-first-n-elements (caadr lexp) l-formal-parameters)
                             (add-symbol-list-to-env (get-first-n-elements (caadr lexp) l-formal-parameters)
                                                     (map increment-depth-of-symbol env)))))

      ((two-list? (car lexp)) (cons (lookup-symbol-name (car lexp) env)
                                    (un-lexical-address (cdr lexp) l-formal-parameters env)))

      ((list? lexp) (cons
                      (un-lexical-address (car lexp) l-formal-parameters env)
                      (un-lexical-address (cdr lexp) l-formal-parameters env))))))

(define op (get-lexical-address lexp '()))

(define lst-parms '(x y z))

(un-lexical-address op lst-parms '())
