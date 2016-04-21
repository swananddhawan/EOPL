(load "../assignment-3/programs.scm")

(define normal-add
  (lambda (x y)
    (+ x y)))

(define curried-add
  (lambda (x)
    (lambda (y)
      (+ x y))))

;(normal-add 3 5)

;; written this because i was not able to pass `and` as parameter to myfold
(define my-and
  (lambda (a b)
    (and a b)))

(define myfold
  (lambda (function accumulator lst)
    (if (null? lst) accumulator
      (myfold function (function accumulator (car lst)) (cdr lst)))))


(define is-expression-grammatical-correct?
  (lambda (expression)
     (let ((lst-operators '(+ *)))
      (cond
        ((null? expression)                      #f)
        
        ((symbol? expression)                    #t)

        ((number? expression)                    (not (negative? expression)))
                                                 
                                                 ;; check if parameters are symbols only  &
        ;; abstraction                           ;; the body of abstraction is grammatically correct
        ((and (= (length expression) 3) 
              (eq? 'lambda (car expression)))    (and (myfold my-and #t (map symbol? (cadr expression)))
                                                      (is-expression-grammatical-correct? (caddr expression))))
      
        ;; num-op
        ((elem? (car expression) lst-operators)  (if (not (= (length (cdr expression)) 2)) #f
                                                      (and (is-expression-grammatical-correct? (cadr expression))
                                                           (is-expression-grammatical-correct? (caddr expression)))))

        
        ;; application
        ((= (length expression) 2)               (and (is-expression-grammatical-correct? (car expression))
                                                      (is-expression-grammatical-correct? (cadr expression))))
        
        (else                                    #f)))))

        

(define convert-lambda-abs-to-curried-form
  (lambda (expression lst-bound-vars)
    (if (null? lst-bound-vars) (convert-to-curried-form (caddr expression))
      (append (cons 'lambda
                    (list (car lst-bound-vars)))
              (list (convert-lambda-abs-to-curried-form expression (cdr lst-bound-vars)))))))



(define convert-to-curried-form
  (lambda (expression)
    (cond 
      ((symbol? expression)              expression)
      
      ((number? expression)              expression)
      
      ((eq? 'lambda (car expression))    (convert-lambda-abs-to-curried-form expression 
                                                                            (cadr expression)))
      
      ((or (eq? '+ (car expression))
           (eq? '* (car expression)))    (list (car expression)
                                               (convert-to-curried-form (cadr expression))
                                               (convert-to-curried-form (caddr expression))))
      
      (else                              (append (list (convert-to-curried-form (car expression)))
                                                 (list (convert-to-curried-form (cadr expression))))))))


(define return-parse-tree
  (lambda (expression)
    (let ((lst-operators '(+ *)))
     (cond
       ((symbol? expression)                    (cons 'var-ref
                                                      (list expression)))

       ((number? expression)                    (cons 'number 
                                                      (list expression)))

       ;; abstraction
       ((eq? 'lambda (car expression))          (append (list 'abs)
                                                        (list (return-parse-tree (cadr expression))
                                                              (return-parse-tree (caddr expression)))))

       ;; if <num-op>
       ((elem? (car expression) lst-operators)  (cons 'num-op 
                                                      (list (list (car expression)
                                                            (return-parse-tree (cadr expression))
                                                            (return-parse-tree (caddr expression))))))


       ;; application
       (else                                    (append (list 'app)
                                                        (list (return-parse-tree (car expression))
                                                              (return-parse-tree (cadr expression)))))))))


(define parse
  (lambda (expression)
    (if (is-expression-grammatical-correct? expression) 
      (return-parse-tree (convert-to-curried-form expression))
      #f)))


(define lst '(lambda (x) (+ x y)))
(define app-lst '((lambda (x y) (+ x y)) (a b)))


;; I have assumed that the values for free variables will be present 
;; in the environment

;; ************ handle the error if not found *************
(define basic-lookup
  (lambda (symbol environment)
    (if (eq? symbol (caar environment))
      (cadar environment)
      (basic-lookup symbol (cdr environment)))))


(define my-lookup
  (lambda (parse-tree environment lst-bound-vars)
    (let ((first-element (car parse-tree)))
     (cond
       
       ;; (var-ref a)
       ((and (eq? 'var-ref first-element)
             (not (elem? (cadr parse-tree) 
                         lst-bound-vars)))        (basic-lookup (cadr parse-tree)
                                                                      environment))

       
       ;; (abs (var-ref x) (body))
       ((eq? 'abs first-element)                  (append (list 'abs)
                                                          (list (cadr parse-tree)
                                                                (my-lookup (caddr parse-tree)
                                                                           environment
                                                                           (cons (cadadr parse-tree)
                                                                                 lst-bound-vars)))))
       
       
       ;; (app (left) (right))
       ((eq? 'app first-element)                  (cons 'app
                                                        (list (my-lookup (cadr parse-tree)
                                                                         environment
                                                                         lst-bound-vars)
                                                              (my-lookup (caddr parse-tree)
                                                                         environment
                                                                         lst-bound-vars))))
       
       
       ;; (num-op (+ (left) (right)))
       ((eq? 'num-op first-element)               (cons 'num-op
                                                        (list (list (caadr parse-tree)
                                                                    (my-lookup (cadadr parse-tree)
                                                                               environment
                                                                               lst-bound-vars)
                                                                    (my-lookup (car (cddadr parse-tree))
                                                                               environment
                                                                               lst-bound-vars)))))
                                                              

       ;; (number n)
       (else                                      parse-tree)))))
                                                     

(define lookup
  (lambda (parse-tree environment)
    (my-lookup parse-tree
               environment
               '())))
