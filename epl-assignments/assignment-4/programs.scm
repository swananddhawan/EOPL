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
    (if (null? lst-bound-vars) (caddr expression)
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
           (eq? '* (car expression)))    (append (list (car expression))
                                                 (convert-to-curried-form (cadr expression))
                                                 (convert-to-curried-form (caddr expression))))
      
      (else                              (append (list (convert-to-curried-form (car expression)))
                                                 (list (convert-to-curried-form (cadr expression))))))))


(define return-parse-tree
  (lambda (expression)
    (let ((lst-operators '(+ *)))
     (cond
       ((symbol? expression)                    (cons 'var-exp
                                                      (list expression)))

       ((number? expression)                    (cons 'number 
                                                      (list expression)))

       ;; abstraction
       ((eq? 'lambda (car expression))          (append (list 'abstraction)
                                                        (list (return-parse-tree (cadr expression)))
                                                        (list (return-parse-tree (caddr expression)))))

       ;; if <num-op>
       ((elem? (car expression) lst-operators)  (append (list 'num-op)
                                                        (list (return-parse-tree (cadr expression)))
                                                        (list (return-parse-tree (caddr expression)))))


       ;; application
       (else                                    (append (list 'application)
                                                        (list (return-parse-tree (car expression)))
                                                        (list (return-parse-tree (cadr expression)))))))))



(define parse
  (lambda (expression)
    (if (is-expression-grammatical-correct? expression) 
      (return-parse-tree (convert-to-curried-form expression))
      #f)))
