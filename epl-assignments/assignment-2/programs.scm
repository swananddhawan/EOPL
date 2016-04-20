(load "../assignment-1/programs.scm")
;; 1a)

;; my-length list = 0  ..... if null list
;; my-length x:list = 1 + my-length list

;; 1b)
(define my-length
        (lambda (l)
                (if (null? l)
                        0
                    (+ 1 (my-length (cdr l))))))

;; 1c)
(define my-append
        (lambda (l1 l2)
                (if (null? l1)
                        l2
                    (cons (car l1) (my-append (cdr l1) l2)))))


;; 1d)
(define my-flatten
	(lambda (l)
		(if (null? l)
			l
		    (if (list? (car l))
		    	     (append (my-flatten (car l)) (my-flatten (cdr l)))
			(cons (car l) (my-flatten (cdr l)))
		    )
		)
	)
)

;; 1e)
(define reverse-list
	(lambda (l)
		(if (null? l)
			l
		    (my-append (reverse-list (cdr l)) (list (car l)))
		)
	)
)

;; 1f
(define get-first-occurence-index
	(lambda (symbol list-of-symbols index)
		(if (null? list-of-symbols)
			-1
		    (if (eq? symbol (car list-of-symbols))
			     index
			(get-first-occurence-index symbol (cdr list-of-symbols) (+ index 1))
		    )
		)
	)
)

(define list-index
	(lambda (symbol list-of-symbols)
		(get-first-occurence-index symbol list-of-symbols 0)
	)	
)


;; 1g)
(define list-all-indices
	(lambda (symbol list-of-symbols index)
		(if (null? list-of-symbols)
			list-of-symbols
		    (if (eq? symbol (car list-of-symbols))
		    	    (cons index (list-all-indices symbol (cdr list-of-symbols) (+ 1 index)))
			(list-all-indices symbol (cdr list-of-symbols) (+ 1 index))    
		    )
		)
	)
)
(define list-indices
	(lambda (symbol list-of-symbols)
		(list-all-indices symbol list-of-symbols 0)
	)
)

;;1h)
(define invert-list
	(lambda (lst)
		(if (null? lst)
			lst
		    (append (cdr lst) (car lst))
		)
	)
)

(define invert
	(lambda (lst)
		(map invert-list lst)
	)
)


;; 2a)
;; To prove (-7 . (3 . (14 . ()))) is a list of numbers
;; Solution:
;;	step 1: (-7 . (3 . (14 . ()))) 
;;	step 2: (-7 . (3 . (14 . list-of-numbers))) 
;;	step 3: (-7 . (3 . (Int . list-of-numbers))) 
;;	step 4: (-7 . (3 . list-of-numbers))
;;	step 5: (-7 . (Int . list-of-numbers))
;;	step 6: (-7 . list-of-numbers)
;;	step 7: (Int . list-of-numbers)
;;	step 8: list-of-numbers

;; 2b)
;; Inductive specification of a list of numbers.
;; List-of-numbers := () | (Int . List-of-numbers) 

;; 2c)
;; Inductive specification of a list of characters.
;; List-of-characters := () | (Char . List-of-characters) 

;; 2d)
;; Inductive specification of a list of boolean.
;; List-of-boolean := () | (Bool . List-of-boolean) 

;; 2e)
;; Inductive specification of a list of strings.
;; List-of-strings := ({List-of-characters}*)

;; 2f)
;; Inductive specification of a list of symbols.
;; List-of-symbols := () | (Symbol . List-of-symbols) 

;; 2g)
;; 	b) inductive specification of nested list of numbers
;;	   nested-list-of-numbers := ({List-of-numbers}*)

;; 	c) inductive specification of nested list of characters
;;	   nested-list-of-characters = list-of-strings := ({List-of-characters}*)

;; 	d) inductive specification of nested list of boolean
;;	   nested-list-of-boolean := ({List-of-boolean}*)

;; 	e) inductive specification of nested list of strings
;;	   nested-list-of-strings := ({List-of-strings}*)

;; 	f) inductive specification of nested list of symbols
;;	   nested-list-of-symbols := ({List-of-symbols}*)


;; 2h)
;; Single inductive specification for the data type based specifications in problem 2g.
;; List-of-all-data-types := nested-list-of-numbers | 
;;			     nested-list-of-characters |
;;			     nested-list-of-boolean |
;;			     nested-list-of-strings |
;;			     nested-list-of-symbols


;; 2i)
;; Binary-Tree = () | (Int Binary-Tree Binary-Tree)


;; 3a)
;; succ a = a + 1
;; pred a = a - 1

;; add a b = if (b == 0)
;; 		a
;; 	   = add (succ a) (pred b)


;; 3b)
(define succ
	(lambda (a)
		(+ a 1)
	)
)

(define pred
	(lambda (a)
		(- a 1)
	)
)

(define add
	(lambda (a b)
		(if (zero? b)
			a
		    (add (succ a) (pred b))
		)
	)
)

;; 3c)
;; mult' accumulator a b = if (b == 0)
;;			      accumulator
;;			   mult' (add accumulator a) a (pred b)

;; mult a 0 = 0
;; mult 0 b = 0
;; mult a b =  mult' 0 a b

;; 3d)
(define mult
	(lambda (accumulator a b)
		(if (zero? b)
			accumulator
		    (mult (add accumulator a) a (pred b))
		)
	)
)
(define multiply
	(lambda (a b)
		(mult 0 a b)
	)
)

;; 3c) 
;; multiply a 0 = 0
;; multiply 0 b = 0
;; multiply a b = add a (multiply a (pred b))

;; 3d)
(define multiply-saurabh
	(lambda (a b)
		(if (zero? a)
			0
		    (if (zero? b)
		    	  0
			(add a (multiply-saurabh a (pred b)))
		    )
		)
	)
)

;; 3e)
;; expo x 0 = 1
;; expo x y = multiply-saurabh x (expo x (pred y))

;; 3f)
(define expo
	(lambda (x y)
		(if (zero? y)
			1
		    (multiply-saurabh x (expo x (pred y)))
		)
	)
)

;; 3g)
(define preorder
	(lambda (root)
		(if (null? root)
			root
			(cons (car root) (append (preorder (cadr root)) (preorder (caddr root))))
		)
	)
)
(define inorder
	(lambda (root)
		(if (null? root)
			root
			(append (inorder (cadr root)) (cons (car root) (inorder (caddr root))))
		)
	)
)
(define postorder
	(lambda (root)
		(if (null? root)
			root
			(append (append (postorder (cadr root)) (postorder (caddr root))) (list (car root)))
			;(append (postorder (cadr root)) (append (postorder (caddr root)) (list (car root))))
		)
	)
)

;; 3h)
(define list-of-numbers?
	(lambda (lst)
		(if (null? lst)
			#t
		    (if (number? (car lst))
		    	  (and #t (list-of-numbers? (cdr lst)))
			#f
		    )
		)
	)
)

;; 3i)
(define nth-element
	(lambda (n lst)
		(if (null? lst)
			(display "element not found..!!\n")
		    (if (zero? n)
		    	   (car lst)
			(nth-element (pred n) (cdr lst))
		    )
		)
	)
)

;; 3j)
(define remove-first
	(lambda (s los)
		(if (null? los)
			los
		    (if (eq? s (car los))
		    	    (cdr los)
			(cons (car los) (remove-first s (cdr los)))
		    )
		)
	)
)

;; 3k)
(define remove-all
	(lambda (s los)
		(if (null? los)
			los
		    (if (eq? s (car los))
		    	   (remove-all s (remove-first s los))
			(cons (car los) (remove-all s (cdr los)))
		    )
		)
	)
)


;; 3l)
(define substitute
	(lambda (old new s1)
		(if (null? s1)
			s1
		    (if (eq? old (car s1))
		    	     (cons new (substitute old new (cdr s1)))
			(cons (car s1) (substitute old new (cdr s1)))
		    )
		)
	)
)
