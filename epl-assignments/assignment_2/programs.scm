;; 1a)

;; my_length list = 0  ..... if null list
;; my_length x:list = 1 + my_length list

;; 1b)
(define my_length
        (lambda (l)
                (if (null? l)
                        0
                    (+ 1 (my_length (cdr l))))))

;; 1c)
(define my_append
        (lambda (l1 l2)
                (if (null? l1)
                        l2
                    (cons (car l1) (my_append (cdr l1) l2)))))


;; 1d)
(define my_flatten
	(lambda (l)
		(if (null? l)
			l
		    (if (list? (car l))
		    	     (append (my_flatten (car l)) (my_flatten (cdr l)))
			(cons (car l) (my_flatten (cdr l)))
		    )
		)
	)
)

;; 1e)
(define reverse_list
	(lambda (l)
		(if (null? l)
			l
		    (my_append (reverse_list (cdr l)) (list (car l)))
		)
	)
)

;; 1f
(define get_first_occurence_index
	(lambda (symbol list_of_symbols index)
		(if (null? list_of_symbols)
			-1
		    (if (eq? symbol (car list_of_symbols))
			     index
			(get_first_occurence_index symbol (cdr list_of_symbols) (+ index 1))
		    )
		)
	)
)

(define list_index
	(lambda (symbol list_of_symbols)
		(get_first_occurence_index symbol list_of_symbols 0)
	)	
)


;; 1g)
(define list_all_indices
	(lambda (symbol list_of_symbols index)
		(if (null? list_of_symbols)
			list_of_symbols
		    (if (eq? symbol (car list_of_symbols))
		    	    (cons index (list_all_indices symbol (cdr list_of_symbols) (+ 1 index)))
			(list_all_indices symbol (cdr list_of_symbols) (+ 1 index))    
		    )
		)
	)
)
(define list_indices
	(lambda (symbol list_of_symbols)
		(list_all_indices symbol list_of_symbols 0)
	)
)

;;1h)
(define invert_list
	(lambda (lst)
		(if (null? lst)
			lst
		    (append (cdr lst) (car lst))
		)
	)
)

(define invert
	(lambda (lst)
		(map invert_list lst)
	)
)


;; 2a)
;; To prove (-7 . (3 . (14 . ()))) is a list of numbers
;; Solution:
;;	step 1: (-7 . (3 . (14 . ()))) 
;;	step 2: (-7 . (3 . (14 . list_of_numbers))) 
;;	step 3: (-7 . (3 . (Int . list_of_numbers))) 
;;	step 4: (-7 . (3 . list_of_numbers))
;;	step 5: (-7 . (Int . list_of_numbers))
;;	step 6: (-7 . list_of_numbers)
;;	step 7: (Int . list_of_numbers)
;;	step 8: list_of_numbers

;; 2b)
;; Inductive specification of a list of numbers.
;; List_of_numbers := () | (Int . List_of_numbers) 

;; 2c)
;; Inductive specification of a list of characters.
;; List_of_characters := () | (Char . List_of_characters) 

;; 2d)
;; Inductive specification of a list of boolean.
;; List_of_boolean := () | (Bool . List_of_boolean) 

;; 2e)
;; Inductive specification of a list of strings.
;; List_of_strings := ({List_of_characters}*)

;; 2f)
;; Inductive specification of a list of symbols.
;; List_of_symbols := () | (Symbol . List_of_symbols) 

;; 2g)
;; 	b) inductive specification of nested list of numbers
;;	   nested_list_of_numbers := ({List_of_numbers}*)

;; 	c) inductive specification of nested list of characters
;;	   nested_list_of_characters = list_of_strings := ({List_of_characters}*)

;; 	d) inductive specification of nested list of boolean
;;	   nested_list_of_boolean := ({List_of_boolean}*)

;; 	e) inductive specification of nested list of strings
;;	   nested_list_of_strings := ({List_of_strings}*)

;; 	f) inductive specification of nested list of symbols
;;	   nested_list_of_symbols := ({List_of_symbols}*)


;; 2h)
;; Single inductive specification for the data type based specifications in problem 2g.
;; List_of_all_data_types := nested_list_of_numbers | 
;;			     nested_list_of_characters |
;;			     nested_list_of_boolean |
;;			     nested_list_of_strings |
;;			     nested_list_of_symbols


;; 2i)
;; Binary_Tree = () | (Int Binary_Tree Binary_Tree)


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
(define multiply_saurabh
	(lambda (a b)
		(if (zero? a)
			0
		    (if (zero? b)
		    	  0
			(add a (multiply_saurabh a (pred b)))
		    )
		)
	)
)

;; 3e)
;; expo x 0 = 1
;; expo x y = multiply_saurabh x (expo x (pred y))

;; 3f)
(define expo
	(lambda (x y)
		(if (zero? y)
			1
		    (multiply_saurabh x (expo x (pred y)))
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
(define list_of_numbers?
	(lambda (lst)
		(if (null? lst)
			#t
		    (if (number? (car lst))
		    	  (and #t (list_of_numbers? (cdr lst)))
			#f
		    )
		)
	)
)

;; 3i)
(define nth_element
	(lambda (n lst)
		(if (null? lst)
			(display "element not found..!!\n")
		    (if (zero? n)
		    	   (car lst)
			(nth_element (pred n) (cdr lst))
		    )
		)
	)
)

;; 3j)
(define remove_first
	(lambda (s los)
		(if (null? los)
			los
		    (if (eq? s (car los))
		    	    (cdr los)
			(cons (car los) (remove_first s (cdr los)))
		    )
		)
	)
)

;; 3k)
(define remove_all
	(lambda (s los)
		(if (null? los)
			los
		    (if (eq? s (car los))
		    	   (remove_all s (remove_first s los))
			(cons (car los) (remove_all s (cdr los)))
		    )
		)
	)
)


;; 3l)
(define substitue
	(lambda (old new s1)
		(if (null? s1)
			s1
		    (if (eq? old (car s1))
		    	     (cons new (substitue old new (cdr s1)))
			(cons (car s1) (substitue old new (cdr s1)))
		    )
		)
	)
)
