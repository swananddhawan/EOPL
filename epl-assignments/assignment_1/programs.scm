;; 1a)
(define (sum x y) (+ x y))

;; 1b)
(define (product x y) (* x y))

;; 1c) checking data type
(define (product_check_params x y) 
	(
		if (and (number? x) (number? y))
		(
			product x y
		)
		;; else
		(display "Invalid types..!!\n")
	)
)

;; 1c) checking data type
(define (sum_check_params x y) (
		if (and (number? x) (number? y)) (
			sum x y
		)
		;; else
		(display "Invalid types..!!\n")
	)
)

; 1e) sum of n numbers
(define sum_of_n_numbers
	(lambda (n)
		(if (= n 0)
			0
		(+ n (sum_of_n_numbers (- n 1))))
	)
)

; 1f) product of n numbers
(define product_of_n_numbers
	(lambda (n)
		(if (= n 1)
			1
		(* n (product_of_n_numbers (- n 1))))
	)
)

; 1g) accumulation of first N natural numbers with operator
(define return_accumulation
	(lambda (n operator)
		(if (= n 1)
			1
		    (operator n (return_accumulation (- n 1) operator))
		)
	)
)

;; 3b) is list of natural numbers

(define (is_natural_number n)
	(if (and (integer? n) (> n 0))
		#t
	#f)
)

(define (is_list_of_natural_numbers l_data)
	(if (null? l_data)
		#f
	)
	;; single element in list
	(if (null? (cdr l_data))
		(is_natural_number (car l_data))
	(and #t (is_list_of_natural_numbers (cdr l_data)))
	)
)

;; 3c
(define (return_sum_of_first_n_numbers_from_list n l_numbers)
        (if (or (= n 0) (< n 0) (null? l_numbers))
                0
        (+ (car l_numbers) (return_sum_of_first_n_numbers_from_list (- n 1) (cdr l_numbers)))
        )
)

;; 3d
(define (return_first_n_elements_list n l_numbers)
        (if (or (= n 0) (< n 0) (null? l_numbers)) (list)
        
         (cons (car l_numbers) (return_first_n_elements_list (- n 1) (cdr l_numbers)))
        )
)

;; 4a

(define gen
	(lambda (n)
		(if (= n 0) (list)
		    (cons n (gen (- n 1)))
		)
	)
)

(define my_sum
	(lambda (f n)
		(return_sum_of_first_n_numbers_from_list n (f n))))


;; 4b

(define my_map
	(lambda (f l)
		(if (null? l) (list)
		    (cons (f (car l)) (my_map f (cdr l)))
		)
	)
)

;; 4e

; 1g) accumulation of first N natural numbers with operator
(define return_accumulation
	(lambda (n operator)
		(if (= n 1)
			1
		    (operator n (return_accumulation (- n 1) operator))
		)
	)
)

(define return_procedure_for_accumulation
	(lambda (operator start end n)
		
	)
)
