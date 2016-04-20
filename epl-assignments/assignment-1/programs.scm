;; 1a)
(define (sum x y) (+ x y))

;; 1b)
(define (product x y) (* x y))

;; 1c) checking data type
(define (product-check-params x y) 
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
(define (sum-check-params x y) (
		if (and (number? x) (number? y)) (
			sum x y
		)
		;; else
		(display "Invalid types..!!\n")
	)
)

; 1e) sum of n numbers
(define sum-of-n-numbers
	(lambda (n)
		(if (= n 0)
			0
		(+ n (sum-of-n-numbers (- n 1))))
	)
)

; 1f) product of n numbers
(define product-of-n-numbers
	(lambda (n)
		(if (= n 1)
			1
		(* n (product-of-n-numbers (- n 1))))
	)
)

; 1g) accumulation of first N natural numbers with operator
(define return-accumulation
	(lambda (n operator)
		(if (= n 1)
			1
		    (operator n (return-accumulation (- n 1) operator))
		)
	)
)

;; 3b) is list of natural numbers

(define (is-natural-number n)
	(if (and (integer? n) (> n 0))
		#t
	#f)
)

(define (is-list-of-natural-numbers l-data)
	(if (null? l-data)
		#f
	)
	;; single element in list
	(if (null? (cdr l-data))
		(is-natural-number (car l-data))
	(and #t (is-list-of-natural-numbers (cdr l-data)))
	)
)

;; 3c
(define (return-sum-of-first-n-numbers-from-list n l-numbers)
        (if (or (= n 0) (< n 0) (null? l-numbers))
                0
        (+ (car l-numbers) (return-sum-of-first-n-numbers-from-list (- n 1) (cdr l-numbers)))
        )
)

;; 3d
(define (return-first-n-elements-list n l-numbers)
        (if (or (= n 0) (< n 0) (null? l-numbers)) (list)
        
         (cons (car l-numbers) (return-first-n-elements-list (- n 1) (cdr l-numbers)))
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

(define my-sum
	(lambda (f n)
		(return-sum-of-first-n-numbers-from-list n (f n))))


;; 4b

(define my-map
	(lambda (f l)
		(if (null? l) (list)
		    (cons (f (car l)) (my-map f (cdr l)))
		)
	)
)

;; 4e

; 1g) accumulation of first N natural numbers with operator
(define return-accumulation
	(lambda (n operator)
		(if (= n 1)
			1
		    (operator n (return-accumulation (- n 1) operator))
		)
	)
)
