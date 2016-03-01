;; lambda expression in BNF form
;; LambdaExpression := identifier
;;		    := (lambda (identier) LambdaExpression)
;;		    := (LambdaExpression LambdaExpression)

;; (let ((a 1)
;;	 (b 2)
;;	)
;;	(+ a b)
;; )

;; (((lambda (a b)
;; 	(+ a b)
;;   )
;;  2)
;; 1)
