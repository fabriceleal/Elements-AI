;(untrace next)

(load "exercises/15_equalelts.lisp")

(defun differenciate (seq arith)
	(cond ((atom seq) '())
		((null (cdr seq)) '())
		(t (cons (apply arith (list (cadr seq) (car seq))) (differenciate (cdr seq) arith)))
		 ) )

(defun next (seq) 
	(cond ((or (atom seq) (null (cdr seq))) 'incomplete)
		((equalelts (differenciate seq '-)) (+ (car (last seq)) (- (cadr seq) (car seq))))
		((equalelts (differenciate seq '/)) (* (car (last seq)) (/ (cadr seq) (car seq))))
		(t 'unknown)
		) )

;(trace differenciate)
;(trace next)

; Prints 32
(next '(2 4 8 16))

; Prints 5
(next '(1 2 3 4))

; Prints 324
(next '(4 -12 36 -108))

; Prints 1/27
(next '(3 1 1/3 1/9))
