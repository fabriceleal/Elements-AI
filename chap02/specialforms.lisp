; The only way of defining special forms that I know of is using macros. The book  talks about a 'defexpr' ...


(defmacro mysetq (l) (set (car l) (eval (cadr l))))

; Evaluation of args is made *inside* of the function, not on call ...
(mysetq (x 5))
(mysetq (x (* 5 5)))
(eval '(mysetq (x (apply (lambda()(print "hello!")(* 5 5)) '()))))


; A funky setq alternative that allows some crazy interactive sessions!
(defmacro setq2(&rest l)
	(cond ((not (= (length l) 2)) (print '(Error in setq2)))
		(t (set (eval (car l)) (cadr l)))
	))

(setq X 'Y)
(setq2 X (car Y)) 
Y
(eval Y)



(defmacro mean(&rest l)
	(/	(apply '+ (mapcar 'eval l))
		(length l)))


(mean 1 2 3)
