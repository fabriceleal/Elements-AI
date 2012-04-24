
(load "../appendix/FUNCTIONS.lisp")
(load "../chap03/match.lisp")


(defun addtoset (elem lst)
	"Cons'es wihout repeating elements"
	(cond ((member elem lst) lst) (t (cons elem lst))) )

(defun addsuperset (aname x)
	(putprop aname (addtoset x (get aname 'isa)) 'isa) )

(defun addsubset (aname x)
	(putprop aname (addtoset x (get aname 'includes)) 'includes) )


(defun isatest (x y n)
	"Is-a test"
	(cond ((eq x y) t) ; Something isa Something
		((zerop n) nil) ; Test depth
		((member y (get x 'isa)) t) 
		(t (any (mapcar
				(function (lambda (xx) (isatest xx y (- n 1))))
				(get x 'isa) ))) ) )

(defun any (lst)
	"Short-circuit OR, with a list as arguments."
	(cond  ((null lst) nil)
		((car lst) t)
		(t (any (cdr lst))) ) )

(defun linneus()
	(prog ()
		(print '(I am linneus))
		(print '(Please give me information or ask questions))
	my_loop (setq textin (read))
		(interpret textin)
		(go my_loop) ) )


(defun interpret (text)
	(cond 

	; Rule for matching (a something us a anotherthing)
	((match '((matcharticle article1)(? x) is (matcharticle article2)(? y)) text) (addsuperset x y) (addsubset y x) (putprop x article1 'article) (putprop y article2 'article) (print '(I understand)) )

	; Rule for matching (what is a something)
	((match '(What is (matcharticle article1) (? x)) text) 
		(setq isaflag nil)
		(setq includeflag t)
		(cond 
			((setq y (get x 'isa))
			 (setq isaflag t) )
			((setq y (get x 'includes))
			 (setq includeflag t) ) ) 
		(print (append (list (get x 'article)) (list x) 
			(cond (isaflag '(is)) (includeflag '(is something more general than)))
			(makeconj y) )) )
	; Rule for matching (is a something a otherthing)
	((match '(is (matcharticle article1) (? x) (matcharticle article2) (? y)) text) 
		(cond 
			((isatest x y 10) (print (append '(yes indeed) (list (get x 'article)) (list x) '(is) (list (get y 'article)) (list y) )) )
			(t (print '(sorry noy that i know off))) ) )


	; Rule for questions such as (why is a bear an animal)
	((match '(why is (matcharticle article1) (? x) (matcharticle article2) (? y)) text)
		(cond 
			((isatest x y 10) (print (cons 'because (explain_links x y)))) 
			(t (print '(but it isn't!))) ) )

	; Rule that handles all other inputs
	(t (print '(I do not understand))) ) )


(defun matcharticle (x)
	(member x '(a an the that this those these)) )

(defun makeconj (lst)
	(cond ((null lst) nil)
		((null (cdr lst)) (cons (get (car lst) 'article) lst))
		(t (cons (get (car lst) 'article)
			(cons (car lst)
				(cons 'and (makeconj (cdr lst)))
				)
			)) ) )

(defun explain_links (x y)
	(cond ((eq x y) '(they are identical))
		((member y (get x 'isa)) '(you told me so) ) 
		(t (explain_chain x (get x 'isa) y)) ) )

(defun explain_chain (x l y)
	(cond ((null l) nil)
		((member y l) 
		 (cons 'and (tell x y)) )
		((isatest (car l) y 10) 
			(append (tell x (car l)) 
				(explain_chain (car l) (get (car l) 'isa) y) ) )
		(t (explain_chain x (cdr l) y)) ) )


(defun tell (x y)
	(list (get x 'article) x 'is (get y 'article) y) )

