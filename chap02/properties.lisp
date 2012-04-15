

(load "../appendix/FUNCTIONS.lisp")


(setf a 13)
(putprop 'a 13 :prop)
(get 'a :prop)
(remprop 'a :prop)
(get 'a :prop)

