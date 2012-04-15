
; These 2 function evalations are equivalent

(eval '(+ 1 2 3 4))

(apply (car '(+ 1 2 3 4)) (cdr '(+ 1 2 3 4)))


