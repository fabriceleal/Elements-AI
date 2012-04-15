

; Replacing pointers in memory

(setf x '(1 2 3))
(nconc x '(4 5 6))
; And now x is (1 2 3 4 5 6)

(setf x '(1 2 3))
(rplaca x x)
; And now x is completely useless, the car of x points to itself. Weird ...

(setf x '(1 2 3))
(rplacd x x)
; And now x is an infinite list of 1's, the cdr of x points to the car of x
