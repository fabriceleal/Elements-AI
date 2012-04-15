
; Aliases for the core functions that the book assumes, when necessary

;(defmacro add1 (n) (+ n 1))

; TODO: define tyi, tyo (read char, return ascii) (get ascii, print char),
; TODO: define locate (put cursor at position)


; Quick and dirty function for storing a property in an atom
(defun putprop (ato value prop) (setf (get ato prop) value))


