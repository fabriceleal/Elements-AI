
(defun treemax (tree)
	(cond ((atom tree) tree)
		(t (max (treemin (car tree)) (treemin (cadr tree)))) ; Binary trees ...
	) )

(defun treemin (tree)
	(cond  ((atom tree) tree)
		(t (min (treemax (car tree)) (treemax (cadr tree)))) ; Binary trees ...
	) )

(print "Should print 3")
(print (treemax '((3 (2 5))(7 (2 1)))))

(print "Should print ...")
(print (treemax '(((1 2) (3 4)) ((5 (6 7)) 8))))

(print "Should print ...")
(print (treemax '(1 (8 (2 (7 (3 (6 (4 5)))))))))
