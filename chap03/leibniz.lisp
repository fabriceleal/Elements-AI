
; Leibniz
; Shows how a production system should be setup so that each time a rule fires, 
; a little progress is made towards the solution of a relatively complicated
; problem.

; TODO: Exercise to me: make this work with an arbitrary number of parameters

; production rules: (current_goal pattern transformation rule_name)


(load "match.lisp")

; Returns T if an expression matches a plus form
(defun plusform (f)
	(and (not (atom f))
		(match '(+ (? e1) (? e2)) f) ) )

(setq diff_plus_rule '(
	differentiate
	(d (plusform f1) (? v1))
	(list '+ (list 'd e1 v1) (list 'd e2 v1))
	diff_plus_rule
	) )

(setq diff_x_rule '(
	differentiate
	(d ((lambda(v)(setq e1 v)) e1) ((lambda(v)(eq v e1)) e2))
	1
	diff_x_rule
	) )

(defun no_v1 (f v1)
	(cond ((null f) t)
		((atom f) (not (eq f v1)))
		((no_v1 (car f) v1) (no_v1 (cdr f) v1))
		(t nil) ) )

(setq diff_const_rule '(
	differentiate
	(d ((lambda(f)(setq e1 f)) f)
		((lambda(v1)(no_v1 e1 v1)) v1) )
	0
	diff_const_rule
	) )

(setq diff_product_rule '(
	differentiate
	(d 
		((lambda (f)
			(and (not (atom f))
				(match '(* (? e1) (? e2)) f)) ) e3)
		(? v1) )
	(list '+ 
		(list '* e2 (list 'd e1 v1)) 
		(list '* e1 (list 'd e2 v1)) )
	diff_product_rule
	) )


(setq diff_power_rule '(
	differentiate
	(d
		((lambda (f) 
			(and (not (atom f))
				(match '(expt (? e1) (numberp e2)) f)) ) e3)
		(? v1) )
	(list '* e2 
		(list '* (list 'expt e1 (list '- e2 1))
		(list 'd e1 v1) ) )
	diff_power_rule
	) )


(setq sub1_rule '(
	simplify
	(- (numberp e1) 1)
	(- e1 1)
	sub1_rule
	) )

(setq exp0_rule '(
	simplify
	(expt (? e1) 0)
	1
	exp0_rule
	) )

(setq exp1_rule '(
	simplify
	(expt (? e1) 1)
	e1
	exp1_fule
	) )

(setq times1_rule '(
	simplify
	(* (? e1) 1)
	e1
	times1_rule
	) )

; Variation of the previous one, with the operand order switched
(setq one_times_rule '(
	simplify
	(* 1 (? e1))
	e1
	one_times_rule
	) )

(setq plus0_rule '(
	simplify
	(+ (? e1) 0)
	e1
	plus0_rule
	) )

; Variation of the previous one, with the operand order switched
(setq zero_plus_rule '(
	simplify
	(+ 0 (? e1))
	e1
	zero_plus_rule
	) )

(setq times0_rule '(
	simplify
	(* (? e1) 0)
	0
	times0_rule
	) )

; Variation of the previous one, with the operand order switched
(setq zero_times_rule '(
	simplify
	(* 0 (? e1))
	0
	zero_times_rule
	) )

(setq constant_addition_rule '(
	simplify
	(+ (numberp e1) (numberp e2))
	(+ e1 e2)
	constant_addition_rule
	) )

(setq constant_multiplication_rule '(
	simplify
	(* (numberp e1) (numberp e2))
	(* e1 e2)
	constant_multiplication_rule
	) )

(setq goal_change_rule '(
	differentiate
	((* f))
	(prog() (setq current_goal 'simplify) (return f))
	goal_change_rule
	) )

(setq rules (list diff_plus_rule diff_x_rule diff_const_rule
	diff_product_rule diff_power_rule goal_change_rule
	sub1_rule exp0_rule exp1_rule
	times1_rule one_times_rule
	times0_rule zero_times_rule
	plus0_rule zero_plus_rule
	constant_addition_rule constant_multiplication_rule
	) )

(defun fire (rule)
	; Fires a rule
        (prog ()
                (princ (caddr (cdr rule))) ; Logs
                (princ " ") 
                (princ 'fires)
                (return (caddr rule)) ; Returns action
                ) )

(defun try_rule_on_list (expression_list pattern rule)
	; Tries to apply the rule to each element on expression_list.
	; Returns NIL if the rule cannot be applied in any of the expressions
	; or their subexpressions. Otherwise returns the list with one 
	; replacement: the first expression in which the rule can be applied
	; is replaced by the result of applying the rule in it
        (cond 
		((null expression_list) nil)
                ((setq temp (try_rule1 (car expression_list) pattern rule))
                 (cons temp (cdr expression_list)) )
                ((setq temp (try_rule_on_list (cdr expression_list) pattern rule))
                 (cons (car expression_list) temp) )
                (t nil) ) )

(defun try_rule1 (expression pattern rule)
	; Does the real work of searching down through the expression
	; to see if the rule can be applied anywhere in it
        (cond
                ((atom expression) nil)
                ((match pattern expression)
                 (fire rule) )
                (t (try_rule_on_list expression pattern rule)) ) )

(defun try_rule (rule expression)
	; Applies one rule to an expression or one of its subexpressions;
	; if the rule is successful, returns the transformed formula; it 
	; returns NIL otherwise
        (prog (rule_goal pattern action)
                (setq rule_goal (car rule))
                (setq pattern (cadr rule))
                (setq action (caddr rule))
                (cond ((not (eq current_goal rule_goal)) (return nil)))
                (return (try_rule1 expression pattern rule)) ) )    


(defun try_rules (rules_left)
	; Tries each rule on the list given until one succeds or the 
	; end of the list is reached or the current formula is no 
	; long a list. If a rule fires, returns the current formula;
	; otherwise, it returns NIL
	(cond 
		((null rules_left) nil)
		((atom current_formula) nil)
		((setq temp
			(try_rule (car rules_left) current_formula) )
			(setq current_formula temp) )
		(t (try_rules (cdr rules_left)))
		) )

(defun control()
	(prog ()
	my_loop (cond ((not (try_rules rules))
			(return current_formula) ))
		(go my_loop)) )

(setq current_goal 'differentiate)
(setq fo '(d (+ (expt x 2) (* 2 x)) x))
(setq current_formula fo)

;(trace control)
;(trace try_rules)
;(trace try_rule)
;(trace try_rule1)
;(trace try_rule_on_list)
;(trace fire)
