(defpackage #:kripke
  (:export #:models 
	   #:make-world 
	   #:world-name
	   #:find-world-by-name
	   #:find-real-world-by-name
	   #:find-relation-for-agent
	   #:find-previous-world
	   #:kripke-model-worlds
	   #:kripke-model-agents
	   #:kripke-model-real-worlds
	   #:kripke-model-relations
	   #:kripke-model-comgraph
	   #:kripke-model-time-relations
	   #:kripke-model-previous-model
	   #:world-propositions
	   #:world-additions
	   #:world-substitutions
	   #:world-preconditions
	   #:make-agent
	   #:agent-name
	   #:agent-p
	   #:make-relation 
	   #:relation-from
	   #:relation-to
	   #:make-kripke-model)
  (:use #:cl #:func))

(in-package #:kripke)

(defun member2 (el lst)
   (cond
     ((null lst) ()) 
     ((equal (car lst) el) lst) 
     (t (member2 el (cdr lst)))))

(defstruct world
  name
  propositions
  additions
  substitutions)

(defmacro world-preconditions (w)
  `(world-propositions ,w))

(defstruct agent
  name)

(defstruct relation
  from
  to)

(defstruct kripke-model
  worlds 
  vocabulary
  relations
  time-relations
  previous-model
  comgraph
  agents
  real-worlds)

(defun find-world-by-name (M name)
  (find-if #'(lambda (n) (string= (world-name n) name))
	   (kripke-model-worlds M)))

(defun find-real-world-by-name (M name)
  (find-if #'(lambda (n) (string= (world-name n) name))
	   (kripke-model-real-worlds M)))

(defun find-agent-by-name (M name)
  (let ((up-name (string-upcase name)))
    (find-if #'(lambda (n) (string= 
			    (string-upcase (agent-name n)) up-name))
	     (kripke-model-agents M))))

(defun find-previous-worlds (M w)
  (mapcar #'cdr (find-all-if #'(lambda (w-rel) (equal w (car w-rel)))
			     (kripke-model-time-relations M))))

(defun find-relation-for-agent (M a)
  (cdr (find-if #'(lambda (x) 
		    (eq (agent-name (car x)) (agent-name a))) 
		(kripke-model-relations M))))

(defun find-relation-for-agent-and-world (M a w)
  (remove-if-not #'(lambda (v) (eq (relation-from v) w)) (find-relation-for-agent M a)))

(defun val (w p)
  (not (null (member2 p (world-propositions w)))))

(defun possible (M w a-name form)
  (let ((a (find-agent-by-name M (string a-name))))
    (when (not a)
      (error "error finding agent in function possible"))
    (not (null (find-if #'(lambda (r) (models M (relation-to r) form))
			(find-relation-for-agent-and-world M a w))))))

(defun yesterday-models (M w form)
  ;(format t "~S~%" form)
  ;(format t "~S~%" (find-previous-worlds M w))
  (not (null (find-if #'(lambda (pw)
			  (models (kripke-model-previous-model M) pw form))
		      (find-previous-worlds M w)))))

;(defun observe-func (M w 

(defun models-list (M w form)
  (let ((op (car form))
	(rest-form (cdr form)))
    (case op
      (:NOT
       (not (models M w rest-form)))
      (:AND 
       (and (models M w (car rest-form)) 
	    (models M w (cadr rest-form))))
      (:OR
       (or (models M w (car rest-form))
	   (models M w (cadr rest-form))))
      (:IMPLIES
       (or (not (models M w (car rest-form)))
	   (models M w (cadr rest-form))))
      (:POSSIBLE
       (possible M w (car rest-form) (cadr rest-form)))
      (:KNOWS
       (not (possible M w (car rest-form) (list :NOT (cadr rest-form)))))
      (:YESTERDAY
       (yesterday-models M w (car rest-form)))
;      (:OBSERVE
;       (observe-func M w (car rest-form)))
      (:TRUE
       t)
      (t
       (models M w op)))))

(defun models (M w form)
  (if (stringp w)
      (models M (find-world-by-name M w) form)
      (if (val w form) 
	  t
	  (when (and (not (null form)) (listp form)) 
	    (models-list M w form)))))
