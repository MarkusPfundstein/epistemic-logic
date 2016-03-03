(defpackage #:kripke
  (:export #:models 
	   #:make-world 
	   #:world-name
	   #:find-world-by-name
	   #:find-real-world-by-name
	   #:find-relation-for-agent
	   #:kripke-model-worlds
	   #:kripke-model-agents
	   #:kripke-model-real-worlds
	   #:kripke-model-relations
	   #:kripke-model-comgraph
	   #:world-propositions
	   #:world-additions
	   #:world-substitutions
	   #:make-agent
	   #:agent-name
	   #:agent-p
	   #:make-relation 
	   #:relation-from
	   #:relation-to
	   #:make-kripke-model)
  (:use #:cl))

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

(defstruct agent
  name)

(defstruct relation
  from
  to)

(defstruct kripke-model
  worlds 
  vocabulary
  relations
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
    ;(format t "~S~%" up-name)
    (find-if #'(lambda (n) (string= 
			    (string-upcase (agent-name n)) up-name))
	     (kripke-model-agents M))))

(defun find-relation-for-agent (M a)
  (cdr (find-if #'(lambda (x) 
		    (eq (agent-name (car x)) (agent-name a))) 
		(kripke-model-relations M))))

(defun find-relation-for-agent-and-world (M a w)
  (remove-if-not #'(lambda (v) (eq (relation-from v) w)) (find-relation-for-agent M a)))

(defun val (w p)
  (not (null (member2 p (world-propositions w)))))

(defun possible (M w a-name form)
  ;(format t "a-name: ~S~%" a-name)
  (let ((a (find-agent-by-name M (string a-name))))
    (when (not a)
      (error "error finding agent in function possible"))
    ;(format t "agent: ~S~%" a)
    (not (null (find-if #'(lambda (r) (models M (relation-to r) form))
			(find-relation-for-agent-and-world M a w))))))
	       
(defun models-list (M w form)
  ;(format t "models-list: ~S~%" form)
  (let ((op (car form))
	(rest-form (cdr form)))
    ;(format t "op: ~S~%" op)
    ;(format t "rest-form: ~S~%" rest-form)
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
      (:TRUE
       t)
      (t 
       ;(format t "fall-trough t~%")
       (models M w op)))))

(defun models (M w form)
  ;(format t "MODELS: ~S~%" form)
  (if (stringp w)
      (models M (find-world-by-name M w) form)
      (if (val w form) 
	  t
	  (when (and (not (null form)) (listp form)) 
	    (models-list M w form)))))
