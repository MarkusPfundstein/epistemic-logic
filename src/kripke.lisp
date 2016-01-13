(defpackage #:kripke
  (:export #:models 
	   #:make-world 
	   #:make-agent
	   #:make-relation 
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
  propositions)

(defstruct agent
  name)

(defstruct relation
  from
  to)

(defstruct kripke-model
  worlds 
  relations
  agents)

(defun find-world-by-name (M name)
  (find-if #'(lambda (n) (string= (world-name n) name))
	   (kripke-model-worlds M)))

(defun find-agent-by-name (M name)
  (let ((up-name (string-upcase name)))
    ;(format t "~S~%" up-name)
    (find-if #'(lambda (n) (string= (string-upcase (agent-name n)) up-name))
	     (kripke-model-agents M))))

(defun find-relation-for-agent (M a)
  (cdr (find-if #'(lambda (x) (eq (car x) a)) (kripke-model-relations M))))

(defun find-relation-for-agent-and-world (M a w)
  (remove-if-not #'(lambda (v) (eq (relation-from v) w)) (find-relation-for-agent M a)))

(defun val (w p)
  (not (null (member2 p (world-propositions w)))))

(defun possible (M w a-name form)
  (let ((a (find-agent-by-name M (string a-name))))
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
      (t 
       ;(format t "fall-trough t~%")
       (models M w op)))))

(defun models (M w form)
  ;(format t "MODELS: ~S~%" form)
  (if (val w form) 
      t
      (when (and (not (null form)) (listp form)) 
	(models-list M w form))))
