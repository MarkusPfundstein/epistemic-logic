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
    (find-if #'(lambda (n) (string= (string-upcase (agent-name n)) up-name))
	     (kripke-model-agents M))))

(defun find-relation-for-agent (M a)
  (cdr (find-if #'(lambda (x) (eq (car x) a)) (kripke-model-relations M))))

(defun find-relation-for-agent-and-world (M a w)
  (remove-if-not #'(lambda (v) (eq (relation-from v) w)) (find-relation-for-agent M a)))

(defun val (w p)
  (not (null (member2 p (world-propositions w)))))

(defun possible (M w a-name form)
  (format t "eval possible ~S ~S~%" (string a-name) form)
  (let ((a (find-agent-by-name M (string a-name))))
    (progn (format t "GOT AGENT: ~S ~S~%" a (find-relation-for-agent-and-world M a w))
	   (not (null (find-if #'(lambda (r) (models M (relation-to r) form))
			       (find-relation-for-agent-and-world M a w)))))))
	       
(defun models-list (M w form)
  (let ((op (car form))
	(rest-form (cdr form)))
	  (case op
	    (NOT
	     (progn (format t "match NOT ~S~%" rest-form)
		    (not (models M w rest-form))))
	    (AND 
	     (progn (format t "MATCH AND ~S ~S~%" (car rest-form) (cadr rest-form))
		    (and (models M w (car rest-form)) 
			 (models M w (cadr rest-form)))))
	    (OR
	     (progn (format t "MATCH OR ~S ~S~%" (car rest-form) (cadr rest-form))
		    (or (models M w (car rest-form))
			(models M w (cadr rest-form)))))
	    (IMPLIES
	     (progn (format t "MATCH IMPLIES ~S ~S~%" (car rest-form) (cadr rest-form))
		    (or (not (models M w (car rest-form)))
			(models M w (cadr rest-form)))))
	    (POSSIBLE
	     (progn (format t "MATCH POSSIBLE ~S ~S~%" (car rest-form) (cadr rest-form))
		    (possible M w (car rest-form) (cadr rest-form))))
	    (KNOWS
	     (progn (format t "MATCH KNOWS ~S ~S~%" (car rest-form) (cadr rest-form))
		    (not (possible M w (car rest-form) (list 'NOT (cadr rest-form))))))
	    (t 
	     (models M w op)))))

(defun models (M w form)
  (format t "eval ~s~%" form)
  (if (val w form) 
      (progn 
	(format t "return true~%") 
	t)
      (when (and (not (null form)) (listp form)) 
	(models-list M w form))))


(defparameter world-w (make-world :name "w" :propositions '(p q (implies p q))))
(defparameter world-v (make-world :name "v" :propositions '(q)))

(defparameter rel-a 
  (list 
   (make-relation :from world-w :to world-w)
   (make-relation :from world-v :to world-v)
   (make-relation :from world-w :to world-v)
   (make-relation :from world-v :to world-w)))

(defparameter rel-b
  (list 
   (make-relation :from world-w :to world-w)
   (make-relation :from world-v :to world-v)))

(defparameter alice (make-agent :name "alice"))
(defparameter bob (make-agent :name "bob"))


(defparameter world-w (make-world :name "w" :propositions '(:p :q (implies :p :q))))
(defparameter world-v (make-world :name "v" :propositions '(:q)))

(defparameter rel-a 
  (list 
   (make-relation :from world-w :to world-w)
   (make-relation :from world-v :to world-v)
   (make-relation :from world-w :to world-v)
   (make-relation :from world-v :to world-w)))

(defparameter rel-b
  (list 
   (make-relation :from world-w :to world-w)
   (make-relation :from world-v :to world-v)))

(defparameter alice (make-agent :name "alice"))
(defparameter bob (make-agent :name "bob"))

(defparameter M1 
  (make-kripke-model 
   :worlds (list world-w world-v)
   :agents (list alice bob)
   :relations (pairlis (list alice bob) (list rel-a rel-b))))
