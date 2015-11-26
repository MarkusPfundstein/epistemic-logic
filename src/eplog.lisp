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
  (let ((a (find-agent-by-name M (string a-name))))
    (not (null (find-if #'(lambda (r) (models M (relation-to r) form))
			(find-relation-for-agent-and-world M a w))))))
	       
(defun models-list (M w form)
  (let ((op (car form))
	(rest-form (cdr form)))
	  (case op
	    (NOT
	     (not (models M w rest-form)))
	    (AND 
	     (and (models M w (car rest-form)) 
		  (models M w (cadr rest-form))))
	    (OR
	     (or (models M w (car rest-form))
		 (models M w (cadr rest-form))))
	    (IMPLIES
	     (or (not (models M w (car rest-form)))
		 (models M w (cadr rest-form))))
	    (POSSIBLE
	     (possible M w (car rest-form) (cadr rest-form)))
	    (KNOWS
	     (not (possible M w (car rest-form) (list 'NOT (cadr rest-form)))))
	    (t 
	     (models M w op)))))

(defun models (M w form)
  (if (val w form) 
      t
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

(defun run-tests ()
  (assert (eq t (models M1 world-v '(IMPLIES (NOT :P) :Q))))
  (assert (eq t (models M1 world-v '(OR :p :q))))
  (assert (eq t (models M1 world-v '(AND (NOT :p) (AND :q (IMPLIES :p :q))))))
  (assert (null (models M1 world-v '(KNOWS alice :p))))
  (assert (eq t (models M1 world-v '(KNOWS bob (NOT :p)))))
  (assert (eq t (models M1 world-w '(POSSIBLE alice (KNOWS bob :p)))))
  (assert (null (models M1 world-v '(KNOWS alice (KNOWS bob :p)))))
  (assert (eq t (models M1 world-v '(KNOWS alice (OR (KNOWS bob :p) (KNOWS bob (NOT :p)))))))
  t)
