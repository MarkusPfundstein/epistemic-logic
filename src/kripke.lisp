(defpackage #:kripke
  (:export #:models
	   #:models-some
	   #:models-all
	   #:unify-formula
	   #:update-val
	   #:make-world 
	   #:connect-worlds
	   #:connect-worlds!
	   #:make-empty-relations
	   #:world-name
	   #:find-world-by-name
	   #:find-real-world-by-name
	   #:find-relation-for-agent
	   #:find-previous-world
	   #:make-protocol
	   #:protocol-precondition-fn
	   #:protocol-action-fn
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
	   #:agent-symbol
	   #:agent-p
	   #:relation-p
	   #:kripke-model-p
	   #:protocol-p
	   #:agent-protocols
	   #:make-relation 
	   #:relation-from
	   #:relation-to
	   #:make-kripke-model)
  (:use #:cl #:alexandria #:iterate #:func))

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

(defstruct protocol
  precondition-fn
  action-fn)

(defstruct agent
  name
  states
  protocols
  symbol)

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

(defun connect-worlds (rels w1 w2 agents &key (symmetric t))
  (let ((lst (if (listp agents) 
		 agents
		 (list agents))))
    (iter (for p in rels)
	  (if (member (car p) lst)
	      (collect (append p 
			       (if (or (not symmetric) (eq w1 w2))
				   (list (make-relation :from w1 :to w2))
				   (list (make-relation :from w1 :to w2)
					 (make-relation :from w2 :to w1)))))
	      (collect p)))))

(defmacro connect-worlds! (rels w1 w2 agents &key (symmetric t))
  `(setf ,rels (connect-worlds ,rels ,w1 ,w2 ,agents :symmetric ,symmetric))) 

(defun make-empty-relations (agents &optional reflexive-worlds)
  (let ((rel (mapcar #'(lambda (ag) (list ag)) agents)))
    (iter (for rw in reflexive-worlds)
	  (iter (for ag in agents)
		(connect-worlds! rel rw rw ag)))
    rel))

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

(defun unify-formula (formula map)
  (if (listp formula)
      (iter (for sf in formula)
	    (collect (unify-formula sf map)))
      (if (string= "?" formula :start2 0 :end2 1)
	  (cdr (find-if #'(lambda (s) (equal (car s) formula)) map))
	  formula)))

(defun val (w p)
  (not (null (member2 p (world-propositions w)))))

(defun possible (M w a-name form &key (A-m nil))
  (let ((a (find-agent-by-name M (string a-name))))
    (when (not a)
      (error "error finding agent in function possible"))
    (not (null (find-if #'(lambda (r)
			    (models M (relation-to r) form :A A-m))
			(find-relation-for-agent-and-world M a w))))))



; taken from del package. must move into Kripke and be removed there
(defun update-val (props add-props sub-props M A w e agents)
  (union
   (union (set-difference props (mapcar #'(lambda (e) (car e)) sub-props))
	  add-props)
   (mapcar #'(lambda (e) (cdr e)) sub-props)))


(defun post (prop world event)
  (not (null (find prop
		   (update-val (world-propositions world)
			       (world-additions event)
			       (world-substitutions event)
			       nil
			       nil
			       world
			       event
			       nil)
		   :test #'equal))))

(defun after-event-prop (M A w event-name prop)
  (format t "after-event-prop: ~S - ~S~%" event-name prop)
  (when-let ((event (find-world-by-name A event-name)))
    (let* ((precond (world-preconditions event))
	   (consequent (if (post prop w event) '(:TRUE) '(:FALSE)))
	   (reduction-form (unify-formula
			   '(:IMPLIES ?PRE-EVENT ?POST)
			   (pairlis '(?PRE-EVENT ?POST)
				    (list precond
					  consequent)))))
      (format t "    reduction-form: ~S~%" reduction-form)
      (models M w reduction-form :A A))))

(defun after-event-knows-form (M A w event-name agent-name form)
  (format t "after-event-knows-form: ~S - ~S~%" agent-name form)
  (when-let ((agent (find-agent-by-name M agent-name))
	     (event (find-world-by-name A event-name))
	     (prop form))
    (format t "prop ~S~%" prop)
    (let* ((precond (world-preconditions event))
	   (conjunction-parts
	    (mapcar #'(lambda (e2-rel)
			(let* ((e2 (relation-to e2-rel)))
			  (unify-formula '(:KNOWS
					   ?AGENT (:AFTER-EVENT
						   ?EVENT
						   ?PROP))
					 (pairlis '(?AGENT ?EVENT ?PROP)
						  (list agent-name
							(world-name e2)
							prop)))))
						   
			;(let* ((e2 (relation-to e2-rel))
			;       (precond-e2 (world-preconditions e2))
			;       (consequent (if (post prop w e2)
		;			       '(:TRUE)
;					       '(:FALSE))))
		;	  (unify-formula '(:IMPLIES ?PRE-EVENT ?POST)
		;			 (pairlis '(?PRE-EVENT ?POST)
		;				  (list precond-e2
		;					consequent)))))
		    (find-relation-for-agent-and-world A agent event)))
	   ;(x (format t "conj parts ~S~%" (length conjunction-parts)))
	   (conjunction (if (> (length conjunction-parts) 1)
			    (append '(:AND) conjunction-parts)
			    (car conjunction-parts)))
	    
	   (reduction-form (unify-formula
			    '(:IMPLIES ?PRE-EVENT ?CONJUNCTION)
			    (pairlis '(?PRE-EVENT ?CONJUNCTION)
				     (list precond conjunction)))))
      ;(format t "parts: ~S~%" conjunction-parts)
      ;(format t "reduction-form: ~S~%" reduction-form)
      (models M w reduction-form :A A))))

(defun form-begins-with-op (form)
  (when (listp form)
    (let ((op (car form)))
      (or (equal ':AND op)
	  (equal ':OR  op)
	  (equal ':IMPLIES op)
	  (equal ':KNOWS op)
	  (equal ':POSSIBLE op)
	  (equal ':NOT op)
	  (equal ':OBSERVE op)
	  (equal ':YESTERDAY op)
	  (equal ':AFTER-EVENT op)))))

(defun after-event (M A w event-name form)
  (unless A (error ":AFTER-EVENT can only be used with :A key in models~%"))
  (format t "after-event: ~S - ~S~%" event-name form)
  (if (form-begins-with-op form)
      (let ((op (car form)))
	(case op
	  (:KNOWS
	   (format t "after-event-knows-axiom~%")
	   (after-event-knows-form M A w event-name (cadr form) (caddr form)))
	  (t
	   (error "sorry! only supported operator after :AFTER-EVENT is :KNOWS"))))
      (after-event-prop M A w event-name form)))
      

(defun yesterday-models (M w form)
  (not (null (find-if #'(lambda (pw)
			  (models (kripke-model-previous-model M) pw form))
		      (find-previous-worlds M w)))))

(defun observe-func (M w a-name form)
  (let ((formula (unify-formula
		  '(:AND (:KNOWS ?ag ?phi) (:YESTERDAY (:NOT (:KNOWS ?ag ?phi))))
		  (pairlis '(?ag ?phi) (list a-name form)))))
    (models M w formula)))

(defun models-list (M w form &key (A nil))
  (let ((op (car form))
	(rest-form (cdr form)))
    (case op
      (:NOT
       (not (models M w rest-form :A A)))
      (:AND 
       (and (models M w (car rest-form) :A A) 
	    (models M w (cadr rest-form) :A A)))
      (:OR
       (or (models M w (car rest-form) :A A)
	   (models M w (cadr rest-form) :A A)))
      (:IMPLIES
       (or (not (models M w (car rest-form) :A A))
	   (models M w (cadr rest-form) :A A)))
      (:POSSIBLE
       (possible M w (car rest-form) (cadr rest-form) :A-m A))
      (:KNOWS
       (not (possible M
		      w
		      (car rest-form)
		      (list :NOT (cadr rest-form))
		      :A-m A)))
      (:YESTERDAY
       (yesterday-models M w (car rest-form)))
      (:OBSERVE
       (observe-func M w (car rest-form) (cadr rest-form)))
      (:AFTER-EVENT
       (after-event M A w (car rest-form) (cadr rest-form)))
      (:TRUE
       t)
      (:FALSE
       nil)
      (t
       (models M w op :A A)))))

; returns true if form is true in at least one world in M
(defun models-some (M form)
  (find-true (mapcar #'(lambda (w) (models M (world-name w) form))
		     (kripke-model-worlds M))))

; returns true if form is true in all worlds
(defun models-all (M form)
  (not (find-nil (mapcar #'(lambda (w) (models M (world-name w) form))
			 (kripke-model-worlds M)))))

(defun models (M w form &key (A nil))
  (if (stringp w)
      (models M (find-world-by-name M w) form :A A)
      (if (val w form) 
	  t
	  (when (and (not (null form)) (listp form)) 
	    (models-list M w form :A A)))))
