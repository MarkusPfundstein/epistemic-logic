(defpackage #:del
  (:export #:product-update)
  (:use #:cl #:kripke))

(in-package #:del)

(defun make-new-world-name (w e)
  (concatenate 
   'string 
   "(" w ", " e ")"))

(defun find-world (name worlds)
  (find-if #'(lambda (n) (string= (world-name n) name)) worlds))

(defun make-new-worlds (M A)
  (let ((new-worlds '()))
    (dolist (w (kripke-model-worlds M))
      (dolist (e (kripke-model-worlds A))
	(when (models M w (world-propositions e))
	  ;(format t "add world: ~S~%" w)
	  (push (make-world 
		 :name (make-new-world-name (world-name w) (world-name e))
		 :propositions (world-propositions w))
		new-worlds))))
    (reverse new-worlds)))

(defun make-new-relation (rel-m rel-a new-worlds)
  (let* ((new-from-name (make-new-world-name (world-name (relation-from rel-m))
					     (world-name (relation-from rel-a))))
	 (new-to-name (make-new-world-name (world-name (relation-to rel-m))
					   (world-name (relation-to rel-a))))
	 (from (find-world new-from-name new-worlds))
	 (to (find-world new-to-name new-worlds)))
    (when (and from to)
      (make-relation :from from :to to))))
 
(defun make-new-relations-for-agent (M A new-worlds ag)    
  ;(format t "A: ~S~%" A)
  (let ((new-relations-for-agent '())
	(relations-ag-m (find-relation-for-agent M ag))
	(relations-ag-a (find-relation-for-agent A ag)))
    ;(format t "rel-m: ~S~%rel-a: ~S~%" relations-ag-m relations-ag-a)
    (dolist (rel-m relations-ag-m)
      (dolist (rel-a relations-ag-a)
	(let ((new-rel (make-new-relation rel-m rel-a new-worlds)))
	  ;(format t "from: ~S~% to:~S ~%" from to)
	  (when new-rel
	    (push new-rel new-relations-for-agent)))))
    (reverse new-relations-for-agent)))

(defun make-new-relations (M A new-worlds)
  ;(format t "new-worlds: ~S~%" new-worlds)
  (let* ((agents (kripke-model-agents M))
	 (new-relations '()))
    (dolist (ag agents)
      ;(format t "new-relations: ~S~%" new-relations)
      (let ((new-relations-ag (make-new-relations-for-agent M A new-worlds ag)))
	;(format t "new-relations-ag: ~S~%" new-relations-ag)
	(setf new-relations (pairlis (list ag) (list new-relations-ag) new-relations))))
    ;(format t "new relations: ~S~%" new-relations)
    new-relations))

(defun make-new-real-worlds (M A new-worlds)
  nil)

(defun update-possible-p (M A)
  (and 
   (eq (length (kripke-model-agents M)) 
       (length (kripke-model-agents A)))))
;   (loop for a1 in (kripke-model-agents M1) for a2 in (kripke-model-agents A)
;	    always (eq a1 a2))))
       
(defun product-update (M A)
  (if (not (update-possible-p M A))
      (error "Set of agents of M and A differ")
      (let* ((new-worlds (make-new-worlds M A))
	     (new-relations (make-new-relations M A new-worlds))
	     (new-real-worlds (make-new-real-worlds M A new-worlds)))
	(make-kripke-model :worlds new-worlds
			   :relations new-relations
			   :agents (kripke-model-agents M)))))

      
  
  
