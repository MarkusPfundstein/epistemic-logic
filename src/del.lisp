(defpackage #:del
  (:export #:product-update)
  (:use #:cl #:kripke))

(in-package #:del)

(defun positions (item sequence &key (test #'eql))
  (mapcar #'car
          (remove item (map 'list #'cons (alexandria:iota (length sequence)) sequence)
		  :test-not test
		  :key #'cdr)))

(defun extract-names (w)
  (let* ((pos (car (reverse (positions #\, (world-name w)))))
	 (w-name (subseq (world-name w) 1 pos))
	 (e-name (subseq (world-name w) (+ pos 2) (1- (length (world-name w))))))
    (list w-name e-name)))

(defun make-new-world-name (w e)
  (concatenate 'string "(" w ", " e ")"))

(defun find-world (name worlds)
  (find-if #'(lambda (n) (string= (world-name n) name)) worlds))

(defun dottedp (l)
  (and (consp l)
       (not (consp (cdr l)))))

(defun update-propositions (props add-props sub-props)
  (union
   (union (set-difference props (map 'list #'(lambda (e) (car e)) sub-props))
	  add-props)
   (map 'list #'(lambda (e) (cdr e)) sub-props)))

(defun make-new-worlds (M A)
  (let ((new-worlds '()))
    (dolist (w (kripke-model-worlds M))
      (dolist (e (kripke-model-worlds A))
	(when (models M w (world-propositions e))
	  ;(format t "add world: ~S~%" w)
	  (push (make-world 
		 :name (make-new-world-name (world-name w) (world-name e))
		 :propositions (update-propositions (world-propositions w)
						    (world-additions e)
						    (world-substitutions e)))
		 ;(append (world-propositions w) (world-substitutions e)))
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
  (let ((new-relations-for-agent '()))
    ;(format t "rel-m: ~S~%rel-a: ~S~%" relations-ag-m relations-ag-a)
    (dolist (rel-m (find-relation-for-agent M ag))
      (dolist (rel-a (find-relation-for-agent A ag))
	(let ((new-rel (make-new-relation rel-m rel-a new-worlds)))
	  ;(format t "from: ~S~% to:~S ~%" from to)
	  (when new-rel
	    (push new-rel new-relations-for-agent)))))
    (reverse new-relations-for-agent)))

(defun make-new-relations (M A new-worlds)
  ;(format t "new-worlds: ~S~%" new-worlds)
  (let ((new-relations '()))
    (dolist (ag (kripke-model-agents M))
      ;(format t "new-relations: ~S~%" new-relations)
      (let ((new-relations-ag (make-new-relations-for-agent M A new-worlds ag)))
	;(format t "new-relations-ag: ~S~%" new-relations-ag)
	(setf new-relations (pairlis (list ag) (list new-relations-ag) new-relations))))
    ;(format t "new relations: ~S~%" new-relations)
    new-relations))

(defun make-new-real-worlds (M A new-worlds)
  (let ((worlds '()))
    (loop for nw in new-worlds do
	 (let* ((tmp (extract-names nw))
		(w-name (car tmp))
		(e-name (cadr tmp))
		(w (find-real-world-by-name M w-name))
		(e (find-real-world-by-name A e-name)))
	   (when (and w e)
	     (push nw worlds))))
    worlds))
	    ;  (w-name ));
	; )

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
			   :real-worlds new-real-worlds
			   :comgraph (kripke-model-comgraph M)
			   :relations new-relations
			   :agents (kripke-model-agents M)))))

      
  
  
