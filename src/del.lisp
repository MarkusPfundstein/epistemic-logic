(defpackage #:del
  (:export #:product-update)
  (:use #:cl #:iterate #:alexandria #:func #:kripke))

(in-package #:del)

(defun positions (item sequence &key (test #'eql))
  (mapcar #'car
          (remove item (map 'list #'cons (iota (length sequence))
			    sequence)
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

(defun make-new-worlds (M A)
  (iter-cartesian2 ((w e) (kripke-model-worlds M) (kripke-model-worlds A))
      (when (models M w (world-preconditions e))
	(collect (make-world :name (make-new-world-name (world-name w)
							(world-name e))
			     :propositions (update-val
					    (world-propositions w)
					    (world-additions e)
					    (world-substitutions e)))))))

(defun make-new-relation (rel-m rel-a new-worlds)
  (when-let* ((new-from-name (make-new-world-name
			      (world-name (relation-from rel-m))
			      (world-name (relation-from rel-a))))
	      (new-to-name (make-new-world-name
			    (world-name (relation-to rel-m))
			    (world-name (relation-to rel-a))))
	      (from (find-world new-from-name new-worlds))
	      (to (find-world new-to-name new-worlds)))
    (make-relation :from from :to to)))
 
(defun make-new-relations-for-agent (M A new-worlds ag)
  (iter-cartesian2 ((rel-m rel-a)
		    (find-relation-for-agent M ag)
		    (find-relation-for-agent A ag))
      (when-let ((new-rel (make-new-relation rel-m rel-a new-worlds)))
	(collect new-rel))))
		    
(defun make-new-relations (M A new-worlds)
  ; to obfuscated to write more functional :P
  (let ((new-relations '()))
    (dolist (ag (kripke-model-agents M))
      (let ((new-relations-ag (make-new-relations-for-agent M A new-worlds ag)))
	(setf new-relations
	      (pairlis (list ag) (list new-relations-ag) new-relations))))
    new-relations))

(defun make-new-time-relations (M A new-worlds)
  (iter (for nw in new-worlds)
	(when-let* ((tmp (extract-names nw))
		    (w (find-world-by-name M (car tmp)))
		    (e (find-world-by-name A (cadr tmp))))
	  ;(format t "link ~S to ~S by ~S~%"
		;  (world-name nw) (world-name w) (world-name e))
	  (collect (cons nw w)))))

(defun make-new-real-worlds (M A new-worlds)
  (iter (for nw in new-worlds)
	;(format t "nw: ~S~%" (world-name nw))
	(when-let* ((tmp (extract-names nw))
		    (w (find-real-world-by-name M (car tmp)))
		    (e (find-real-world-by-name A (cadr tmp))))
	  (collect nw))))
	  ;(if (and w e)
	      ;(collect nw)
	      ;(format t "tmp: ~S - w: ~S e: ~S~%~%" tmp (if w (world-name w) nil) (if e (world-name e) nil))))))

(defun update-possible-p (M A)
  (and 
   (eq (length (kripke-model-agents M)) 
       (length (kripke-model-agents A)))))
       
(defun product-update (M A)
  ;(format t "real worlds:~%M:~S~%A:~S~%" 
	;  (mapcar #'(lambda (rw) (world-name rw)) (kripke-model-real-worlds M))
	 ; (mapcar #'(lambda (rw) (world-name rw)) (kripke-model-real-worlds A)))
  (if (not (update-possible-p M A))
      (error "Set of agents of M and A differ")
      (let* ((new-worlds (make-new-worlds M A))
	     (new-relations (make-new-relations M A new-worlds))
	     (new-real-worlds (make-new-real-worlds M A new-worlds))
	     (new-time-relations (make-new-time-relations M A new-worlds)))
	(make-kripke-model :worlds new-worlds
			   :previous-model M
			   :real-worlds new-real-worlds
			   :time-relations new-time-relations
			   :comgraph (kripke-model-comgraph M)
			   :relations new-relations
			   :agents (kripke-model-agents M)))))

      
  
  
