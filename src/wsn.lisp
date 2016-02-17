(defpackage #:wsn
  (:use #:cl #:kripke #:del #:message #:comgraph)
  (:export #:can-send-p
	   #:make-observe-proposition-update
	   #:wsn-message-update))

(in-package #:wsn)

(defun can-send-p (M msg)
  (let* ((recs (message-receiver msg))
	 (sender (message-sender msg))
	 (neighs (neighbours (kripke-model-comgraph M) sender)))
    (dolist (r recs) 
      (when (not (member r neighs))
	(return-from can-send-p nil)))
    t))

(defun make-observe-proposition-update (M observer prop)
  (let* ((e-obs (make-world :name "e-obs" 
			    :propositions '(:TRUE) 
			    :substitutions (list prop)))
	 (e-not (make-world :name "e-not-obs"
			    :propositions '(:TRUE)))
	 (rel-all (map 'list #'(lambda (agent)
				 (cons agent 
				       (append 
					(list (make-relation :from e-obs :to e-obs)
					      (make-relation :from e-not :to e-not))
					(when (not (eq observer agent))
					  (list (make-relation :from e-obs :to e-not)
						(make-relation :from e-not :to e-obs))))))
		       (kripke-model-agents M))))
    (make-kripke-model :worlds (list e-obs e-not)
		       :relations rel-all
		       :agents (kripke-model-agents M)
		       :real-worlds (list e-obs))))

(defun wsn-message-update (M msg)
  (if (can-send-p M msg)
      (message-update M msg :positive t)
      M))
