(defpackage #:wsn
  (:use #:cl #:kripke #:del #:message #:comgraph)
  (:export #:can-send-p
	   #:make-observe-proposition-update
	   #:wsn-message-update
	   #:wsn-learn-prop))

(in-package #:wsn)

(defun can-send-p (M msg)
  (let* ((recs (message-receiver msg))
	 (sender (message-sender msg))
	 (neighs (neighbours (kripke-model-comgraph M) sender)))
    (dolist (r recs) 
      (when (not (member r neighs))
	(return-from can-send-p nil)))
    t))

(defun find-agent (ag-name agents)
  (find-if #'(lambda (agent) (equal (string-upcase (agent-name agent))
				    (string-upcase ag-name)))
	   agents))

(defun make-observe-proposition-update (M observer prop)
  (let ((observing-agent (if (agent-p observer) 
			     observer 
			     (find-agent observer (kripke-model-agents M)))))
    (when (not observing-agent) 
      (error "agent not found"))
    (let* ((e-obs (make-world :name "e-obs" 
			      :propositions '(:TRUE) 
			      :additions (list prop)))
	   (e-not (make-world :name "e-not-obs"
			      :propositions '(:TRUE)))
	   (rel-all (mapcar #'(lambda (agent)
				(cons agent 
				      (append 
				       (list (make-relation :from e-obs :to e-obs)
					     (make-relation :from e-not :to e-not))
				       (when (not (eq observing-agent agent))
					 (list (make-relation :from e-obs :to e-not)
					       (make-relation :from e-not :to e-obs))))))
			    (kripke-model-agents M))))
      (make-kripke-model :worlds (list e-obs e-not)
			 :relations rel-all
			 :agents (kripke-model-agents M)
			 :real-worlds (list e-obs)))))

(defun wsn-learn-prop (M who prop)
  (product-update M (make-observe-proposition-update M who prop)))

(defun wsn-message-update (M msg)
  (if (can-send-p M msg)
      (message-update M msg :positive t)
      (error "message recipients not reachable by comgraph")))
