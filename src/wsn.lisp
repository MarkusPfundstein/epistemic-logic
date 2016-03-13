(defpackage #:wsn
  (:use #:cl #:iterate #:alexandria #:kripke #:del #:message #:comgraph)
  (:export #:can-send-p
	   #:make-observe-proposition-update
	   #:make-state-change-update-experimental
	   #:make-state-change-update
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
  ; To-DO: should work for multiple agents
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
	   (rel-all (make-empty-relations (kripke-model-agents M) (list e-obs e-not))))
      (iter  (for ag in (kripke-model-agents M))
	     (unless (eq observing-agent ag)
	       (connect-worlds! rel-all e-obs e-not ag)))
      (make-kripke-model :worlds (list e-obs e-not)
			 :relations rel-all
			 :agents (kripke-model-agents M)
			 :real-worlds (list e-obs)))))

(defun make-state-change-update (M observer precondition from-state to-state &key (others nil))
  (let* ((observing-agent (if (agent-p observer) ;
			      observer 
			      (find-agent observer (kripke-model-agents M))))
	 (other-observers (mapcar #'(lambda (ag)
				      (if (agent-p ag) 
					  ag
					  (find-agent ag (kripke-model-agents M))))
				  others))
	 (all-observers (union other-observers (list observing-agent)))
	 (independent (= (length all-observers)
			 (length (kripke-model-agents M)))))
    (when (not observing-agent) 
      (error "agent not found"))
    (let* ((e-ign-sc (make-world :name "e-ign-sc"
				 :propositions '(:TRUE)))
	   (e-sc (make-world :name "e-sc"
			     :propositions (unify-formula
					    '(:AND
					      (:KNOWS ?AG ?PRE)
					      ?FROM)
					    (pairlis '(?AG ?PRE ?FROM)
						     (list observer precondition from-state)))
			     :substitutions (list (cons from-state to-state))))
	   (rel-all (make-empty-relations (kripke-model-agents M) (list e-ign-sc e-sc))))
      (iter (for ag in (kripke-model-agents M))
	    (unless (member ag all-observers)
	      (connect-worlds! rel-all e-ign-sc e-sc ag)))
      (make-kripke-model :worlds (append (list e-sc) (if independent 
							 nil
							 (list e-ign-sc)))
			 :relations rel-all
			 :agents (kripke-model-agents M)
			 :real-worlds (list e-sc)))))

(defun make-state-change-update-experimental (M observer from-state to-state &key (others nil))
  (let* ((observing-agent (if (agent-p observer) ;
			      observer 
			      (find-agent observer (kripke-model-agents M))))
	 (other-observers (mapcar #'(lambda (ag)
				      (if (agent-p ag) 
					  ag
					  (find-agent ag (kripke-model-agents M))))
				  others))
	 (all-observers (union other-observers (list observing-agent)))
	 (independent (= (length all-observers)
			 (length (kripke-model-agents M)))))
    (when (not observing-agent) 
      (error "agent not found"))
    (let* ((e-ign-sc (make-world :name "e-x-ign-sc"
				 :propositions '(:TRUE)))
	   (e-sc (make-world :name "e-x-sc"
			     :propositions (unify-formula
					    '(?FROM)
					    (pairlis '(?FROM)
						     (list from-state)))
			     :substitutions (list (cons from-state to-state))))
	   (e-no-sc (make-world :name "e-x-no-sc"
				:propositions (unify-formula
					       '(?TO)
					       (pairlis '(?TO)
							(list to-state)))))
	   (rel-all (make-empty-relations (kripke-model-agents M) (list e-ign-sc
									e-sc
									e-no-sc))))
      (iter (for ag in (kripke-model-agents M))
	    (connect-worlds! rel-all e-no-sc e-sc ag)
	    (unless (member ag all-observers)
	      (connect-worlds! rel-all e-ign-sc e-no-sc ag)
	      (connect-worlds! rel-all e-ign-sc e-sc ag)))
      (make-kripke-model :worlds (append (list e-sc e-no-sc) (if independent 
								 nil
								 (list e-ign-sc)))
			 :relations rel-all
			 :agents (kripke-model-agents M)
			 :real-worlds (list e-no-sc)))))
			       


(defun wsn-learn-prop (M who prop)
  (product-update M (make-observe-proposition-update M who prop)))

(defun wsn-message-update (M msg)
  (if (can-send-p M msg)
      (message-update M msg :positive t)
      (error "message recipients not reachable by comgraph")))
