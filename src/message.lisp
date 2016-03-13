(defpackage #:message
  (:export #:make-message-action-model
	   #:message-update
	   #:message-sender
	   #:message-receiver
	   #:message-content)
  (:use #:cl #:kripke #:del))

(in-package #:message)

(defun message-sender (message)
  (car message))

(defun message-receiver (message)
  (caddr message))

(defun message-content (message)
  (cadr message))

(defun is-recipient (agent message)
  (let ((names (map 'list #'(lambda (ag) (string-upcase (string ag))) 
		    (message-receiver message))))
    (not (null (member (string-upcase (agent-name agent)) names :test #'string=)))))

; (message sender-name formula (receiver_1, ..., receiver_n))
(defun make-message-action-model (M message &key (positive t))
  (let* ((em (make-world 
	      :name "em" 
	      :additions (list message)
	      :propositions (unify-formula 
			     '(:KNOWS ?ag ?content)
			     (pairlis '(?ag ?content) 
				      (list (message-sender message)
					    (message-content message))))))
	 (e-not (make-world
		 :name "e-not"
		 :propositions '(:TRUE)))
	 (rel-all (mapcar #'(lambda (agent) 
			      (cons agent 
				    (append 
				     (list (make-relation :from em :to em)
					   (make-relation :from e-not :to e-not))
				     (when (not (is-recipient agent message))
				       (list (make-relation :from em :to e-not)
					     (make-relation :from e-not :to em))))))
		       (kripke-model-agents M))))
    (make-kripke-model :worlds (list em e-not)
		       :relations rel-all
		       :agents (kripke-model-agents M)
		       :real-worlds (list (if positive em e-not)))))

(defun message-update (M message &key (positive t))
  (product-update M (make-message-action-model M message :positive positive)))

	  
 
