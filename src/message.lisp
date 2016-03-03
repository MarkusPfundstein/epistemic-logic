(defpackage #:message
  (:export #:make-message-action-model
	   #:make-message-sent-model
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
(defun make-message-action-model (M message)
  (let* ((em (make-world 
	      :name "em" 
	      :additions (list message)
	      :propositions (list
			     (cons :KNOWS 
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
		       :real-worlds (list em e-not))))

(defun make-message-sent-model (M message &key (positive t))
  (let* ((e-sent (make-world :name "e-sent" :propositions (list message)))
	 (e-not-sent (make-world :name "e-not-sent" :propositions (cons :NOT (list message))))
	 (rel-all (mapcar #'(lambda (agent)
			      (cons agent (list
					   (make-relation :from e-sent :to e-sent)
					   (make-relation :from e-not-sent :to e-not-sent)
					   (make-relation :from e-sent :to e-not-sent)
					   (make-relation :from e-not-sent :to e-sent))))
			  (kripke-model-agents M))))
    (make-kripke-model :worlds (list e-sent e-not-sent)
		       :relations rel-all
		       :agents (kripke-model-agents M)
		       :real-worlds (list (if positive e-sent e-not-sent)))))
	 
    
(defun message-update (M message &key (positive t))
  (let ((M1 (product-update M (make-message-action-model M message))))
    (product-update M1 (make-message-sent-model M1 message :positive positive))))

	  
