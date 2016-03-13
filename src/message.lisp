(defpackage #:message
  (:export #:make-message-action-model
	   #:message-update
	   #:message-sender
	   #:message-receiver
	   #:message-content)
  (:use #:cl #:iterate #:alexandria #:kripke #:del))

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
  (let* ((independent (= (length (message-receiver message))
			 (length (kripke-model-agents M))))
	 (em (make-world 
	      :name "em" 
	      :additions (list message)
	      :propositions (unify-formula 
			     '(:KNOWS ?ag ?content)
			     (pairlis '(?ag ?content) 
				      (list (message-sender message)
					    (message-content message))))))
	 (e-not (make-world
		 :name "e-not"
		 :propositions '(:TRUE))) ; :TRUE must be in paranthesis
	 (rels (make-empty-relations (kripke-model-agents M) (list em e-not))))
    (iter (for ag in (kripke-model-agents M))
	  (unless (is-recipient ag message)
	    (connect-worlds! rels em e-not ag)))
    (make-kripke-model :worlds (append (list em) (if independent nil (list e-not)))
		       :relations rels
		       :agents (kripke-model-agents M)
		       :real-worlds (list (if positive em e-not)))))

(defun message-update (M message &key (positive t))
  (product-update M (make-message-action-model M message :positive positive)))

	  
 
