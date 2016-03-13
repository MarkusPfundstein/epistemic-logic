(defpackage #:state-change-before-message-2-pushes
    (:use #:cl #:iterate #:alexandria #:func #:kripke #:del #:message #:comgraph #:wsn #:drawing #:sim))

(in-package #:state-change-before-message-2-pushes)

(defparameter t-world-w (make-world :name "w"
				    :propositions '(:OPEN)))

(defparameter t-proto-b (make-protocol
			 :precondition-fn (lambda (M ts) 
					    (let ((f (unify-formula '(:OBSERVE b (:PUSH ?TIME))
							    (pairlis '(?TIME)
								     (list ts)))))
					      (format t "[~S] check precondition for b: ~S~%" ts f)
					      (let ((res (models-some M f)))
						(when res (format t "-> satisfied~%"))
						res)))
			 :action-fn (lambda (M ts) 
				      (let ((f (unify-formula '(b (:PUSH ?TIME) (a b))
								   (pairlis '(?TIME)
									    (list ts)))))
					(format t "[~S] make message action model for b: ~S~%" ts f)
					(make-message-action-model M f)))))

(defparameter t-proto-a-1 (make-protocol
			   :precondition-fn (lambda (M ts)
					      (let ((f (unify-formula '(:AND 
									(:OBSERVE a (:PUSH ?TIME))
									(:OPEN))
								      (pairlis '(?TIME)
									       (list ts)))))
						(format t "[~S] check precondition for a: ~S~%" ts f)
						(let ((res (models-some M f)))
						  (when res (format t "-> satisfied~%"))
						  res)))
			   :action-fn (lambda (M ts)
					(let ((f (unify-formula '(:PUSH ?TIME)
								(pairlis '(?TIME) (list ts)))))
					  (format t "[~S] make state change action model for a: ~S~%" ts f)
					  (make-state-change-update M 'a f ':OPEN ':CLOSED)))))

(defparameter t-proto-a-2 (make-protocol
			   :precondition-fn (lambda (M ts)
					      (let ((f (unify-formula '(:AND 
									(:OBSERVE a (:PUSH ?TIME))
									(:CLOSED))
								      (pairlis '(?TIME)
									       (list ts)))))
						(format t "[~S] check precondition for a: ~S~%" ts f)
						(let ((res (models-some M f)))
						  (when res (format t "-> satisfied~%"))
						  res)))
			   :action-fn (lambda (M ts)
					(let ((f (unify-formula '(:PUSH ?TIME)
								(pairlis '(?TIME) (list ts)))))
					  (format t "[~S] make state change action model for a: ~S~%" ts f)
					  (make-state-change-update M 'a f ':CLOSED ':OPEN)))))

(defparameter t-proto-a-3 (make-protocol
			   :precondition-fn (lambda (M ts)
					      (let ((f '(:OBSERVE a :CLOSED)))
						(format t "[~S] call precondition for a: ~S~%" ts f)
						(let ((res (models-some M f)))
						  (when res (format t "-> satisfied~%"))
						  res)))
			   :action-fn (list 
				       (lambda (M ts)
					 (let ((f ':TRUE))
					   (format t "[~S] make experimental state change model for a: ~S~%" ts f)
					   (make-state-change-update-experimental
					    M 'a ':OPEN ':CLOSED :others '(b c))))
				       (lambda (M ts)
					 (let ((f '(a :CLOSED (a b c))))
					   (format t "[~S] make message action model for a: ~S~%" ts f)
					   (make-message-action-model M f))))))


(defparameter t-proto-a-4 (make-protocol
			   :precondition-fn (lambda (M ts)
					      (let ((f '(:OBSERVE a :OPEN)))
						(format t "[~S] call precondition for a: ~S~%" ts f)
						(let ((res (models-some M f)))
						  (when res (format t "-> satisfied~%"))
						  res)))
			   :action-fn (list 
				       (lambda (M ts)
					 (let ((f ':TRUE))
					   (format t "[~S] make experimental state change model for a: ~S~%" ts f)
					   (make-state-change-update-experimental
					    M 'a ':CLOSED ':OPEN :others '(b c))))
				       (lambda (M ts)
					 (let ((f '(a :OPEN (a b c))))
					   (format t "[~S] make message action model for a: ~S~%" ts f)
					   (make-message-action-model M f))))))
						        
(defparameter t-agent-a (make-agent :name "a"
				    :protocols (list 
						t-proto-a-1 
						t-proto-a-2 
						t-proto-a-3
						t-proto-a-4)))
(defparameter t-agent-b (make-agent :name "b" 
				    :protocols (list t-proto-b)))
(defparameter t-agent-c (make-agent :name "c"))

(defparameter t-comgraph (make-comgraph '(a b c)))
; b <-> a <-> c
(add-undirected-edge t-comgraph 'a 'b)
(add-undirected-edge t-comgraph 'a 'c)

(defparameter t-rel-a (list
		       (make-relation :from t-world-w :to t-world-w)))
(defparameter t-rel-b (list
		       (make-relation :from t-world-w :to t-world-w)))
(defparameter t-rel-c (list
		       (make-relation :from t-world-w :to t-world-w)))

(defparameter M-init (make-kripke-model
		      :worlds (list t-world-w)
		      :relations (pairlis (list t-agent-a
						t-agent-b
						t-agent-c)
					  (list t-rel-a
						t-rel-b
						t-rel-c))
		      :comgraph t-comgraph
		      :agents (list t-agent-a t-agent-b t-agent-c)
		      :real-worlds (list t-world-w)))

(assert (eq t (models M-init "w" '(:AND
				   (:KNOWS a :OPEN)
				   (:KNOWS b :OPEN)
				   (:KNOWS c :OPEN)))))

(defparameter test-events (list 
			    (lambda (M ts) 
			      (let ((f (unify-formula '(:PUSH ?TIME)
						      (pairlis '(?TIME)
							       (list ts)))))
				(format t "[~S] call event ~S~%" ts f)
				(make-observe-proposition-update M 'b f)))
			    (lambda (M ts) 
			      (let ((f (unify-formula '(:PUSH ?TIME)
						      (pairlis '(?TIME)
							       (list ts)))))
				(format t "[~S] call event ~S~%" ts f)
				(make-observe-proposition-update M 'b f)))))

(defun run-experiment (&key (return-model nil))
  (let ((m (run-test-sim M-init test-events 
			 :dest-path "/Users/markus/experiments/state-change-before-message-2-pushes" 
			 :keep-dot-file nil)))
    (if return-model 
      m
      nil)))

					     
