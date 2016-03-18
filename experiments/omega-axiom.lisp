(defpackage #:omega-axiom
    (:use #:cl #:iterate #:alexandria #:func #:kripke #:del #:message #:comgraph #:wsn #:drawing #:sim))

(in-package #:omega-axiom)

(defun make-model ()
  (let* ((agent-a (make-agent :name "a" :symbol 'a))
	 (agent-b (make-agent :name "b" :symbol 'b))
	 (agent-c (make-agent :name "c" :symbol 'c))
	 (agents (list agent-a agent-b agent-c))
	 (world-w (make-world :name "w"
			      :propositions '(:c)))
	 (relations (make-empty-relations agents
					  (list world-w))))
    (make-kripke-model :worlds (list world-w)
		       :real-worlds (list world-w)
		       :relations relations
		       :agents agents)))

(defun run-experiment ()
  (let* ((M-init (make-model))
		    (A-obs (make-observe-proposition-update M-init 'b '(:P 1)))
		    (M-obs (product-update M-init A-obs :update-fun 'update-val-omega))
		    (A-msg (make-message-action-model M-obs '(b (:P 1) (a b))))
		    (M-msg (product-update M-obs A-msg :update-fun 'update-val-omega))
		    (A-state (MAKE-STATE-CHANGE-UPDATE
			      M-msg 'a '(:OMEGA a (:P 1)) ':C ':O))
		    (M-state (product-update M-msg A-state :update-fun 'update-val-omega)))
			      
	       (write-model M-obs 1 "/home/markus/experiments/omega-axiom/")
	       (write-model M-msg 2 "/home/markus/experiments/omega-axiom/")
	       (write-model M-state 3 "/home/markus/experiments/omega-axiom/")))
