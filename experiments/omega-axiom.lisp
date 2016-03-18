(defpackage #:omega-axiom
    (:use #:cl #:iterate #:alexandria #:func #:kripke #:del #:message #:comgraph #:wsn #:drawing #:sim))

(in-package #:omega-axiom)

(defun make-model ()
  (let* ((agent-a (make-agent :name "a"))
	 (agent-b (make-agent :name "b"))
	 (agent-c (make-agent :name "c"))
	 (agents (list agent-a agent-b agent-c))
	 (world-w (make-world :name "w"
			      :propositions '(:c)))
	 (relations (make-empty-relations agents
					  (list world-w))))
    (make-kripke-model :worlds (list world-w)
		       :real-worlds (list world-w)
		       :relations relations
		       :agents agents)))
