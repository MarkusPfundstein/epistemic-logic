(defpackage #:doxastic2
    (:use #:cl #:iterate #:alexandria #:func #:kripke #:del #:message #:comgraph #:wsn #:drawing #:sim))

(in-package #:doxastic2)

(defun make-model ()
  (let* ((agent-a (make-agent :name "a"))
	 (agent-b (make-agent :name "b"))
	 (agent-c (make-agent :name "c"))
	 (agents (list agent-a agent-b agent-c))
	 (world-w (make-world :name "w"
			      :propositions '(:o)))
	 (world-v (make-world :name "v"
			      :propositions '(:c)))
	 (relations (make-empty-relations agents
					  (list world-w
						world-v))))
         ; make it explicit that we are in a doxastic model. hence no symmetry
         ; by default...
    (connect-worlds! relations world-w world-v agents :symmetric nil)
    (connect-worlds! relations world-v world-w agents :symmetric nil)
    (make-kripke-model :worlds (list world-w world-v)
		       :real-worlds (list world-w)
		       :relations relations
		       :agents agents)))

; in M-3 , the real world is _NOT_ serial anymore (no a arrow)
(defun run-experiment (&key (exp-path "/Users/markus/experiments/doxastic1/"))
  (let* ((M-init (make-model))
	 (M-2 (product-update M-init
			      (make-doxastic-observe-proposition-update-experimental
			       M-init 'b '(:P 1))))
	 (M-3 (product-update M-2
			      (make-doxastic-message-action-model-experimental
			       M-2 '(b (:P 1) (a b))))))
    (write-model M-init 1 exp-path
		 :draw-reflexive t
		 :keep-dot-file t
		 :digraph t)
    (write-model M-2 2 exp-path
		 :draw-reflexive t
		 :keep-dot-file t
		 :digraph t)
    (write-model M-3 3 exp-path
		 :draw-reflexive t
		 :keep-dot-file t
		 :digraph t)))
    
    
    
	 
    

    


		       










