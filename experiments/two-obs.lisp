(defpackage #:two-obs
    (:use #:cl #:iterate #:alexandria #:func #:kripke #:del #:message #:comgraph #:wsn #:drawing #:sim))

(in-package #:two-obs)

(defun make-model ()
  (let* ((agent-a (make-agent :name "a"))
	 (agent-b (make-agent :name "b"))
	 (agent-c (make-agent :name "c"))
	 (agents (list agent-a agent-b agent-c))
	 (world-w (make-world :name "w"
			      :propositions '(:o)))
	 (relations (make-empty-relations agents
					  (list world-w))))
    (make-kripke-model :worlds (list world-w)
		       :real-worlds (list world-w)
		       :relations relations
		       :agents agents)))

; in M-3 , the real world is _NOT_ serial anymore (no a arrow)
(defun run-experiment (&key
			 (exp-path "/Users/markus/experiments/two-obs/")
			 (keep-dot-file nil))
			 
  (let* ((M-init (make-model))
	 (M-2 (product-update M-init
			      (make-observe-proposition-update
			       M-init 'b '(:P 1))))
	 (M-3 (product-update M-2
			      (make-message-action-model
			       M-2 '(b (:P 1) (a b)))))
	 (M-4 (product-update M-3
			      (make-doxastic-observe-proposition-update
			       M-3 'c '(:A 2)))))
    (write-model M-init 1 exp-path
		 :keep-dot-file keep-dot-file)
    (write-model M-2 2 exp-path
		 :keep-dot-file keep-dot-file)
    (write-model M-3 3 exp-path
		 :keep-dot-file keep-dot-file)
    (write-model M-4 4 exp-path
		 :digraph t
		 :draw-reflexive t
		 :keep-dot-file t)))
    
    

; Observe -> better learn value of p, not only p
; (?p) <-> A-{a} <-> (?¬p)
; [e](K_a(p) of K_a(¬p) i.p.v. Y
; minimum -> remove eventually T and M_Y
; discussion for Leen
; S5, sensor protocols, communication graph
	 
    

    


		       










