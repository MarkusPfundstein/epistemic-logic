(defpackage #:sim
  (:use #:cl #:iterate #:alexandria #:func #:kripke #:del #:message #:comgraph #:wsn #:drawing)
  (:export #:run-test-sim))≈

(in-package #:sim)

(defun evaluate-protocols (M ts)
  (reduce 'append 
	  (iter (for ag in (kripke-model-agents M))
	      (collect 
		  (iter (for proto in (agent-protocols ag))
		      (when (funcall (protocol-precondition-fn proto) M ts)
			(collect (funcall (protocol-action-fn proto) M ts))))))))
  
(defun write-model (M ts dest-path)
  (let ((dest (concatenate 'string dest-path "/sim-" (write-to-string ts) ".dot")))
    (write-graphviz-png-file M dest :draw-reflexive nil :keep-dot-file nil)))

(defun run-test-sim (M-start events &key (dest-path "~"))
  (let ((M-current M-start)
	(ts 1))
    (write-model M-current 0 dest-path)
    (iter (for ev in events)
	  (format t "evaluate event ~S at timestamp ~S~%" (car ev) ts)
	  ; apply event to current model
	  (setf M-current (funcall (cdr ev) M-current ts))
	  (write-model M-current ts dest-path)
	  ; get matching protocols in updated model
	  (if-let ((action-updates (evaluate-protocols M-current ts)))
	    (iter (for action in action-updates)
		  (incf ts)
		  (setf M-current (product-update M-current action))
		  (write-model M-current ts dest-path)) 
	  ; at the end, increase timestamp
	    (incf ts)))
    t))
    ; at the end.. iterate end-form-list
    ;(not (find-nil (mapcar #'(lambda (w-and-form)
;				(models M-current (car w-and-form) (cdr w-and-form)))
;			    end-form-lst)))))
					
