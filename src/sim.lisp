(defpackage #:sim
  (:use #:cl #:iterate #:alexandria #:func #:kripke #:del #:message #:comgraph #:wsn #:drawing)
  (:export #:run-test-sim
	   #:write-model))

(in-package #:sim)

(defun evaluate-protocols (M ts)
  (reduce 'append 
	  (iter (for ag in (kripke-model-agents M))
	      (collect 
		  (iter (for proto in (agent-protocols ag))
		      (when (funcall (protocol-precondition-fn proto) M ts)
			(collect (protocol-action-fn proto))))))))
  
(defun write-model (M ts dest-path &key (keep-dot-file nil) (prefix "/m-"))
  (when dest-path
    (let ((dest (concatenate 'string dest-path prefix (write-to-string ts) ".dot")))
      (write-graphviz-png-file M dest :draw-reflexive nil :keep-dot-file keep-dot-file))))

(defun run-test-sim (M-start events &key (dest-path "~") (keep-dot-file nil))
  (let* ((M-current M-start)
	 (ts 0)
	 (model-counter 0)
	 (user-events events)
	 (event-stack (list (cons :user-event (pop user-events)))))
    (write-model M-current model-counter dest-path)
    (loop do
	 (let ((ev (pop event-stack)))
	   (incf model-counter)
	   (when (eq (car ev) :user-event)
	     (format t "[~S] increase ts~%" ts)
	     (incf ts))
	   (format t "[~S] apply next event of type: ~S~%" ts (car ev))
	   (let ((A (funcall (cdr ev) M-current ts)))
	     (when (not (kripke-model-p A))
	       (format t "[ERROR] event must be a Kripke model~%")
	       (return-from run-test-sim M-current))
	     (setf M-current (product-update M-current A))
	     (write-model A model-counter dest-path :keep-dot-file keep-dot-file :prefix "/a-")
	     (write-model M-current model-counter dest-path :keep-dot-file keep-dot-file)
	     (when (emptyp (kripke-model-real-worlds M-current))
	       (format t "[ERROR] no real world in Kripke model anymore~%")
	       (return-from run-test-sim M-current)))
	   (when (emptyp event-stack)
	     (format t "- event stack empty. check protocols -~%")
	     (when-let ((action-updates (evaluate-protocols M-current ts)))
	       (iter (for proto-action in action-updates)
		     (if (listp proto-action)
					; HERE -> reorder actions or merge them or make them concurrent
			 (iter (for action in (reverse proto-action))
			       (format t "[~S] push action~%" ts)
			       (push (cons :proto-event action) event-stack))
			 (progn
			   (push (cons :proto-event proto-action) event-stack)
			   (format t "[~S] push action~%" ts)))))
	     (when (emptyp event-stack)
	       (format t "- event stack still empty. check :user-events -~%")
	       (when-let ((new-e (pop user-events)))
		 (format t "[~S] push user event~%" ts)
		 (push (cons :user-event new-e) event-stack)))))
	 while (not (emptyp event-stack)))
    M-current))
    ; at the end.. iterate end-form-list
    ;(not (find-nil (mapcar #'(lambda (w-and-form)
;				(models M-current (car w-and-form) (cdr w-and-form)))
;			    end-form-lst)))))
					
