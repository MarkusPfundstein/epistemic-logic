(defpackage #:wsn
  (:use #:cl #:kripke #:del #:message #:comgraph)
  (:export #:can-send-p))

(in-package #:wsn)

; 
(defun can-send-p (M msg)
  (let* ((recs (message-receiver msg))
	 (sender (message-sender msg))
	 (neighs (neighbours (kripke-model-comgraph M) sender)))
    (dolist (r recs) 
      (when (not (member r neighs))
	(return-from can-send-p nil)))
    t))
