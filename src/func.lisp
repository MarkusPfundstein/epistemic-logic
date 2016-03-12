(defpackage #:func
  (:use #:cl #:iterate)
  (:export
   #:contains-nil
   #:find-nil
   #:find-true
   #:find-all
   #:find-all-if
   #:cartesian2
   #:iter-cartesian2))

(in-package #:func)

(defmacro find-nil (seq)
  `(contains-nil ,seq))

(defun find-true (seq)
  (iter (for s in seq)
	 (when s (return-from find-true t))))
	  

(defun contains-nil (seq)
  (iter (for s in seq)
	(when (null s)
	  (return-from contains-nil t))))

(defun find-all (item seq &rest keyword-args
		 &key (test #'eql) test-not &allow-other-keys)
  (if test-not
      (apply #'remove item seq
	     :test-not (complement test-not) keyword-args)
      (apply #'remove item seq
	     :test (complement test) keyword-args) ) )

(defun find-all-if (pred seq &rest keyword-args)
  (apply #'remove-if (complement pred) seq keyword-args) )

(defmacro cartesian2 (seq1 seq2)
  `(alexandria:mappend #'(lambda (e1)
			   (mapcar #'(lambda (e2) (list e1 e2)) ,seq2))
		       ,seq1))

(defmacro iter-cartesian2 (((e1 e2) seq1 seq2) cmd)
  `(iter (for (,e1 ,e2) in (cartesian2 ,seq1 ,seq2))
	 ,cmd))
