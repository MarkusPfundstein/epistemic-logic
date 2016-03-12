(defpackage #:func
  (:use #:cl #:iterate)
  (:export
   #:find-all
   #:find-all-if
   #:cartesian2
   #:iter-cartesian2))

(in-package #:func)

(defun find-all (item seq &rest keyword-args &key (test #'eql) test-not &allow-other-keys)
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
