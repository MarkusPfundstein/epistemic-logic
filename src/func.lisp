(defpackage #:func
  (:use #:cl #:iterate)
  (:export
   #:cartesian2
   #:iter-cartesian2))

(in-package #:func)

(defmacro cartesian2 (seq1 seq2)
  `(alexandria:mappend #'(lambda (e1)
			   (mapcar #'(lambda (e2) (list e1 e2)) ,seq2))
		       ,seq1))

(defmacro iter-cartesian2 (((e1 e2) seq1 seq2) cmd)
  `(iter (for (,e1 ,e2) in (cartesian2 ,seq1 ,seq2))
	 ,cmd))
