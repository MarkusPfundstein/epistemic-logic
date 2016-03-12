(defpackage #:drawing
  (:use #:cl #:kripke)
  (:export #:generate-graphviz-dot-file-string
	   #:write-graphviz-dot-file
	   #:write-graphviz-png-file))

(in-package #:drawing)

(defun find-agents-for-relation (M r)
  (let ((agents-found '()))
    (dolist (all-rel (kripke-model-relations M))
      (let ((found (find-if #'(lambda (rel) (and (and (string-equal (world-name (relation-from r)) 
								   (world-name (relation-from rel))))
						 (and (string-equal (world-name (relation-to r))
								   (world-name (relation-to rel))))))
			    (cdr all-rel))))
	(when found
	  (push (car all-rel) agents-found))))
    agents-found))

(defun make-relation-name (r)
  (format nil "~S -- ~S" (world-name (relation-from r)) (world-name (relation-to r))))

(defun generate-graphviz-dot-file-string (M &key (draw-reflexive t))
  (let ((string-list '("strict graph G {")))
    (dolist (world (kripke-model-worlds M))
      (push (format nil "\"~a\" [label = \"~a : ~{~a~^, ~}\"];"
		    (world-name world) (world-name world) (world-propositions world)
		    )
	    string-list))
    (let ((rels-found '()))
      (dolist (agent (kripke-model-agents M))
	(let ((rels (find-relation-for-agent M agent)))
	  (dolist (relation rels)
	    (let ((relation-name (make-relation-name relation)))
	      (when (not (member relation-name rels-found :test 'string-equal))
					;(format t "make: ~S ~%xxx ~S~%" relation rels-found)
		(push relation-name rels-found)
		(when (or draw-reflexive
			  (not (string-equal (world-name (relation-to relation))
					     (world-name (relation-from relation)))))
		  (let ((agents-for-rel (find-agents-for-relation M relation)))
					;(format t "agents ~S for relation ~S~%" agents-for-rel relation)
		    (push (format nil 
				  "\"~a\" -- \"~a\" [label = \"~{~a~^, ~}\"];" 
				  (world-name (relation-from relation))
				  (world-name (relation-to relation))
				  (map 'list #'(lambda (ag) (agent-name ag)) agents-for-rel))
			  string-list)))))))))
    (format nil "~{~a~^~%~}" (reverse (push "}" string-list)))))

(defun write-graphviz-dot-file (M dest &key (draw-reflexive nil))
  (let ((content (generate-graphviz-dot-file-string M :draw-reflexive draw-reflexive)))
    (with-open-file (stream dest
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (format stream "~a" content))))

(defun write-graphviz-png-file (M dest 
				&key 
				  (draw-reflexive nil) (keep-dot-file nil)
				  (dot-file-path "/usr/local/bin/dot"))
  (write-graphviz-dot-file M dest :draw-reflexive draw-reflexive)
  (sb-ext:run-program dot-file-path (list "-Tpng" "-O" dest) :output *standard-output*)
  (when (not keep-dot-file)
    (with-open-file (f dest)
      (delete-file f))))
