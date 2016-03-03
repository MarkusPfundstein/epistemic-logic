(defpackage #:tests
  (:use #:cl #:iterate #:func #:kripke #:del #:message #:comgraph #:wsn #:drawing))

(in-package #:tests)

(defparameter world-w (make-world :name "w" :propositions '(:p :q (implies :p :q))))
(defparameter world-v (make-world :name "v" :propositions '(:q (message alice :q (bob)))))

(defparameter rel-a 
  (list 
   (make-relation :from world-w :to world-w)
   (make-relation :from world-v :to world-v)
   (make-relation :from world-w :to world-v)
   (make-relation :from world-v :to world-w)))

(defparameter rel-b
  (list 
   (make-relation :from world-w :to world-w)
   (make-relation :from world-v :to world-v)))

(defparameter alice (make-agent :name "alice"))
(defparameter bob (make-agent :name "bob"))

(defparameter M1 
  (make-kripke-model 
   :worlds (list world-w world-v)
   :real-worlds (list world-w)
   :agents (list alice bob)
   :relations (pairlis (list alice bob) (list rel-a rel-b))))

(defun run-tests-1 ()
  (assert (eq t (models M1 world-v '(:IMPLIES (:NOT :P) :Q))))
  (assert (eq t (models M1 "v" '(:IMPLIES (:NOT :P) :Q))))
  (assert (eq t (models M1 world-v '(:OR :p :q))))
  (assert (eq t (models M1 world-v '(:AND (:NOT :p) (:AND :q (:IMPLIES :p :q))))))
  (assert (null (models M1 world-v '(:KNOWS alice :p))))
  (assert (eq t (models M1 world-v '(:KNOWS bob (:NOT :p)))))
  (assert (eq t (models M1 world-w '(:POSSIBLE alice (:KNOWS bob :p)))))
  (assert (eq t (models M1 world-w '(:POSSIBLE alice (message alice :q (bob))))))
  (assert (eq t (models M1 "w" '(:POSSIBLE alice (:KNOWS bob :p)))))
  (assert (null (models M1 world-v '(:KNOWS alice (:KNOWS bob :p)))))
  (assert (eq t (models M1 world-v '(:KNOWS alice (:OR (:KNOWS bob :p) (:KNOWS bob (:NOT :p)))))))
  t)

(defun run-tests-2 ()
  (let* ((event-a (make-world :name "e" :propositions '(:p)))
	 (rel-event (list (make-relation :from event-a :to event-a)))
	 (A1 (make-kripke-model :worlds (list event-a)
			       :real-worlds (list event-a)
			       :agents (list alice bob)
			       :relations (pairlis (list alice bob)
						   (list rel-event rel-event)))))
    (assert (eq 1 (length (kripke-model-worlds (product-update M1 A1)))))
    (assert (eq t (models (product-update M1 A1) "(w, e)" '(:AND (:KNOWS bob :P) (:KNOWS alice :P)))))
  t))


;; Chapter 2 ;;

(defun run-tests-3 ()
  (let* ((device-a (make-agent :name "a"))
	 (device-b (make-agent :name "b"))
	 (device-c (make-agent :name "c"))
	 (world-u (make-world :name "u" :propositions '(:EARTHQUAKE)))
	 (world-v (make-world :name "v" :propositions '()))
	 (rel-device-a
	  (list (make-relation :from world-u :to world-u)
		(make-relation :from world-v :to world-v)))
	 (rel-device-b
 	  (list (make-relation :from world-u :to world-u)
		(make-relation :from world-v :to world-v)
		(make-relation :from world-u :to world-v)
		(make-relation :from world-v :to world-u)))
	 (rel-device-c
	  (list (make-relation :from world-u :to world-u)
		(make-relation :from world-v :to world-v)
		(make-relation :from world-u :to world-v)
		(make-relation :from world-v :to world-u)))
	 (M (make-kripke-model :worlds (list world-u world-v)
			       :real-worlds (list world-u)
			       :agents (list device-a device-b device-c)
			       :relations (pairlis 
					   (list device-a device-b device-c)
					   (list rel-device-a rel-device-b rel-device-c)))))
    ; a knows there was an earthquake
    (assert (eq t (models M "u" '(:KNOWS a :EARTHQUAKE))))
    ; b considers it possible that there was no earthquake
    (assert (eq t (models M "u" '(:POSSIBLE b (:NOT :EARTHQUAKE)))))
    ; c doesn't know there there was no earthquake
    (assert (eq t (models M "v" '(:NOT :KNOWS c (:NOT :EARTHQUAKE)))))
    ; a knows that both, b and c, don't know that there was an earthquake
    (assert (eq t (models M "u" '(:KNOWS a (:AND (:NOT (:KNOWS b :EARTHQUAKE))
					         (:NOT (:KNOWS c :EARTHQUAKE)))))))
    ; c doesn't know that a knows
    (assert (eq t (models M "u" '(:NOT (:KNOWS c (:KNOWS a :EARTHQUAKE))))))
    ; c doesn't know that b doesn't know. that should fail. there is thus error in the example
;    (assert (eq t (models M "w" '(:NOT :KNOWS c (:NOT (:KNOWS b :EARTHQUAKE))))))
    (let* ((event-d (make-world :name "d" :propositions '(:EARTHQUAKE)))
	   (event-e (make-world :name "e" :propositions '(:TRUE)))
	   (rel-event-device-a (list
				(make-relation :from event-d :to event-d)
				(make-relation :from event-e :to event-e)))
	   (rel-event-device-c (list
				(make-relation :from event-d :to event-d)
				(make-relation :from event-e :to event-e)))
	   (rel-event-device-b (list
				(make-relation :from event-d :to event-d)
				(make-relation :from event-e :to event-e)
				(make-relation :from event-d :to event-e)
				(make-relation :from event-e :to event-d)))
	   (Ax (make-kripke-model :worlds (list event-d event-e)
				  :real-worlds (list event-d event-e)
				  :agents (kripke-model-agents M)
				  :relations (pairlis 
					     (list device-a device-b device-c)
					     (list rel-event-device-a
						   rel-event-device-b
						   rel-event-device-c))))
	   (M-new (product-update M Ax)))
      (assert (eq 3 (length (kripke-model-worlds M-new))))
      (assert (eq t (models M-new "(u, d)" '(:KNOWS c :EARTHQUAKE))))
      (assert (eq t (models M-new "(v, e)" '(:NOT (:KNOWS c :EARTHQUAKE)))))
      (assert (eq t (models M-new "(u, d)" '(:NOT (:KNOWS b :EARTHQUAKE)))))
      (assert (eq t (models M-new "(u, d)" '(:KNOWS a :EARTHQUAKE))))
      (assert (eq t (models M-new "(v, e)" '(:KNOWS a (:NOT :EARTHQUAKE)))))
      (assert (eq t (models M-new "(u, e)" '(:KNOWS a :EARTHQUAKE))))
      ; in the real world, c doesn't know if b knows earthquake or doesn't know earthquake
      (assert (eq t (models M-new "(u, d)" '(:NOT (:KNOWS c) 
					     (:OR 
					      (:KNOWS b :EARTHQUAKE)
					      (:NOT (:KNOWS b :EARTHQUAKE)))))))
      ; c knows that b doesn't know earthquake
      (assert (eq t (models M-new "(u, d)" '(:KNOWS c (:NOT (:KNOW b :EARTHQUAKE))))))

      )
    t))

(defun run-tests-4 ()
  (let* ((device-a (make-agent :name "a"))
	 (device-b (make-agent :name "b"))
	 (device-c (make-agent :name "c"))
	 (world-u (make-world :name "u" :propositions '(:EARTHQUAKE)))
	 (world-v (make-world :name "v" :propositions '()))
	 (rel-device-a
	  (list (make-relation :from world-u :to world-u)
		(make-relation :from world-v :to world-v)))
	 (rel-device-b
 	  (list (make-relation :from world-u :to world-u)
		(make-relation :from world-v :to world-v)
		(make-relation :from world-u :to world-v)
		(make-relation :from world-v :to world-u)))
	 (rel-device-c
	  (list (make-relation :from world-u :to world-u)
		(make-relation :from world-v :to world-v)
		(make-relation :from world-u :to world-v)
		(make-relation :from world-v :to world-u)))
	 (M (make-kripke-model :worlds (list world-u world-v)
			       :real-worlds (list world-u)
			       :agents (list device-a device-b device-c)
			       :relations (pairlis 
					   (list device-a device-b device-c)
					   (list rel-device-a rel-device-b rel-device-c)))))
    (let* ((Am (make-message-action-model M '(a :EARTHQUAKE (a b))))
	   (M2 (product-update M Am)))
      ;(format t "~S~%" M2)
      ; a and b know now :EARTHQUAKE in (u, em)
      (assert (eq t (models M2 "(u, em)" '(:AND (:KNOWS b :EARTHQUAKE) (:KNOWS a :EARTHQUAKE)))))
      ; a and b know that message was sent
      (assert (eq t (models M2 "(u, em)" '(:AND (:KNOWS b (a :EARTHQUAKE (a b)))
					        (:KNOWS a (a :EARTHQUAKE (a b)))))))
      ; c doesn't know about message
      (assert (eq t (models M2 "(u, em)" '(:NOT :KNOWS c (a :EARTHQUAKE (a b))))))
      ; a and b dont know about message in worlds where it was not sent
      (assert (eq t (models M2 "(u, e-not)" '(:NOT :KNOWS a (a :EARTHQUAKE (a b))))))
      (assert (eq t (models M2 "(u, e-not)" '(:NOT :KNOWS b (a :EARTHQUAKE (a b))))))
      (assert (eq t (models M2 "(v, e-not)" '(:NOT :KNOWS a (a :EARTHQUAKE (a b))))))
      (assert (eq t (models M2 "(v, e-not)" '(:NOT :KNOWS b (a :EARTHQUAKE (a b))))))
      ; c doesnt know :EARTHQUAKE in (u, em)
      (assert (eq t (models M2 "(u, em)" '(:NOT :KNOWS c :EARTHQUAKE))))
      ; in e-not model, everything stays the same (like in tests 3)
      (assert (eq t (models M2 "(u, e-not)" '(:KNOWS a :EARTHQUAKE))))
      (assert (eq t (models M2 "(u, e-not)" '(:POSSIBLE b (:NOT :EARTHQUAKE)))))
      (assert (eq t (models M2 "(v, e-not)" '(:NOT :KNOWS c (:NOT :EARTHQUAKE)))))
      (assert (eq t (models M2 "(u, e-not)" '(:KNOWS a (:AND (:NOT (:KNOWS b :EARTHQUAKE))
					      (:NOT (:KNOWS c :EARTHQUAKE)))))))
      (assert (eq t (models M2 "(u, e-not)" '(:NOT (:KNOWS c (:KNOWS a :EARTHQUAKE))))))
      (let* ((Ap (make-message-sent-model M2 '(a :EARTHQUAKE (a b))))
	     (M3 (product-update M2 Ap)))
	;(format t "~S~%" M3)
	(assert (eq 1 (length (kripke-model-real-worlds M3))))
	(assert (eq t (= (length (kripke-model-worlds M2)) (length (kripke-model-worlds M3)))))
	(assert (eq t (= (length (kripke-model-relations M2)) (length (kripke-model-relations M3)))))
	(assert (eq t (models M3 "((u, em), e-sent)" 
			      '(:AND (:KNOWS b :EARTHQUAKE) (:KNOWS a :EARTHQUAKE)))))
					; a and b know that message was sent
	(assert (eq t (models M3 "((u, em), e-sent)" '(:AND (:KNOWS b (a :EARTHQUAKE (a b)))
					     (:KNOWS a (a :EARTHQUAKE (a b)))))))
					; c doesn't know about message
	(assert (eq t (models M3 "((u, em), e-sent)" '(:NOT :KNOWS c (a :EARTHQUAKE (a b))))))
					; a and b dont know about message in worlds where it was not sent
	(assert (eq t (models M3 "((u, e-not), e-not-sent)" '(:NOT :KNOWS a (a :EARTHQUAKE (a b))))))
	(assert (eq t (models M3 "((u, e-not), e-not-sent)" '(:NOT :KNOWS b (a :EARTHQUAKE (a b))))))
	(assert (eq t (models M3 "((v, e-not), e-not-sent)" '(:NOT :KNOWS a (a :EARTHQUAKE (a b))))))
	(assert (eq t (models M3 "((v, e-not), e-not-sent)" '(:NOT :KNOWS b (a :EARTHQUAKE (a b))))))
					; c doesnt know :EARTHQUAKE in (u, em)
	(assert (eq t (models M3 "((u, em), e-sent)" '(:NOT :KNOWS c :EARTHQUAKE))))
					; in e-not model, everything stays the same (like in tests 3)
	(assert (eq t (models M3 "((u, e-not), e-not-sent)" '(:KNOWS a :EARTHQUAKE))))
	(assert (eq t (models M3 "((u, e-not), e-not-sent)" '(:POSSIBLE b (:NOT :EARTHQUAKE)))))
	(assert (eq t (models M3 "((v, e-not), e-not-sent)" '(:NOT :KNOWS c (:NOT :EARTHQUAKE)))))
	(assert (eq t (models M3 "((u, e-not), e-not-sent)" '(:KNOWS a (:AND (:NOT (:KNOWS b :EARTHQUAKE))
							  (:NOT (:KNOWS c :EARTHQUAKE)))))))
	(assert (eq t (models M3 "((u, e-not), e-not-sent)" '(:NOT (:KNOWS c (:KNOWS a :EARTHQUAKE))))))
	(let ((M4 (message-update M '(a :EARTHQUAKE (a b)))))
	  (assert (equal (format nil "~S" M3) (format nil "~S" M4)))
	  ;(write-graphviz-dot-file M4 "~/run-tests-4.dot" :draw-reflexive nil)
	  )
	)
      )
    t
))

(defun run-tests-5 ()
  (let ((com (make-comgraph '(a b c))))
    (add-undirected-edge com 'a 'b)
    (add-undirected-edge com 'a 'c)
    (let* ((device-a (make-agent :name "a"))
	   (device-b (make-agent :name "b"))
	   (device-c (make-agent :name "c"))
	   (world-u (make-world :name "u" :propositions '(:EARTHQUAKE)))
	   (world-v (make-world :name "v" :propositions '()))
	   (rel-device-a
	    (list (make-relation :from world-u :to world-u)
		  (make-relation :from world-v :to world-v)))
	   (rel-device-b
	    (list (make-relation :from world-u :to world-u)
		  (make-relation :from world-v :to world-v)
		  (make-relation :from world-u :to world-v)
		  (make-relation :from world-v :to world-u)))
	   (rel-device-c
	    (list (make-relation :from world-u :to world-u)
		  (make-relation :from world-v :to world-v)
		  (make-relation :from world-u :to world-v)
		  (make-relation :from world-v :to world-u)))
	   (M1 (make-kripke-model :worlds (list world-u world-v)
				  :comgraph com
				  :real-worlds (list world-u)
				  :agents (list device-a device-b device-c)
				  :relations (pairlis 
					      (list device-a device-b device-c)
					      (list rel-device-a rel-device-b rel-device-c)))))
;      (format t "~S~%" M1)
      (assert (eq t (can-send-p M1 '(a :TRUE (a b c)))))
      (assert (eq nil (can-send-p M1 '(b :TRUE (c)))))
      (let* ((A-obs (make-observe-proposition-update M1 'a ':EARTHQUAKE-2))
	     (M2 (product-update M1 A-obs)))
	(format t "~S" M2))
       
      )
    )
  t
  )

(defun test-make-change-state-update (M a G s1 s2 phi)
  (let* ((e1 (make-world :name "e1" 
			 :propositions (list ':AND s1 (list ':KNOWS a phi))
			 :additions (list ':S1-to-S2)))
	 (e2 (make-world :name "e2"
			 :propositions (list ':AND s2 (list ':KNOWS a phi))
			 :additions (list ':S2-to-S1)))
	 (e3 (make-world :name "e3"
			 :propositions (list ':NOT (list ':KNOWS a phi))))
	 (ea (make-world :name "e-all" :propositions '(:TRUE)))
	 (rel-all (mapcar #'(lambda (agent) 
				 (cons agent 
				       (append
					 (list (make-relation :from e1 :to e1)
					       (make-relation :from e2 :to e2)
					       (make-relation :from e3 :to e3)
					       (make-relation :from ea :to ea)
					       (make-relation :from e1 :to e2)
					       (make-relation :from e1 :to e3)
					       (make-relation :from e2 :to e1)
					       (make-relation :from e2 :to e3)
					       (make-relation :from e3 :to e1)
					       (make-relation :from e3 :to e2))
					 (when (not (member agent G :test 'equal))
					   (list
					    (make-relation :from e1 :to ea)
					    (make-relation :from e2 :to ea)
					    (make-relation :from e3 :to ea)
					    (make-relation :from ea :to e1)
					    (make-relation :from ea :to e2)
					    (make-relation :from ea :to e3))))))
		       (kripke-model-agents M))))
    (make-kripke-model :worlds (list e1 e2 e3 ea)
		       :relations rel-all
		       :agents (kripke-model-agents M)
		       :real-worlds (list e1))))

(defun run-tests-6 ()
  (let ((com (make-comgraph '(a b c))))
    (add-undirected-edge com 'a 'b)
    (add-undirected-edge com 'a 'c)
    (let* ((device-a (make-agent :name "a"))
	   (device-b (make-agent :name "b"))
	   (device-c (make-agent :name "c"))
	   (world-u (make-world :name "u" :propositions '(:OPEN)))
	   (world-v (make-world :name "v" :propositions '(:CLOSED)))
	   (rel-device-a
	    (list (make-relation :from world-u :to world-u)
		  (make-relation :from world-v :to world-v)))
	   (rel-device-b
	    (list (make-relation :from world-u :to world-u)
		  (make-relation :from world-v :to world-v)
		  (make-relation :from world-u :to world-v)
		  (make-relation :from world-v :to world-u)))
	   (rel-device-c
	    (list (make-relation :from world-u :to world-u)
		  (make-relation :from world-v :to world-v)
		  (make-relation :from world-u :to world-v)
		  (make-relation :from world-v :to world-u)))
	   (M1 (make-kripke-model :worlds (list world-u world-v)
				  :comgraph com
				  :real-worlds (list world-u)
				  :agents (list device-a device-b device-c)
				  :relations (pairlis 
					      (list device-a device-b device-c)
					      (list rel-device-a rel-device-b rel-device-c)))))
      (format t "~S~%" M1)
      (let ((M2 (wsn-learn-prop M1 'b ':PHI)))
	(format t "~S~%" M2)
	(write-graphviz-dot-file M2 "~/m2.dot")
	(let ((M3 (wsn-message-update M2 '(b :PHI (a b)))))
	  (write-graphviz-dot-file M3 "~/m3.dot")
	  ;(setq MM M3)
	  ;(setq da device-a)
	  ;(setq db device-b)
	  (format t "~S~%" M3)
	  (let* ((test-update (test-make-change-state-update M3 
							     'a 
							     (list device-a device-b)
							     ':OPEN ':CLOSED ':PHI))
		 (M4 (product-update M3 test-update)))
	    (format t "~S~%" M4)
	    (write-graphviz-dot-file M4 "~/m4.dot")))))))


(defun run-tests ()
  (run-tests-1)
  (format t "tests 1 done~%")
  (run-tests-2)
  (format t "tests 2 done~%")
  (run-tests-3)
  (format t "tests 3 done~%")
  (run-tests-4)
  (format t "tests 4 done~%")
  (run-tests-5)
  (format t "tests 5 done~%")
  t
)


