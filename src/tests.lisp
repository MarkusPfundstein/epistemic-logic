(defpackage #:tests
  (:use #:cl #:kripke #:del #:message))

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
	   (M2 (message-update M Am)))
      (format t "~S~%" M2)
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
	     (M3 (message-update M2 Ap)))
	(format t "~S~%" M3)
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
	)
      )
    t
))

(defun run-tests ()
  (run-tests-1)
  (format t "tests 1 done~%")
  (run-tests-2)
  (format t "tests 2 done~%")
  (run-tests-3)
  (format t "tests 3 done~%")
  (run-tests-4)
  (format t "tests 4 done~%")
  t
)


