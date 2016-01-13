(defpackage #:tests
  (:use #:cl #:kripke))

(in-package #:tests)

(defparameter world-w (make-world :name "w" :propositions '(:p :q (implies :p :q))))
(defparameter world-v (make-world :name "v" :propositions '(:q)))

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
   :agents (list alice bob)
   :relations (pairlis (list alice bob) (list rel-a rel-b))))

(defun run-tests ()
  (assert (eq t (models M1 world-v '(:IMPLIES (:NOT :P) :Q))))
  (assert (eq t (models M1 world-v '(:OR :p :q))))
  (assert (eq t (models M1 world-v '(:AND (:NOT :p) (:AND :q (:IMPLIES :p :q))))))
  (assert (null (models M1 world-v '(:KNOWS alice :p))))
  (assert (eq t (models M1 world-v '(:KNOWS bob (:NOT :p)))))
  (assert (eq t (models M1 world-w '(:POSSIBLE alice (:KNOWS bob :p)))))
  (assert (null (models M1 world-v '(:KNOWS alice (:KNOWS bob :p)))))
  (assert (eq t (models M1 world-v '(:KNOWS alice (:OR (:KNOWS bob :p) (:KNOWS bob (:NOT :p)))))))
  t)
