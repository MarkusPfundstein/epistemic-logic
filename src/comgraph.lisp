(defpackage #:comgraph
  (:use #:cl)
  (:export #:make-comgraph
	   #:add-undirected-edge
	   #:add-directed-edge
	   #:neighbours))

(in-package #:comgraph)

(defstruct (comgraph 
	     (:constructor make-comgraph (agents)))
  (vertices agents)
  (adj-list (map 'list #'(lambda (agent) (cons agent (list (list agent)))) agents)))

(defun add-directed-edge (graph from to)
  (when (not (and (member from (comgraph-vertices graph))
		  (member to (comgraph-vertices graph))))
    (error ":from and :to must be member of comgraph vertices"))
  (when (not (member to (cadr (assoc from (comgraph-adj-list graph)))))
    (push to (cadr (assoc from (comgraph-adj-list graph)))))
  )

(defun add-undirected-edge (graph a b)
  (add-directed-edge graph a b)
  (add-directed-edge graph b a))

(defun neighbours (graph v)
  (cadr (assoc v (comgraph-adj-list graph))))


