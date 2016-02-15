;;;; EpLogResearch.asd

(asdf:defsystem #:eplog
  :serial t
  :description "Implementation of paper: Evolution of Knowledge in WSN"
  :author "Markus Pfundstein <markus.pfundstein@student.uva.nl>"
  :license "MIT"
  :depends-on ("alexandria")
  :components ((:file "package")
	       (:file "src/kripke")
	       (:file "src/del")
	       (:file "src/message")
	       (:file "src/comgraph")
	       (:file "src/wsn")
	       (:file "src/tests")))

