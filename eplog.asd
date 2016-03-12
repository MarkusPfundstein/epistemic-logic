;;;; EpLogResearch.asd

(asdf:defsystem #:eplog
  :serial t
  :description "Implementation of paper: Evolution of Knowledge in WSN"
  :author "Markus Pfundstein <markus.pfundstein@student.uva.nl>"
  :license "MIT"
  :depends-on ("iterate" "alexandria")
  :components ((:file "package")
	       (:file "src/func")
	       (:file "src/kripke")
	       (:file "src/del")
	       (:file "src/message")
	       (:file "src/comgraph")
	       (:file "src/wsn")
	       (:file "src/drawing")
	       (:file "src/sim")
	       (:file "src/tests")))

