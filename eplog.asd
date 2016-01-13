;;;; EpLogResearch.asd

(asdf:defsystem #:eplog
  :serial t
  :description "Implementation of paper: Evolution of Knowledge in WSN"
  :author "Markus Pfundstein <markus.pfundstein@student.uva.nl>"
  :license "MIT"
  :components ((:file "package")
	       (:file "src/kripke")
	       (:file "src/tests")))

