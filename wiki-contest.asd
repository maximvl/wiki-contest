;;;; wiki-contest.asd

(asdf:defsystem #:wiki-contest
  :description "Describe wiki-contest here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:drakma
               #:alexandria
               #:cl-containers
               #:cl-html-parse
               #:percent-encoding)
  :serial t
  :components ((:file "package")
               (:file "wiki-contest")))

