;;;; cl-alsaseq.asd

(asdf:defsystem #:cl-alsaseq
  :serial t
  :description "Describe cl-alsaseq here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :components ((:file "package")
               (:file "cl-alsaseq")
               (:file "bindings"))
  :depends-on (#:cffi))

