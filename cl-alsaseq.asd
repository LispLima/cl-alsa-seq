;;;; cl-alsaseq.asd

(asdf:defsystem cl-alsaseq
  :serial t
  :description "CL bindings to alsa midi sequencer"
  :author "Rick Venn <richard.venn@gmail.com>"
  :license "GPL"
  :depends-on (#:cffi)
  :components ((:file "package")
               (:file "bindings")
               (:file "cl-alsaseq")))

