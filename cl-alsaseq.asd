;;;; cl-alsaseq.asd

(asdf:defsystem cl-alsaseq
  :serial t
  :description "CL bindings to alsa midi sequencer"
  :author "Rick Venn <richard.venn@gmail.com>"
  :license "GPL"
  :depends-on (#:cffi #:calispel #:optima #:let-over-lambda)
  :components ((:file "package")
               (:file "bindings")
               (:file "event-lookup")
               (:file "cl-alsaseq")
               (:file "midi-reader")
               (:file "midi-writer")
               (:file "hires-clock")
               (:file "simple-looper")))
