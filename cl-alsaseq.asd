(defsystem cl-alsaseq
  :serial t
  :description "CL bindings to alsa midi sequencer"
  :author "Rick Venn <richard.venn@gmail.com>"
  :license "GPL"
  :depends-on ("cffi"
               "calispel"
               "optima"
               "let-over-lambda")
  :serial t
  :components ((:file "package")
               (:module "bindings"
                :components ((:file "seq-event")
                             (:file "seq")))
               (:module "driver"
                :components ((:file "bindings")
                             (:file "event-lookup")
                             (:file "cl-alsaseq")))
               (:module "midihelper"
                :components ((:file "reader")
                             (:file "writer")
                             (:file "clock")
                             (:file "midihelper")))
               (:file "easy-api")
               (:file "readtable")
               (:file "midiloops")))
