;;;; package.lisp

(defpackage #:cl-alsaseq
  (:use #:cl #:cffi #:calispel #:optima #:optima.extra)
  (:import-from :let-over-lambda
                #:g!-symbol-p
                #:defmacro/g!
                #:o!-symbol-p
                #:o!-symbol-to-g!-symbol
                #:defmacro!)
  (:export :open-port
           :close-port
           :open-seq
           :close-seq
           :with-seq
           :send-ctrl
           :send-note
           ))


(defpackage #:cl-alsaseq.quick
  (:use #:cl #:cl-alsaseq)
  (:export :send-note-on
           :send-note-off
           :send-pgmchange
           :send-chanpress
           :send-pitchbend
           :send-control
           ))

(defpackage #:cl-alsaseq.util
  (:use #:cl #:cl-alsaseq #:optima #:optima.extra)
  (:export :*clock-chan*
           :*midi-in-chan*
           :set-master-bpm
           :set-master
           :set-slave
           :set-hires
           :set-lores
           :inspect-helper-thread
           :start-helper-threads
           :stop-helper-threads
           :check-helper-threads
           ))

(defpackage #:cl-midiloops
  (:use #:cl #:cl-alsaseq #:cl-alsaseq.util #:optima #:optima.extra #:let-over-lambda)
  (:import-from :let-over-lambda
                #:g!-symbol-p
                #:defmacro/g!
                #:o!-symbol-p
                #:o!-symbol-to-g!-symbol
                #:defmacro!)
  (:nicknames #:cl-ml))
