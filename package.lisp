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
           :send-queue-ctrl
           :send-ctrl
           :send-note
           :recv
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

(defpackage #:midihelper
  (:use #:cl #:cffi #:cl-alsaseq #:optima #:optima.extra #:calispel)
  (:export :*clock-chan*
           :*midi-in-chan*
           :set-master-bpm
           :set-hires
           :set-lores
           :bpm-test
           :inspect-midihelper
           :start-midihelper
           :stop-midihelper
           :check-midihelper
           :if-gesture
           :if-clock
           :macromatch
           :drain-channel
           :send-event
           :stop-reader :stop-writer
           :start-reader :start-writer
           ))

(defpackage #:cl-midiloops
  (:use #:cl #:cl-alsaseq #:midihelper #:optima #:optima.extra #:calispel)
  (:import-from :let-over-lambda
                #:g!-symbol-p
                #:defmacro/g!
                #:o!-symbol-p
                #:o!-symbol-to-g!-symbol
                #:defmacro!)
  (:nicknames #:cl-ml))
