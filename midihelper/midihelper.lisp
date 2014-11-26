(in-package :cl-alsaseq.util)
(defun inspect-midihelper ()
  (list '*midi-in-thread* *midi-in-thread*
        '*tick-thread* *tick-thread*
        '*tock-thread* *tock-thread*
        '*seq* *seq*))

;; (defun start-midihelper ()
;;   (start-reader)
;;   (start-writer)
;;   (set-master)
;;   (inspect-midihelper))

(defun check-midihelper ()
  (alexandria:doplist
      (key val (inspect-midihelper))
    (if (null val)
        (warn "Helper ~A not running" key))))
(defun stop-midihelper ()
  (check-midihelper)
  (if *midi-in-thread* (stop-reader))
  (if *seq* (stop-writer))
  (if *tick-thread* (stop-master-clock))
  (if *tock-thread* (stop-hires-clock))
  (inspect-midihelper))
