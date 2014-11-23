(in-package :cl-alsaseq.util)
(defun inspect-midihelper ()
  (list '*midi-in-thread* *midi-in-thread*
        '*tick-thread* *tick-thread*
        '*tock-thread* *tock-thread*
        '*seq* *seq*))

(defun start-midihelper ()
  (start-reader)
  (start-master-clock)
  (start-hires-clock)
  (inspect-midihelper))

(defun check-midihelper ()
  (alexandria:doplist
      (key val (inspect-midihelper))
    (if (null val)
        (warn "Helper ~A not running" key))))

(defun stop-midihelper ()
  (check-helper-threads)
  (stop-reader)
  (stop-writer)
  (stop-master-clock)
  (stop-hires-clock)
  (inspect-midihelper))
