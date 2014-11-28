(in-package :cl-alsaseq.util)
(defun inspect-midihelper ()
  (list '*midi-in-thread* *midi-in-thread*
        '*tick-thread* *tick-thread*
        '*seq* *seq*))

(defun start-midihelper (&optional
                           (master-slave :master)
                           (ppqn 96))
  (assert (or (= ppqn 96)
              (= ppqn 24)))
  (alexandria:doplist (key val (inspect-midihelper))
    (assert (null val)))
  (drain-channel *master-tick-chan*)
  (drain-channel *slave-tick-chan*)
  (start-reader *slave-tick-chan*)
  (start-ticker *master-tick-chan* *slave-tick-chan* :slave ppqn))

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
