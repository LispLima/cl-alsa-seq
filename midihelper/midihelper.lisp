(in-package :midihelper)
(defun inspect-midihelper ()
  (list '*reader-thread* *reader-thread*
        '*clock-thread* *clock-thread*
        '*seq* *seq*))

(defun start-midihelper (&optional
                           (master-slave :master)
                           (ppqn 96))
  (assert (or (= ppqn 96)
              (= ppqn 24)))
  (assert (or (eq master-slave :master)
              (eq master-slave :slave)))
  (alexandria:doplist (key val (inspect-midihelper))
    (assert (null val)))
  (drain-channel *clock-ochan*)
  (drain-channel *clock-ctrl-chan*)
  (start-reader *clock-ctrl-chan*)
  (start-clock *clock-ctrl-chan* master-slave ppqn))

(defun check-midihelper ()
  (alexandria:doplist
      (key val (inspect-midihelper))
    (if (null val)
        (warn "Helper ~A not running" key))))

(defun stop-midihelper ()
  (check-midihelper)
  (if *reader-thread* (stop-reader))
  (if *seq* (stop-writer))
  (if *clock-thread* (stop-clock))
  (inspect-midihelper))
