(in-package :midihelper)
(defun inspect-midihelper ()
  (list '*reader-thread* *reader-thread*
        '*clock-thread* *clock-thread*
        '*seq* *seq*))

(defun start-midihelper (&optional
                           (master-slave :master)
                           (ppqn 96) (reader-map #'identity))
  (assert (or (= ppqn 96)
              (= ppqn 24)))
  (assert (or (eq master-slave :master)
              (eq master-slave :slave)))
  (alexandria:doplist (key val (inspect-midihelper))
    (assert (null val)))
  (drain-channel *clock-ochan*)
  (drain-channel *clock-ctrl-chan*)
  (start-reader *clock-ctrl-chan* reader-map)
  (start-clock *clock-ctrl-chan* master-slave ppqn)
  (start-writer-thread))

(defun check-midihelper ()
  (alexandria:doplist
      (key val (inspect-midihelper))
    (if (null val)
        (warn "Helper ~A not running" key))))

(defun stop-midihelper ()
  (check-midihelper)
  (handler-case
      (if *reader-thread*
          (stop-reader))
    (SB-THREAD:INTERRUPT-THREAD-ERROR ()
      (setf *reader-thread* nil)))

  (if *seq*
      (stop-writer-thread))
  (handler-case
      (if *clock-thread*
          (stop-clock))
    (SB-THREAD:INTERRUPT-THREAD-ERROR ()
      (setf *clock-thread* nil)))
  (inspect-midihelper))
