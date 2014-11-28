(in-package :midihelper)
(defun inspect-midihelper ()
  (list '*reader-thread* *reader-thread*
        '*tick-thread* *tick-thread*
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
  (let ((clock-ichan (make-nonblock-buf-channel)))
    (start-reader clock-ichan)
    (start-ticker *clock-ochan* clock-ichan master-slave ppqn)))

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
