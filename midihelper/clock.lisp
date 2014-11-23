(in-package :cl-alsaseq.util)

(defvar *master-tick-chan* (make-nonblock-buf-channel))

(defvar *slave-tick-chan* (make-nonblock-buf-channel))

(defvar *tick-chan*  *master-tick-chan*)

(defvar *tick-echo-chan* (make-nonblock-buf-channel))

(defvar *tock-chan*  (make-nonblock-buf-channel))

(defvar *clock-chan* *tock-chan*)

(defun tick ()
  (! *tick-echo-chan* (? *tick-chan*))
  (get-internal-real-time))

(defvar *tick-time* 0.05)

(defun set-master-bpm (bpm)
  (setf *tick-time* (/ (/ 60 24) bpm)))

(defun ticker ()
  "optional master clock"
  (loop
     (! *master-tick-chan* "tick")
     (sleep *tick-time*)))

(defvar *tick-thread* nil)

(defun start-master-clock ()
  (assert (null *tick-thread*))
  (setf *tick-thread* (bt:make-thread #'ticker
                                      :name "master clock")))
(defun stop-master-clock ()
  (bt:destroy-thread *tick-thread*)
  (setf *tick-thread* nil))

(defun bpm-test (&optional (ppqn 24) (clock-chan *tick-chan*))
  (loop repeat 20 do
         (? clock-chan 0.1));; clear buffer of stale ticks
  (let ((start-time (get-internal-real-time))
        (reps (* ppqn 5)))
    (loop repeat reps do
         (? clock-chan 0.1))
    (format t "counted ~A ticks in ~A ms. (~F bpm)~%~%"
            reps
            (- (get-internal-real-time) start-time)
            (/ (* reps 60 1000)
               (* ppqn (- (get-internal-real-time) start-time))))))

(defun tocker ()
  "free-running clock multiplier"
  (let* ((this (tick))
        (next (tick))
        (intvl (/ (- next this) 4000)))
    (loop (! *tock-chan* "tock")
       (loop repeat 3
          do
            (sleep intvl)
            (! *tock-chan* "tack"))
       (setf this next)
       (setf next (tick))
       (setf intvl (/ (- next this) 4000)))))

(defvar *tock-thread* nil)

(defun start-hires-clock ()
  (assert (null *tock-thread*))
  (setf *tock-thread* (bt:make-thread #'tocker :name "96ppqn clock")))

(defun stop-hires-clock ()
  (bt:destroy-thread *tock-thread*)
  (setf *tock-thread* nil))

(defun set-master ()
  (drain-channel *master-tick-chan*)
  (setf *tick-chan* *master-tick-chan*))

(defun set-slave ()
  (drain-channel *slave-tick-chan*)
  (setf *tick-chan* *slave-tick-chan*))

(defun set-hires ()
  (setf *clock-chan* *tock-chan*))

(defun set-lores ()
  (setf *clock-chan* *tick-echo-chan*))
