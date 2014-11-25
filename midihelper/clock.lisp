(in-package :cl-alsaseq.util)

(defvar *master-tick-chan* (make-nonblock-buf-channel))

(defvar *slave-tick-chan* (make-nonblock-buf-channel))

(defvar *tick-chan*  *master-tick-chan*)

(defvar *tick-echo-chan* (make-nonblock-buf-channel))

(defvar *tock-chan*  (make-nonblock-buf-channel))

(defvar *clock-chan* *tock-chan*)

(defun zap-channels ()
  (mapcar (lambda (sym) (unintern sym))
          '(*master-tick-chan* *slave-tick-chan* *tick-chan* *tick-echo-chan* *tock-chan* *clock-chan*)))

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

(defun bpm-test (&optional (ppqn 96) (clock-chan *clock-chan*))
  (drain-channel clock-chan);; clear buffer of stale ticks
  (? clock-chan)
  (let ((start-time)
        (end-time)
        (reps (* ppqn 5)))
    (setf start-time (get-internal-real-time))
    (loop repeat reps do
         (? clock-chan 0.1))
    (setf end-time (get-internal-real-time))
    (format t "counted ~A ticks in ~A ms. (~F bpm)~%~%"
            reps
            (- end-time start-time)
            (/ (* reps 60 1000)
               (* ppqn (- end-time start-time))))))
(defvar *songpos-ticks* 0)

(defun songpos ()
  "This is song position, defined in 1/4 beats"
  (match *clock-chan*
    ((equal *tick-echo-chan*)
     (floor *songpos-ticks* 6))
    ((equal  *tock-chan*)
     (floor *songpos-ticks* 24))))

(defun set-songpos (ticks)
  "Set song position, specified in 1/4 beats (same units as songpos pointer)"
  (match *clock-chan*
    ((equal *tick-echo-chan*)
     (setf *songpos-ticks* (* ticks 6)))
    ((equal  *tock-chan*)
     (setf *songpos-ticks* (* ticks 24)))))

(let ((running-total 0))
  (defun calculate-intvl (this next)
    (let ((intvl (/ (- next this) ;; 4000
                    4000
                    )))
      (if (> intvl (/ 1 8))
          (setf running-total 0)
          (setf running-total (+ (* intvl 0.05)
                                 (* running-total 0.095)))))))

(defun tocker (last-ticktime)
  "free-running clock multiplier"
  (let* ((event (? *tick-chan*))
         (next-ticktime (get-internal-real-time))
         (intvl (calculate-intvl last-ticktime next-ticktime)))
    (match event
      ((property :EVENT-TYPE :SND_SEQ_EVENT_CLOCK)
       (! *tock-chan* event)
       (! *tick-echo-chan* event)
       (loop repeat 3
          do (sleep intvl)
            (! *tock-chan* (ev-microtick)))
       (setf last-ticktime next-ticktime))
      ((property :EVENT-TYPE (or :SND_SEQ_EVENT_STOP
                                 :SND_SEQ_EVENT_START
                                 :SND_SEQ_EVENT_CONTINUE))
       (! *tock-chan* event))
      ((plist :EVENT-TYPE :SND_SEQ_EVENT_SONGPOS
              :EVENT-DATA (property VALUE songpos))
       (set-songpos songpos))
      (_ (warn "unknown event seen by timer thread ~A" event)))
    (tocker last-ticktime)))

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
