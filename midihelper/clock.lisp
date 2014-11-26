(in-package :cl-alsaseq.util)


(defvar *master-tick-chan* (make-nonblock-buf-channel))

(defvar *slave-tick-chan* (make-nonblock-buf-channel))

(defvar *tick-echo-chan* (make-nonblock-buf-channel))

(defvar *tock-chan*  (make-nonblock-buf-channel))

(defvar *clock-chan* *tock-chan*)

(defun zap-channels ()
  (mapcar (lambda (sym) (unintern sym))
          '(*master-tick-chan* *slave-tick-chan* *tick-echo-chan* *tock-chan* *clock-chan*)))

(defvar *tick-time* 0.05)

(defun set-master-bpm (bpm)
  (setf *tick-time* (/ (/ 60 24) bpm)))

(defun ticker (tick-chan)
  "optional master clock"
  (loop
     (! tick-chan (ev-tick))
     (sleep *tick-time*)))

(defvar *tick-thread* nil)

(defun bpm-test (&optional (ppqn 96) (clock-chan *clock-chan*))
  ;; (drain-channel clock-chan)
  ;; clear buffer of stale ticks
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

(defun calculate-intvl (this next)
  (assert (>= next this))
  (let ((intvl (/ (- next this) ;; 4000
                  4000
                  )))
    (if (> intvl (/ 1 8))
        0.05
        intvl)))

(defun tocker (last-ticktime tick-chan)
  "free-running clock multiplier"
  (if (= 0 (nth-value 1 (songpos)))
      (print (songpos)))
  (let* ((event (? tick-chan))
         (next-ticktime (get-internal-real-time))
         (intvl (calculate-intvl last-ticktime next-ticktime)))
    (match event
      ((property :EVENT-TYPE :SND_SEQ_EVENT_CLOCK)
       (! *tock-chan* event)
       (! *tick-echo-chan* event)
       (incf *songpos-ticks*)
       (loop repeat 3
          do
            (sleep intvl)
            (incf *songpos-ticks*)
            (! *tock-chan* (ev-microtick)))
       (setf last-ticktime next-ticktime))
      ((property :EVENT-TYPE (or :SND_SEQ_EVENT_STOP
                                 :SND_SEQ_EVENT_START
                                 :SND_SEQ_EVENT_CONTINUE))
       (! *tock-chan* event))
      ((plist :EVENT-TYPE :SND_SEQ_EVENT_SONGPOS
              :EVENT-DATA (property VALUE songpos))
       (set-songpos songpos))
      (_ (error "unknown event seen by timer thread ~A" event)
         (sleep 1)))
    (tocker last-ticktime tick-chan)))

(defvar *tock-thread* nil)

(defun start-hires-clock (tick-chan)
  (assert (null *tock-thread*))
  (setf *tock-thread*
        (bt:make-thread
         (lambda ()
           (sleep 1)
           (unwind-protect
                (handler-case
                    (tocker (get-internal-real-time) tick-chan)
                  (stop-thread ()))
             (setf *tock-thread* nil)))
         :name "96ppqn clock")))

(defun stop-hires-clock ()
  (bt:interrupt-thread
   *tock-thread* (lambda ()
                   (error 'stop-thread))))

(defun start-master-clock (tick-chan)
  (assert (null *tick-thread*))
  (setf *tick-thread* (bt:make-thread (lambda ()
                                        (ticker tick-chan))
                                      :name "master clock")))

(defun stop-master-clock ()
  (bt:destroy-thread *tick-thread*)
  (setf *tick-thread* nil))

(defun start-with-master-clock ()
  (alexandria:doplist (key val (inspect-midihelper))
    (assert (null val)))
  (setf *master-tick-chan* (make-nonblock-buf-channel))
  (start-hires-clock *master-tick-chan*)
  (start-master-clock *master-tick-chan*))

(defun start-with-slave-clock ()
  (alexandria:doplist (key val (inspect-midihelper))
    (assert (null val)))
  (setf *slave-tick-chan* (make-nonblock-buf-channel))
  (start-hires-clock *slave-tick-chan*)
  (start-reader *slave-tick-chan*))

(defun set-hires ()
  (setf *clock-chan* *tock-chan*))

(defun set-lores ()
  (setf *clock-chan* *tick-echo-chan*))



