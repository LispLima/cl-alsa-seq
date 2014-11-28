(in-package :midihelper)

(defvar *clock-ctrl-chan* (make-nonblock-buf-channel))

(defvar *clock-ochan*  (make-nonblock-buf-channel))

(defvar *tick-time* 0.05)

(defun set-master-bpm (bpm)
  (setf *tick-time* (/ (/ 60 24) bpm)))

(defun hires-tick (tick-chan microtick-intvl)
  (! tick-chan (ev-tick));;FIXME - actually calculate microtick interval
  (sleep microtick-intvl)
  (loop repeat 3
     do
       (! tick-chan (ev-tick))
       (sleep microtick-intvl)))

(defun hires-semiquaver (tick-chan tick-intvl)
  (loop repeat 4
     do (hires-tick tick-chan (/ tick-intvl 4))))

(defun lores-semiquaver (tick-chan tick-intvl)
  (loop repeat 6
     do (! tick-chan (ev-tick))
       (sleep tick-intvl)))

(let ((songpos 0)
      (ticker-state :stopped)
      (last (get-internal-real-time))
      (next (get-internal-real-time)))

  (defun measure-tick-time ()
    (assert (>= next last))
    (let ((intvl (/ (- next last) 1000 )))
      (setf last next)
      (setf next (get-internal-real-time))
      (if (> intvl 1/8);; 1/8 second => 20bpm - lower limit
          0.005 ;;0.005 ms per tick => 500bpm - upper limit
          intvl)))

  (defun set-songpos (ticks)
  "Set song position, specified in 1/4 beats (same units as songpos pointer)"
  (setf songpos (* ticks 24)))

  (defun get-songpos ()
    "Get song position, in semiquavers"
    (/ songpos 24))


  (defun stopped-handler (tick-chan ctrl-chan)
    (match (? ctrl-chan)
      ((property :event-type :snd_seq_event_continue)
       (! tick-chan (ev-songpos songpos))
       (! tick-chan (ev-continue))
       (setf ticker-state :running))
      ((property :event-type :snd_seq_event_start)
       (print "ping")
       (setf songpos 0)
       (! tick-chan (ev-songpos songpos))
       (! tick-chan (ev-start))
       (setf ticker-state :running))
      ((property :EVENT-TYPE :SND_SEQ_EVENT_CLOCK)
       (warn "received clock signal before control signal whilst in stopped state, ignoring"))))

  (defun ticker (tick-chan ctrl-chan master-slave ppqn)
    "optional master clock"
    (multiple-value-bind (semiquavers rem)
        (floor songpos (/ ppqn 4))
      (assert (= 0 rem))
      (print master-slave)
      (print ppqn)
      (print semiquavers))
    (match (list ticker-state master-slave)
      ((list :stopped _)
        (stopped-handler tick-chan ctrl-chan))
      ((list :running :master)
       (pri-alt ((? ctrl-chan ctrl)
                 (match ctrl
                   ((property :EVENT-TYPE :SND_SEQ_EVENT_STOP)
                    (setf ticker-state :stopped))))
                (otherwise
                 (match ppqn
                   (24 (lores-semiquaver tick-chan *tick-time*)
                       (incf songpos 24))
                   (96 (hires-semiquaver tick-chan *tick-time*)
                       (incf songpos 24))))))
      ((list :running :slave)
       (match (? ctrl-chan)
         ((property :EVENT-TYPE :SND_SEQ_EVENT_STOP)
          (setf ticker-state :stopped))
         ((property :EVENT-TYPE :SND_SEQ_EVENT_CLOCK)
          (match ppqn
            (24 (! tick-chan (ev-tick)))
            (96 (hires-tick tick-chan (measure-tick-time))))))))))

(defvar *clock-thread* nil)

(defun bpm-test (&optional (ppqn 96) (clock-chan *clock-ochan*))
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

(defun start-clock (ctrl-chan master-slave ppqn)
  (assert (null *clock-thread*))
  (setf *clock-thread*
        (bt:make-thread
         (lambda ()
           (loop (ticker *clock-ochan* ctrl-chan master-slave ppqn)))
         :name "midihelper clock")))

(defun stop-clock ()
  (bt:destroy-thread *clock-thread*)
  (setf *clock-thread* nil))
