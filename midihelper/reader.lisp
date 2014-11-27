(in-package :cl-alsaseq.util)

(defun make-nonblock-buf-channel (&optional (queue 10))
  (make-instance 'calispel:channel
                 :buffer (MAKE-INSTANCE
                          'jpl-queues:LOSSY-BOUNDED-FIFO-QUEUE
                          :CAPACITY queue)))

(defun drain-channel (chan)
  (loop (pri-alt ((? chan res)
                  (print res))
                 (otherwise
                  (return-from drain-channel)))))

(define-condition stop-thread (error)
  ())

(defvar *midi-in-chan* (make-nonblock-buf-channel))

(defmacro if-gesture (&body body)
  `((plist :EVENT-TYPE (guard event-type (or (equal event-type :snd_seq_event_noteoff)
                                             (equal event-type :snd_seq_event_noteon)
                                             (equal event-type
                                                    :snd_seq_event_controller)
                                             (equal event-type
                                                    :snd_seq_event_pgmchange)
                                             (equal event-type
                                                    :snd_seq_event_chanpress)
                                             (equal event-type
                                                    :snd_seq_event_pitchbend)
                                             (equal event-type
                                                    :snd_seq_event_control14)
                                             (equal event-type
                                                    :snd_seq_event_nonregparam)
                                             (equal event-type
                                                    :snd_seq_event_regparam))))
    ,@body))

(defmacro if-clock (&body body)
  `((plist :EVENT-TYPE (guard event-type (or (equal event-type
                                                    :snd_seq_event_clock)
                                             (equal event-type
                                                    :snd_seq_event_start)
                                             (equal event-type
                                                    :snd_seq_event_stop)
                                             (equal event-type
                                                    :snd_seq_event_continue)
                                             (equal event-type
                                                    :snd_seq_event_songpos))))
    ,@body))

(defmacro macromatch (arg &body clauses)
  `(match ,arg
     ,@(mapcar #'macroexpand
              clauses)))

(defun midi-input (seq tick-chan gesture-chan)
  (open-port "in" seq :input)
  (loop (let ((mess (recv seq)))
          (macromatch mess
            (if-gesture
              (! gesture-chan mess))
            (if-clock 
              (! tick-chan mess))))))     

(defvar *midi-in-thread* nil)

(defun start-reader (tick-chan)
  (assert (null *midi-in-thread*))
  (setf *midi-in-thread*
        (bt:make-thread (lambda ()
                          (sleep 1)
                          (unwind-protect
                               (handler-case
                                   (with-seq (seq :name "CL")
                                     (midi-input seq
                                                 tick-chan
                                                 *midi-in-chan*
                                                 ))
                                 (stop-thread ()))
                            (setf *midi-in-thread* nil)))
                        :name "simple-midi-reader")))

(defun stop-reader ()
  (bt:interrupt-thread
   *midi-in-thread* (lambda ()
                      (error 'stop-thread))))
