(in-package :cl-alsaseq.util)

(defun make-nonblock-buf-channel (&optional (queue 10))
  (make-instance 'calispel:channel
                 :buffer (MAKE-INSTANCE
                          'jpl-queues:LOSSY-BOUNDED-FIFO-QUEUE
                          :CAPACITY queue)))

(defun drain-channel (chan)
  (let ((buffer (calispel::buffer chan)))
    (loop repeat (jpl-queues:capacity buffer)
       do (jpl-queues:dequeue buffer))))

(define-condition stop-thread (error)
  ())

(defvar *midi-in-chan* (make-nonblock-buf-channel))

(defmacro if-gesture (event &body body)
  `(match ,event
      ((plist :EVENT-TYPE (guard event-type (or (equal event-type :snd_seq_event_noteoff)
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
       ,@body)))

(defmacro if-clock (event &body body)
  `(match ,event
     ((plist :EVENT-TYPE (guard event-type (equal event-type
                                                  :snd_seq_event_clock)))
      ,@body)))

(defun midi-input (seq)
  (open-port "in" seq :input)
  (loop (let ((mess (recv seq)))
          (if-gesture mess
            (! *midi-in-chan* mess))
          (if-clock mess
            (! *slave-tick-chan* "tick")))))

(defvar *midi-in-thread* nil)

(defun start-reader ()
  (assert (null *midi-in-thread*))
  (setf *midi-in-thread*
        (bt:make-thread (lambda ()
                          (unwind-protect
                               (handler-case
                                   (with-seq (seq :name "CL")
                                     (midi-input seq))
                                 (stop-thread ()))
                            (setf *midi-in-thread* nil)))
                        :name "simple-midi-reader")))

(defun stop-reader ()
  (bt:interrupt-thread
   *midi-in-thread* (lambda ()
                      (error 'stop-thread))))
