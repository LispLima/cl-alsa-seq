(in-package :cl-alsaseq)

(defun make-nonblock-buf-channel (&optional (queue 10))
  (make-instance 'calispel:channel
                 :buffer (MAKE-INSTANCE
                          'jpl-queues:LOSSY-BOUNDED-FIFO-QUEUE
                          :CAPACITY queue)))

(define-condition stop-thread (error)
  ())

(defvar *midi-in-chan* (make-nonblock-buf-channel))

(defun midi-input (seq)
  (open-port "in" seq :input)
  (loop (! *midi-in-chan* (recv seq))))

(defvar *midi-in-thread* nil)

(defun start-simple-midi-reader ()
  (assert (null *midi-in-thread*))
  (setf *midi-in-thread*
        (bt:make-thread (lambda ()
                          (unwind-protect
                               (handler-case
                                   (with-alsa (seq :name "CL")
                                     (midi-input seq))
                                 (stop-thread ()))
                            (setf *midi-in-thread* nil)))
                        :name "simple-midi-reader")))

(defun stop-simple-midi-reader ()
  (bt:interrupt-thread
   *midi-in-thread* (lambda ()
                      (error 'stop-thread))))
