(in-package :cl-alsaseq)

(defvar *tick-channel*  (make-instance 'calispel:channel
                                       :buffer (MAKE-INSTANCE
                                                'jpl-queues:LOSSY-BOUNDED-FIFO-QUEUE
                                                :CAPACITY 10)))

(defvar *tock-channel*  (make-instance 'calispel:channel
                                       :buffer (MAKE-INSTANCE
                                                'jpl-queues:LOSSY-BOUNDED-FIFO-QUEUE
                                                :CAPACITY 10)))

(defun tick ()
  (? *tick-channel*)
  (get-internal-real-time))

(defun tocker ()
  "free-running clock multiplier"
  (let* ((this (tick))
        (next (tick))
        (intvl (/ (- next this) 4000)))
    (loop (! *tock-channel* "tock")
       (loop repeat 3
          do
            (sleep intvl)
            (! *tock-channel* "tack"))
       (setf this next)
       (setf next (tick))
       (setf intvl (/ (- next this) 4000)))))

(defun ticker (&optional (ticktime 1))
  "optional master clock"
  (lambda ()
    (loop
       (! *tick-channel* "tick")
       (sleep ticktime))))
