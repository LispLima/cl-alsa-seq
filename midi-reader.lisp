(in-package :cl-alsaseq)

(defvar *midi-in-chan* (make-nonblock-buf-channel))

(defun midi-input ()
  (with-alsa (seq :name "CL")
    (open-port "in" seq :input)
    (loop (! *midi-in-chan* (recv seq)))))

(defvar *input-thread* nil)

(defun start-simple-midi-reader ()
  (assert (null *input-thread*))
  (setf *input-thread* (bt:make-thread #'midi-input
                                       :name "simple-midi-reader")))

(defun stop-simple-midi-reader ()
  (bt:destroy-thread *input-thread*)
  (setf *input-thread* nil))
