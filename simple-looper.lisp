(in-package :cl-alsaseq)

(defvar *ticks* 0)

(defconstant +n-loops+ 4)
(defconstant +default-loop-res+ 96)
(defconstant +default-loop-len+ 8)

(defun inspect-helper-threads ()
  (list '*midi-in-thread* *midi-in-thread*
        '*tick-thread* *tick-thread*
        '*tock-thread* *tock-thread*))

(defun start-helper-threads ()
  (start-simple-midi-reader)
  (start-master-clock)
  (start-hires-clock)
  (inspect-helper-threads))

(defun check-helper-threads ()
  (alexandria:doplist
      (key val (inspect-helper-threads))
    (if (null val)
        (warn "Helper thread ~A not running" key))))

(defun stop-helper-threads ()
  (check-helper-threads)
  (stop-simple-midi-reader)
  (stop-master-clock)
  (stop-hires-clock)
  (inspect-helper-threads))

(defun null-trigger ()
  (lambda (event)
    (declare (ignore event))
    nil))

(defun new-m-loop ()
  (list
   :seq (make-array (* +default-loop-res+
                       +default-loop-len+) :initial-element nil :fill-pointer 0)
   :pos 0;;current position on loop tape
   :run nil;;flag for whether to run loop tape
   :play nil;;flag for whether to play events on loop tape
   :rec nil;;flag for whether to record incoming events
   :res +default-loop-res+
   :ichan (make-nonblock-buf-channel)
   :ochan (make-nonblock-buf-channel)))

(defun make-fixed-loop (bars &key (major 4) (minor 4) (res +default-loop-res+))
  (let ((newloop (new-m-loop)))
    (setf (getf newloop :seq) (make-array (/ (* bars major 4 res) minor) :initial-element nil))
    newloop))

(defun track-songpos (event loop)
  (symbol-macrolet ((pos (getf loop :pos)))
    (match event
      ((or (plist :event-type :snd_seq_event_clock)
           "tack" "tock")
       (progn
         (incf pos)
         (multiple-value-bind (quarters rem)
             (floor pos 96)
           (if (= 0 rem)
               (print quarters)))))
      ((plist :event-type :snd_seq_event_songpos
              :event-data (plist value 16ths))
       (progn
         (format t "~%seek to beat ~A~%" (/ 16ths 4.0))
         (setf pos (* 24 16ths)))))))

;; (defmacro! mloop-ref (mloop pos)
;;   `(aref (getf ,mloop :seq)
;;          (nth-value 1 (floor ,pos
;;                              (length (getf ,mloop :seq))))))

(defun mloop-idx (mloop)
  (nth-value 1 (floor (getf mloop :pos)
                      (length (getf mloop :seq)))))

(defun loop-read (mloop)
  (aref (getf mloop :seq)
        (mloop-idx mloop)))

(defun loop-write (event mloop)
  (let* ((ticks (mloop-idx mloop))
         (seq (getf mloop :seq)))
    (setf (aref seq ticks)
          (cons event (aref seq ticks)))))

(defun loop-write-gesture (event mloop)
  (if-gesture event
    (loop-write event mloop)))

(defvar *my-mloop* (make-fixed-loop 2))

(defun clear-dumb-loop ()
  (setf *my-mloop* (make-fixed-loop 2)))

(defun run-dumb-loop (&key seq (mloop *my-mloop*))
  (check-helper-threads)
  ;; (drain-channel *midi-in-chan*)
  (let ((port 0;; (open-port "foo" seq)
          ))
    (loop (pri-alt ((? *tock-chan* tick)
                    (mapcar (lambda (event)
                              (send-event event seq port))
                            (loop-read mloop))
                    (track-songpos tick mloop))
                   ((? *midi-in-chan* event) (loop-write-gesture
                                                event
                                                mloop))))))

(defparameter *default-tick-ev*
  '((:event-type :snd_seq_event_noteon
     :EVENT-DATA (VELOCITY 127 NOTE 46 CHANNEL 0))))

(defparameter *default-tock-ev*
  '((:event-type :snd_seq_event_noteon
     :EVENT-DATA (VELOCITY 80 NOTE 46 CHANNEL 0))))

(defun make-simple-metro (bars
                          &rest rest
                          &key (tick-ev *default-tick-ev*)
                            (tock-ev *default-tock-ev*)
                            (major 4) (minor 4) (res +default-loop-res+))
  (let* ((newloop (apply #'make-fixed-loop `(,bars ,@rest)))
         (seq (getf newloop :seq)))
    (loop for i below (length seq)
       do (match (multiple-value-list (floor i (* res (/ minor 4))))
            ((list beats 0)
             (match (multiple-value-list (floor beats major))
               ((list _ 0)
                (setf (aref seq i) tick-ev))
              ((list _ _)
               (setf (aref seq i) tock-ev))))))
    newloop))

(defun make-jazz-metro (bars
                        &rest rest
                        &key (tick-ev *default-tick-ev*)
                          (major 4) (minor 4) (res +default-loop-res+))
  (let* ((newloop (apply #'make-fixed-loop `(,bars ,@rest)))
         (seq (getf newloop :seq)))
    (loop for i below (length seq)
       do (match (multiple-value-list (floor i (* res (/ minor 4))))
            ((list beats 0)
             (match (multiple-value-list (floor beats major))
               ((list _ (guard barbeat (oddp barbeat)))
                (setf (aref seq i) tick-ev))))))
    newloop))

(defvar *loop-stack* (list :loops (loop repeat +n-loops+
                                     collect (new-m-loop))
                           :metro (make-jazz-metro 2)
                           :jazz-metro (make-simple-metro 2)))
