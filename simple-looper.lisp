(in-package :cl-alsaseq)

(defun make-nonblock-buf-channel (&optional (queue 10))
  (make-instance 'calispel:channel
                 :buffer (MAKE-INSTANCE
                          'jpl-queues:LOSSY-BOUNDED-FIFO-QUEUE
                          :CAPACITY queue)))
(defvar *ticks* 0)

(defun track-songpos (event)
  (match event
    ((plist :event-type :snd_seq_event_clock)
     (progn
       (incf *ticks*)
       (multiple-value-bind (quarters rem)
           (floor *ticks* 24)
         (if (= 0 rem)
             (print quarters)))))
    ((plist :event-type :snd_seq_event_songpos
            :event-data (plist value 16ths)
            )
     (progn
       (format t "~%seek to beat ~A~%" (/ 16ths 4.0))
       (setf *ticks* (* 6 16ths))))))

(defmacro mloop-ref (mloop pos)
  `(aref ,mloop (nth-value 1 (floor ,pos (length ,mloop)))))

(defun loop-read (ticks mloop)
  (mloop-ref mloop ticks))

(defun loop-write (ticks event mloop)
  (setf (mloop-ref mloop ticks)
        (cons event (mloop-ref mloop ticks))))

(defun dumb-loop (event)
  (track-songpos event)
  (mapcar #'send-event
          (loop-read *ticks* *my-mloop*))
  (match event
    ((plist :event-type (or :snd_seq_event_noteon
                           :snd_seq_event_noteoff))
     (loop-write *ticks* event *my-mloop*))))

(defconstant +n-loops+ 4)
(defconstant +default-loop-res+ 96)
(defconstant +default-loop-len+ 8)

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

(defparameter *default-tick-ev*
  '(:event-type :noteon :note-number 69))

(defparameter *default-tock-ev*
  '(:event-type :noteon :note-number 70))
  
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


(defvar *loop-stack* (make-array +n-loops+
                                 :initial-contents (loop repeat +n-loops+
                                                     collect (new-m-loop))))
