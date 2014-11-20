(in-package :cl-alsaseq)

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

(defvar *my-mloop* (make-array (* 24 8) :initial-element nil))

(defmacro mloop-ref (mloop pos)
  `(aref ,mloop (nth-value 1 (floor ,pos (length ,mloop)))))

(defun loop-read (ticks mloop)
  (mapcar (lambda (event)
            (print event)
            (send-event event))
          (mloop-ref mloop ticks)))

(defun loop-write (ticks event mloop)
  (setf (mloop-ref mloop ticks)
        (cons event (mloop-ref mloop ticks))))

(defun dumb-loop (event)
  (track-songpos event)
  (loop-read *ticks* *my-mloop*)
  (match event
    ((plist :event-type (or :snd_seq_event_noteon
                           :snd_seq_event_noteoff))
     (loop-write *ticks* event *my-mloop*))))
