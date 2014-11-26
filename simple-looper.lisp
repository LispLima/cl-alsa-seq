(in-package :cl-midiloops)

(defvar *ticks* 0)

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
   ;; :ichan (make-nonblock-buf-channel)
   ;; :ochan (make-nonblock-buf-channel)
   ))

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
  (macromatch event
    (if-gesture
      (loop-write event mloop))))

(defvar *my-mloop* (make-fixed-loop 2))

(defun clear-dumb-loop ()
  (setf *my-mloop* (make-fixed-loop 2)))

(defun run-dumb-loop (&key seq (mloop *my-mloop*))
  (check-midihelper)
  ;; (drain-channel *midi-in-chan*)
  (let ((port 0;; (open-port "foo" seq)
          ))
    (loop (pri-alt ((? *clock-chan* tick)
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

(defvar *loop-stack* (append (loop for i from 1 to +n-loops+
                                collect (cons (intern (format nil "LOOP~D" i) :keyword)
                                              (new-m-loop)))
                             (list :metro (make-jazz-metro 2))
                             (list :jazz-metro (make-simple-metro 2))))

(defun loop-push-extend (loop-id &key (push-extend t))
  (list :EVENT-TYPE :LOOP-EXTEND
        :LOOP-ID loop-id
        :PUSH-EXTEND push-extend))

(defun loop-overdub (loop-id &key (overdub t))
  (list :EVENT-TYPE :LOOP-OVERDUB
        :LOOP-ID loop-id
        :OVERDUB overdub))

(defun loop-play (loop-id)
  (list :EVENT-TYPE :LOOP-PLAY
        :LOOP-ID loop-id))

(defun loop-stop (loop-id)
  (list :EVENT-TYPE :LOOP-STOP
        :LOOP-ID loop-id))

(defun loop-erase (loop-id)
  (list :EVENT-TYPE :LOOP-ERASE
        :LOOP-ID loop-id))

(defun loop-cycle (loop-id)
  "This is the typical create, define-endpoint, overdub cycle"
  (list :EVENT-TYPE :LOOP-CYCLE
        :LOOP-ID loop-id))
