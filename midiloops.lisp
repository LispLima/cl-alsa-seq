(in-package :midiloops)

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
   :pos 0;;loop tape head
   :off 0;;loop start time clock microticks
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
    (loop (pri-alt ((? *clock-ochan* tick)
                    (mapcar (lambda (event)
                              (send-event event seq port))
                            (loop-read mloop))
                    (track-songpos tick mloop))
                   ((? *reader-ochan* event) (loop-write-gesture
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

(defun loop-push-extend (loop-id)
  (list :EVENT-TYPE :LOOP-EXTEND
        :LOOP-ID loop-id))

(defun loop-overdub (loop-id)
  (list :EVENT-TYPE :LOOP-OVERDUB
        :LOOP-ID loop-id))

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

(defvar *loop-stack* (append (loop for i from 1 to +n-loops+
                                append (list i
                                             (new-m-loop)))
                             (list :metro (make-simple-metro 2))
                             (list :jazz-metro (make-jazz-metro 2))))

(defmacro if-loop-ctrl (&body body)
  `((plist :EVENT-TYPE (guard event-type (or (equal event-type
                                                    :push-extend)
                                             (equal event-type
                                                    :loop-overdub)
                                             (equal event-type
                                                    :loop-play)
                                             (equal event-type
                                                    :loop-stop)
                                             (equal event-type
                                                    :loop-erase)
                                             (equal event-type
                                                    :loop-cycle))))
    ,@body))

(defun seek-to (mloop songpos)
  (error "under construction"))

(defun dispatch-event (event mloop)
  (macromatch event
    ((plist :event-type (or :microtick :snd_seq_event_clock)
            :songpos songpos)
     (seek-to mloop songpos))
    ((plist :event-type :loop-push-extend)
     (setf (getf mloop :rec) :push-extend))
    ((plist :event-type :loop-overdub)
     (setf (getf mloop :rec) :overdub))
    ((plist :event-type :loop-overwrite)
     (setf (getf mloop :rec) :overwrite))
    ((plist :event-type :loop-play)
     (setf (getf mloop :play) t))
    ((plist :event-type :loop-stop)
     (setf (getf mloop :play) nil)
     (setf (getf mloop :rec) nil))
    ((plist :event-type :loop-erase)
     (setf (getf mloop :play) nil)
     (let ((seq (getf mloop :seq)))
       (loop for i below (length seq)
          do (setf (aref seq i) nil))
       (setf (fill-pointer seq)
             0)))
    ((property :event-type
               :loop-cycle)
     (warn "loop-cycle not yet implemented"))
    (if-gesture
      (match mloop
        ((plist :rec (not nil))
         (let ((seq (getf mloop :seq)))
           (push event (aref seq
                             (getf mloop :pos)))))))))

(defun run-loop-stack ()
  (loop for event = (pri-alt ((? *clock-ochan*))
                             ((? *reader-ochan*)))
     do
       (loop for mloop in *loop-stack*
          do
            (match (list event mloop)
              ((or (not (list (plist :loop-id _)
                              _));;dispatch events with no loop-id info
                   (list (plist :loop-id (guard ev-loop-id
                                                (or (equal ev-loop-id :all)
                                                    (equal ev-loop-id loop-id))))
                         (plist :loop-id loop-id)));;dispatch events where loop-ids match
               (dispatch-event event mloop))))))
