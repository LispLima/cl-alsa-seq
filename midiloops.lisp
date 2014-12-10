(in-package :midiloops)

(defconstant +n-loops+ 4)
(defconstant +default-loop-res+ 96)
(defconstant +default-loop-len+ 800)

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
   :play nil;; nil :push-extend :repeat
   :rec nil;; nil :overwrite or :overdub
   :res +default-loop-res+ ;; 96 or 24
   :trans #'identity
   ;; :ichan (make-nonblock-buf-channel)
   ;; :ochan (make-nonblock-buf-channel)
   ))

(defun make-fixed-loop (bars &key (major 4) (minor 4) (res +default-loop-res+))
  (let ((newloop (new-m-loop)))
    (setf (getf newloop :seq) (make-array (/ (* bars major 4 res) minor) :initial-element nil :fill-pointer 0))
    newloop))

(defparameter *default-tick-ev*
  (list (ev-noteon 0 46 127)))

(defparameter *default-tock-ev*
  (list (ev-noteon 0 46 67)))

(defun make-simple-metro (bars
                          &rest rest
                          &key (tick-ev *default-tick-ev*)
                            (tock-ev *default-tock-ev*)
                            (major 4) (minor 4) (res +default-loop-res+))
  (let* ((newloop (apply #'make-fixed-loop `(,bars ,@rest)))
         (seq (getf newloop :seq))
         (beatlen (* res (/ minor 4)))
         (barlen (* major beatlen))
         (looplen (* barlen bars)))
    (setf (fill-pointer seq) looplen)
    (setf (getf newloop :play) :repeat)
    (loop for i below looplen
       do (match (multiple-value-list (floor i beatlen))
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
         (seq (getf newloop :seq))
         (beatlen (* res (/ minor 4)))
         (barlen (* major beatlen))
         (looplen (* barlen bars)))
    (setf (fill-pointer seq) looplen)
    (setf (getf newloop :play) nil)
    (loop for i below looplen
       do (match (multiple-value-list (floor i beatlen))
            ((list beats 0)
             (match (multiple-value-list (floor beats major))
               ((list _ (guard barbeat (oddp barbeat)))
                (setf (aref seq i) tick-ev))))))
    newloop))

(defvar *loop-stack* (let ((looplist
                              `(,@(loop for i from 1 to +n-loops+
                                     collect (append `(:loop-id ,i)
                                                     (new-m-loop)))
                                  (:loop-id :metro ,@(make-simple-metro 2))
                                  ;; (:loop-id :jazz-metro ,@(make-jazz-metro 2))
                                  )))
                       (make-array (length looplist)
                                   :initial-contents looplist
                                   :fill-pointer (length looplist))))

(defun simple-metro ()
  (nth +n-loops+ *loop-stack*))

(defun jazz-metro ()
  (nth (+ 1 +n-loops+) *loop-stack*))

(let ((active-loop 1))

  (defun ev-active-loop (loop-id)
    (list :EVENT-TYPE :ACTIVE-LOOP
          :LOOP-ID loop-id))

  (defun active-loop (n)
    (setf active-loop n))

  (defun ev-loop-push-extend (&optional (loop-id active-loop))
    (list :EVENT-TYPE :LOOP-PUSH-EXTEND
          :LOOP-ID loop-id))

  (defun ev-loop-overdub (&optional (loop-id active-loop))
    (list :EVENT-TYPE :LOOP-OVERDUB
          :LOOP-ID loop-id))

  (defun ev-loop-overwrite (&optional (loop-id active-loop))
    (list :EVENT-TYPE :LOOP-OVERWRITE
          :LOOP-ID loop-id))

  (defun ev-loop-continue (&optional (loop-id active-loop))
    (list :EVENT-TYPE :LOOP-CONTINUE
          :LOOP-ID loop-id))

  (defun ev-loop-play (&optional (loop-id active-loop))
    (list :EVENT-TYPE :LOOP-PLAY
          :LOOP-ID loop-id))

  (defun ev-loop-stop (&optional (loop-id active-loop))
    (list :EVENT-TYPE :LOOP-STOP
          :LOOP-ID loop-id))

  (defun ev-loop-erase (&optional (loop-id active-loop))
    (list :EVENT-TYPE :LOOP-ERASE
          :LOOP-ID loop-id))

  (defun ev-loop-cycle (&optional (loop-id active-loop))
    "This is the typical create, define-endpoint, overdub cycle"
    (list :EVENT-TYPE :LOOP-CYCLE
          :LOOP-ID loop-id)))

(defun loop-overdub (mloop)
  (symbol-macrolet ((rec (getf mloop :rec)))
    (setf rec
          (match rec
            (:overdub
             nil)
             ((or :overwrite nil)
              :overdub))))
  (print mloop))

(defun loop-overwrite (mloop)
  (symbol-macrolet ((rec (getf mloop :rec)))
    (setf rec
          (match rec
            (:overwrite
             nil)
             ((or :overdub nil)
              :overwrite))))
  (print mloop))

(defun loop-norec (mloop)
  (setf (getf mloop :rec) nil))

(defun nearest-beat (songpos)
  (* 96 (round songpos 96)))
s
(defun loop-play (mloop songpos)
  (setf (getf mloop :play) :repeat)
  (symbol-macrolet ((off (getf mloop :off)))
    (setf off (nearest-beat songpos))
    (loop for i from off to (- songpos 1)
       do
         (mapcar #'send-event
                 (aref (getf mloop :seq)
                       (- i off)))))
  (print mloop))

(defun loop-continue (mloop)
  (setf (getf mloop :play) :repeat))

(defun loop-push-extend (mloop songpos)
  (match mloop
    ((plist :play nil)
     (setf (getf mloop :pos) (nearest-beat songpos))))
  (setf (getf mloop :play) :push-extend)
  (print mloop))

(defun loop-stop (mloop)
  (setf (getf mloop :play) nil)
  (setf (getf mloop :rec) nil)
  (print mloop))

(defun loop-erase (mloop)
  (let ((seq (getf mloop :seq)))
    (loop for i below (length seq)
       do (setf (aref seq i) nil))
    (setf (fill-pointer seq)
          0)))

(defun loop-cycle (mloop songpos)
  (symbol-macrolet ((play (getf mloop :play))
                    (rec (getf mloop :rec)))
    (match mloop
      ((plist :play nil
              :seq (guard seq
                          (= 0 (fill-pointer seq))))
       (setf play :push-extend)
       (setf rec :overdub)
       (setf (getf mloop :off) (nearest-beat songpos)))
      ((plist :play :push-extend
              :rec :overdub)
       (setf play :repeat)
       (setf (fill-pointer (getf mloop :seq))
             (nearest-beat (- songpos (getf mloop :off))))
       (setf rec nil))
      ((plist :play :repeat)
       (setf play nil)
       (setf rec nil))
      ((plist :play (or :push-extend
                        nil))
       (setf play :repeat))))
    (print mloop))

(defun store-gesture (event mloop)
  (match mloop
    ((plist :play _
            :rec  (not nil))
     (symbol-macrolet ((seq (getf mloop :seq)))
       (push event (aref seq
                         (getf mloop :pos)))))))

(defun seek-to (mloop songpos)
  (match mloop
    ((plist :play play
            :off off
            :rec rec
            :seq seq)
     (symbol-macrolet ((pos (getf mloop :pos))
                       (fill (fill-pointer seq)))
       (if (>= songpos off)
           (match play
             (:push-extend
              (setf pos (- songpos off))
              (if (>= songpos (+ off fill))
                  (setf fill (+ pos 1))))
             (:repeat
                 (if (< fill 1)
                     (return-from seek-to))
               (setf pos
                     (nth-value 1 (floor (- songpos off)
                                         fill)))))
           (setf pos 0))
       (match rec
         (:overwrite
          (setf (aref seq pos)
                nil)))))))

(defun read-gestures (mloop songpos)
  (match mloop
    ((plist :play (not nil)
            :off (guard off (>= songpos off)))
     (mapcar (lambda (ev)
               (send-event ev))
             (aref (funcall (getf mloop :trans)
                            (getf mloop :seq))
                   (getf mloop :pos))))))

(defvar *last-songpos* 0)
(defun dispatch-event (event mloop)
  (macromatch event
    ((plist :event-type (guard type
                               (or (equal type :microtick)
                                   (equal type :snd_seq_event_clock)))
            :songpos songpos)
     (seek-to mloop songpos)
     (read-gestures mloop songpos))
    ((plist :event-type (guard type
                               (or (equal type :microtick)
                                   (equal type :snd_seq_event_clock))))
     (error "loop received clock data with no songpos"))
    ((plist :event-type :active-loop
            :loop-id loop-id)
     (active-loop loop-id))
    ((plist :event-type :loop-push-extend)
     (loop-push-extend mloop *last-songpos*))
    ((plist :event-type :loop-overdub)
     (loop-overdub mloop))
    ((plist :event-type :loop-overwrite)
     (loop-overwrite mloop))
    ((plist :event-type :loop-continue)
     (loop-continue mloop))
    ((plist :event-type :loop-play)
     (loop-play mloop *last-songpos*))
    ;; ((plist :event-type :SND_SEQ_EVENT_START)
    ;;  (loop-play mloop 0))
    ((plist :event-type :loop-stop)
     (loop-stop mloop))
    ((plist :event-type :loop-erase)
     (loop-erase mloop))
    ((plist :event-type :loop-cycle)
     (loop-cycle mloop *last-songpos*))
    (if-gesture
      (store-gesture event mloop))))

(defvar *send-clock* nil)

(defun run-loop-stack ()
  ;; (drain-channel *clock-ochan*)
  (loop for event = (pri-alt ((? *clock-ochan* ev) ev)
                             ((? *reader-ochan* ev) ev))
     do
       (macromatch event
         ((plist :event-type (guard type
                                    (or (equal type :microtick)
                                        (equal type :snd_seq_event_clock)))
                 :songpos songpos)
          (setf *last-songpos* songpos))
         (if-gesture
           (send-event event));;echo gestures
         ((plist :event-type :snd_seq_event_clock)
          (if *send-clock* (send-event event))));;echo clock
       (loop for idx below (length *loop-stack*)
          do
            (match (list event (aref *loop-stack* idx))
              ((or (not (list (plist :loop-id _)
                              _));;dispatch events with no loop-id info
                   (list (plist :loop-id (guard ev-loop-id
                                                (or (equal ev-loop-id :all)
                                                    (equal ev-loop-id loop-id))))
                         (plist :loop-id loop-id)));;dispatch events where loop-ids match
               (dispatch-event event (aref *loop-stack* idx)))))))

(defmacro if-loop-ctrl (&body body)
  `((plist :EVENT-TYPE (guard event-type (or (equal event-type
                                                    :push-extend)
                                             (equal event-type
                                                    :loop-overdub)
                                             (equal event-type
                                                    :loop-overwrite)
                                             (equal event-type
                                                    :loop-play)
                                             (equal event-type
                                                    :loop-stop)
                                             (equal event-type
                                                    :loop-erase)
                                             (equal event-type
                                                    :loop-cycle))))
    ,@body))

(defun test-single-loop ()
  (let* ((myloop (aref *loop-stack* 0))
         (seq (getf myloop :seq)))
    (loop-erase myloop)
    (setf (fill-pointer seq) (* 96 4))
    (loop-overdub myloop)))

(defvar *midiloops-thread* nil)

(defun start-midiloops ()
  (start-midihelper)
  (sleep 1)
  (setf *midiloops-thread* (bt:make-thread #'run-loop-stack :name "loopstack")))

(defun stop-midiloops ()
  (ignore-errors (bt:destroy-thread *midiloops-thread*))
  (stop-midihelper))
