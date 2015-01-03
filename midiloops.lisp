(in-package :midiloops)

(defconstant +n-loops+ 4)
(defconstant +default-loop-res+ 96)
(defconstant +default-loop-len+ 1500)

(defun null-trigger ()
  (lambda (event)
    (declare (ignore event))
    nil))

(defun new-m-loop ()
  (list
   :seq (make-array (* +default-loop-res+
                       +default-loop-len+)
                    :initial-element nil
                    :fill-pointer 0)
   :off 0;;loop start time clock microticks
   :play nil;; nil :push-extend :repeat
   :rec nil;; nil :overwrite or :overdub
   :trans #'identity;;this function is applied at read time
   :rec-tones nil;;list of active rec notes to prevent stuck notes
   :play-tones nil;;list of active play notes to prevent stuck notes
   :end-tag (make-array 96
                        :initial-element nil);;events which were played just before rec button was hit.  These will be glued to back of the array
   ))

(defvar *loop-stacks*
  (make-array 4
              :initial-contents
              (loop for i from 1 to 4
                 collect (let ((looplist
                                  `(,@(loop for i from 1 to +n-loops+
                                         collect (append `(:loop-id ,i)
                                                         (new-m-loop))))))
                           (make-array (length looplist)
                                       :initial-contents looplist
                                       :fill-pointer (length looplist))))))

(defvar *loop-stack* (aref *loop-stacks* 0))
(defvar *songpos* 0)

(defun pos (mloop)
"Return a loops read-head position (loop frame of reference), calculated from offset, play state and *songpos*. Also handles push-extend behaviour"
(or
 (match mloop
   ((plist :play play
           :off off
           ;;:rec rec
           :seq seq)
    (symbol-macrolet ((fill (fill-pointer seq)))
      (if (>= *songpos* off)
          (match play
            (:push-extend
             (let ((pos (- *songpos* off)))
               (if (>= *songpos* (+ off fill))
                   (setf fill (+ pos 1))
                   pos)))
            (:repeat
             (if (>= fill 1)
                 (nth-value 1 (floor (- *songpos* off)
                                     fill)))))))))
 0))

(defvar *sync* :beat)

(defun sync-intvl ()
  (case *sync*
    (:beat 96)
    (:free 1)
    (:loop1 (max (length (getf (aref *loop-stack* 0)
                               :seq))
                 1))))

(defun nearest-beat (&optional (songpos *songpos*))
  (* (sync-intvl)
     (round songpos
            (sync-intvl))))

(defun make-fixed-loop (bars &key (major 4) (minor 4) (res +default-loop-res+))
  (let ((newloop (new-m-loop)))
    (setf (getf newloop :seq)
          (make-array (/ (* bars major 4 res) minor)
                      :initial-element nil
                      :fill-pointer 0))
    newloop))

(defparameter *default-tick-ev*
  (list (ev-noteon 0 43 127)))

(defparameter *default-tock-ev*
  (list (ev-noteon 0 43 67)))

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

(defvar *metronome*
  (make-simple-metro 2)
  ;;(make-jazz-metro 2)
  )


(defun toggle-metronome ()
  (symbol-macrolet ((tog (getf *metronome* :play)))
    (setf tog (and (not tog)
                   :repeat))))

(defun loop-group (group-id)
  (setf *loop-stack*
        (aref *loop-stacks* (- group-id 1))))

(defun loop-overdub (mloop)
  (symbol-macrolet ((rec (getf mloop :rec)))
    (setf rec
          (match rec
            (:overdub
             nil)
             ((or :overwrite nil)
              :overdub))))
  (print mloop))

;; (defun loop-overwrite (mloop)
;;   (symbol-macrolet ((rec (getf mloop :rec)))
;;     (setf rec
;;           (match rec
;;             (:overwrite
;;              nil)
;;              ((or :overdub nil)
;;               :overwrite))))
;;   (print mloop))

(defun drain-hanging-play-tones (mloop)
  (symbol-macrolet ((tones (getf mloop :play-tones)))
    (mapcar #'send-event
            tones)
    (setf tones nil)))

(defun drain-hanging-rec-tones (mloop)
  (symbol-macrolet ((slot (aref (getf mloop :seq)
                                (pos mloop)))
                    (tones (getf mloop :rec-tones)))
    (setf slot
          (append tones slot))))

(defun loop-play (mloop)
  (setf (getf mloop :play) :repeat)
  (symbol-macrolet ((off (getf mloop :off)))
    (setf off (nearest-beat))
    (loop for i from off to (- *last-songpos* 1)
       do
         (mapcar #'send-event
                 (aref (getf mloop :seq)
                       (- i off)))))
  (print mloop))

(defun loop-continue (mloop)
  (setf (getf mloop :play) :repeat))

(defun loop-push-extend (mloop)
  (setf (getf mloop :play) :push-extend)
  (print mloop))

(defun loop-stop (mloop)
  (setf (getf mloop :play) nil)
  (setf (getf mloop :rec) nil)
  (drain-hanging-rec-tones mloop)
  (drain-hanging-play-tones mloop)
  (print mloop))

(defun loop-erase (mloop)
  (let ((seq (getf mloop :seq)))
    (setf (getf mloop :play) nil)
    (loop for i below (length seq)
       do (setf (aref seq i) nil))
    (setf (fill-pointer seq)
          0))
  (setf (getf mloop :rec-tones) nil)
  (print mloop))

(defun loop-cycle (mloop &optional (songpos *songpos*))
  (symbol-macrolet ((play (getf mloop :play))
                    (rec (getf mloop :rec)))
    (match mloop
      ((plist :play nil
              :seq (guard seq
                          (= 0 (fill-pointer seq))))
       (setf play :push-extend)
       (setf rec :overdub)
       (setf (getf mloop :off) (nearest-beat)))
      ((plist :play :push-extend
              :rec :overdub)
       (setf play :repeat)
       (setf (fill-pointer (getf mloop :seq))
             (nearest-beat (- songpos (getf mloop :off))))
       (setf rec nil)
       (drain-hanging-rec-tones mloop))
      ((plist :play :repeat)
       (loop-stop mloop))
      ((plist :play (or :push-extend
                        nil))
       (setf play :repeat))))
    (print mloop))

(defun note-pop (tones note channel)
  (if tones
      (match (car tones)
        ((plist :event-data
                (plist NOTE (eq note) CHANNEL (eq channel)))
         (note-pop (cdr tones) note channel))
        (otherwise (cons (car tones)
               (note-pop (cdr tones) note channel))))))

(defmacro hang-tones ()
  `(match event
     ((plist :event-type :SND_SEQ_EVENT_NOTEON)
      (let ((event-copy (copy-tree event)))
        (setf (getf event-copy :event-type)
              :SND_SEQ_EVENT_NOTEOFF)
        (push event-copy tones)))
     ((plist :event-type :SND_SEQ_EVENT_NOTEOFF
             :event-data (plist NOTE note CHANNEL channel))
      (setf tones
            (note-pop tones note channel)))))

(defun store-gesture (event mloop)
  (match mloop
    ((plist :play _
            :rec  (not nil))
     (symbol-macrolet ((tones (getf mloop :rec-tones))
                       (seq (getf mloop :seq)))
       (push (let ((event-copy (copy-tree event)))
               (if (>= (+ (getf mloop :off)
                          (pos mloop))
                       *songpos*)
                   (setf (getf event-copy :skip-1st)
                         t)
                   event-copy));;The preceding block ensures events recorded 'into the future' are marked to skip 1st play, e.g quantised recording or when the rec button is pressed slightly ahead of time
             (aref seq
                   (pos mloop)))
       (hang-tones);;caution - very 'unhygienic' macro!
       ))))

(defun read-gestures (mloop songpos)
  (match mloop
    ((plist :play (not nil)
            :off (guard off (>= songpos off)))
     (mapcar (lambda (event)
               (if (getf event :skip-1st)
                   (setf (getf event :skip-1st) nil)
                   (symbol-macrolet ((tones (getf mloop :play-tones)))
                     (hang-tones);;caution - very 'unhygienic' macro!
                     (send-event event))))
             (aref (funcall (getf mloop :trans)
                            (getf mloop :seq))
                   (pos mloop))))))

(defvar *last-songpos* 0)
(defun dispatch-event (event mloop)
  (macromatch event
    ((plist :event-type (guard type
                               (or (equal type :microtick)
                                   (equal type :snd_seq_event_clock)))
            :songpos songpos)
     (read-gestures mloop songpos))
    ((plist :event-type (guard type
                               (or (equal type :microtick)
                                   (equal type :snd_seq_event_clock))))
     (error "loop received clock data with no songpos"))
    ((plist :event-type :loop-ctrl
            :function function)
     (funcall function mloop))
    (if-gesture
      (store-gesture event mloop))))

(defvar *send-clock* nil)

(defvar *last-beat* (make-array 96
                                :initial-element nil))

(defun handle-tick (songpos event)
  (setf *last-songpos* *songpos*)
  (setf *songpos* songpos)
  (read-gestures *metronome* songpos)
  (setf (aref *last-beat*
              (nth-value 1 (floor *songpos* (sync-intvl))))
        nil)
  (match event
    ((plist :event-type :snd_seq_event_clock)
     (if *send-clock* (send-event event)))))

(defun handle-gesture (event)
  (push event (aref *last-beat* (- *songpos*
                                   (nearest-beat))))
  (send-event event))

(defun run-loop-stack ()
  (let ((event (pri-alt ((? *clock-ochan* ev) ev)
                        ((? *reader-ochan* ev) ev))))
    (macromatch event
      ((plist :event-type :toggle-metronome)
       (toggle-metronome))
      ((plist :event-type :global-ctrl
              :function function)
       (funcall function))
      ((plist :event-type (guard type
                                 (or (equal type :microtick)
                                     (equal type :snd_seq_event_clock)))
              :songpos songpos)
       (handle-tick songpos event));;send clock

      (if-gesture
        (handle-gesture event)));;echo gestures

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

(defun test-single-loop ()
  (let* ((myloop (aref *loop-stack* 0))
         (seq (getf myloop :seq)))
    (loop-erase myloop)
    (setf (fill-pointer seq) (* 96 4))
    (loop-overdub myloop)))

(defvar *midiloops-thread* nil)

(defun start-midiloops ()
  (start-midihelper :master
                    96 #'quneo-reader)
  (sleep 0.1)
  (setf *midiloops-thread* (bt:make-thread (lambda ()
                                             (loop
                                                (restart-case (run-loop-stack)
                                                  (carry-on-looping ()))))
                                           :name "loopstack")))

(defun stop-midiloops ()
  (ignore-errors (bt:destroy-thread *midiloops-thread*))
  (stop-midihelper))
