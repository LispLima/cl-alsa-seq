(in-package :cl-alsaseq.util)

(defvar *seq* nil);;sequence struct
(defvar **seq nil);;pointer to sequence struct (for memory deallocation)
(defvar *my-ports* nil)

(defun start-writer ()
  (assert (null **seq))
  (assert (null *seq*))
  (assert (null *my-ports*))
  (setf **seq (open-seq "Main"))
  (setf *seq* (mem-ref **seq :pointer))
  (setf *my-ports*
        (loop for i from 1 to 1
           collect (open-port (format nil "port~A" i)
                              (mem-ref *seq* :pointer)))))

(defun stop-writer ()
  (assert **seq)
  (close-seq **seq)
  (setf *seq* nil)
  (setf **seq nil)
  (setf *my-ports* nil))

(defun send-event (description &optional (port (car *my-ports*)) (seq *seq*))
  (match description
    ((plist :EVENT-TYPE (guard event-type (or (equal event-type :snd_seq_event_noteoff)
                                              (equal event-type :snd_seq_event_noteon)))
            :EVENT-DATA
            (plist ;; DURATION duration OFF_VELOCITY off_velocity
             VELOCITY velocity NOTE note CHANNEL channel))
     (send-note velocity note channel event-type seq port))
    ((plist :EVENT-TYPE (guard event-type (or (equal event-type
                                                     :snd_seq_event_controller)
                                             (equal event-type
                                                    :snd_seq_event_pgmchange)
                                             (equal event-type
                                                    :snd_seq_event_chanpress)
                                             (equal event-type
                                                    :snd_seq_event_pitchbend)
                                             (equal event-type
                                                    :snd_seq_event_control14)
                                             (equal event-type
                                                    :snd_seq_event_nonregparam)
                                             (equal event-type
                                                    :snd_seq_event_regparam)))
            :EVENT-DATA (plist VALUE value PARAM param CHANNEL channel))
     (send-ctrl channel param value event-type seq port))
     (_ (format t "unknown event ~S~%" description))))
