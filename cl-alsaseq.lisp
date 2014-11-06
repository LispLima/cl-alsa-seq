;;;; cl-alsaseq.lisp

(in-package #:cl-alsaseq)

;;; "cl-alsaseq" goes here. Hacks and glory await!
(load-foreign-library "libasound.so")

(defparameter seq (foreign-alloc :pointer))

(defun init-seq ()
  (snd_seq_open seq "default" SND_SEQ_OPEN_DUPLEX 0)
  (snd_seq_set_client_name (mem-ref seq :pointer) "Common Lisp"))

(defun create-port (name)
  (snd_seq_create_simple_port (mem-ref seq :pointer) name
                              (logior SND_SEQ_PORT_CAP_WRITE 
                                      SND_SEQ_PORT_CAP_SUBS_WRITE
                                      SND_SEQ_PORT_CAP_READ
                                      SND_SEQ_PORT_CAP_SUBS_READ)
                              (logior SND_SEQ_PORT_TYPE_MIDI_GENERIC 
                                      SND_SEQ_PORT_TYPE_APPLICATION)))


(defun bork-everything ()
  (let ((event  (foreign-alloc :pointer)))
    (values (snd_seq_event_input (mem-ref seq :pointer) (mem-ref event :pointer))
            event)))
