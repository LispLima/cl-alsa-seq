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

(defparameter event (foreign-alloc :pointer))
;;This function missing some header stuff for the polling lib
(defun midi-poll (my-port)
  (declare (ignore my-port))
  (let* ((npfds (snd_seq_poll_descriptors_count (mem-ref seq :pointer) POLLIN))
         (pfds (foreign-alloc '(:struct pollfd) :count npfds)))
    (snd_seq_poll_descriptors (mem-ref seq :pointer) pfds npfds POLLIN)
    (if (> (print (poll pfds npfds 100000)) 0)
        (progn
          (values (snd_seq_event_input (mem-ref seq :pointer)
                                       event)
                  (mem-ref (mem-ref event :pointer) '(:struct pollfd)))))))
