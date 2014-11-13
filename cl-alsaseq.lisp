(in-package #:cl-alsaseq)

(load-foreign-library "libasound.so")

(defvar **seq nil)

(defun init-seq (client-name)
  (assert (null **seq))
  (setf **seq (foreign-alloc :pointer))
  (snd_seq_open **seq "default" SND_SEQ_OPEN_DUPLEX 0)
  ;;or alternatively can experiment with
  ;; (snd_seq_open **seq "default" SND_SEQ_OPEN_DUPLEX SND_SEQ_NONBLOCK)
  (snd_seq_set_client_name (mem-ref **seq :pointer) client-name))

(defun deinit-seq ()
  (snd_seq_close (mem-ref **seq :pointer))
  (foreign-free **seq)
  (setf **seq nil))

(defvar *my-port* nil)
(defun create-port (name)
  (assert (null *my-port*))
  (setf *my-port*
        (snd_seq_create_simple_port (mem-ref **seq :pointer) name
                                    (logior SND_SEQ_PORT_CAP_WRITE 
                                            SND_SEQ_PORT_CAP_SUBS_WRITE
                                            SND_SEQ_PORT_CAP_READ
                                            SND_SEQ_PORT_CAP_SUBS_READ
                                            )
                                    (logior SND_SEQ_PORT_TYPE_MIDI_GENERIC 
                                            SND_SEQ_PORT_TYPE_APPLICATION)))

  ;; (snd_seq_nonblock (mem-ref **seq :pointer) 1)
  ;;Shouldn't be necessary 
  )

(defun cond-lookup-test (event-type-key)
  "Helper function for debugging cond-lookup macro e.g
(cond-lookup-test :snd_seq_event_noteon)"
  (let ((event-type (FOREIGN-ENUM-VALUE
                     'SND_SEQ_EVENT_TYPE event-type-key)))
    (cond-lookup)))

(defun midi-data (*data event-type)
  "Mapping event-type and data pointer to lisp-readable midi data"
  (cffi:mem-ref *data (cond-lookup)))

(defun describe-note (*event)
  (let* ((event (mem-ref *event :pointer))
         (event-type (getf (mem-ref
                            event '(:struct snd_seq_event_t))
                           'type))
         (*data (cffi:foreign-slot-pointer
                 event
                 '(:struct snd_seq_event_t) 'data))
         (*dest (cffi:foreign-slot-pointer
                 event
                 '(:struct snd_seq_event_t) 'dest))
         (*source (cffi:foreign-slot-pointer
                   event
                   '(:struct snd_seq_event_t) 'source)))
    (list :source (mem-ref *source  '(:struct snd_seq_addr_t))
          :dest (mem-ref *dest '(:struct snd_seq_addr_t))
          :event-type (foreign-enum-keyword 'snd_seq_event_type
                                            event-type)
          :event-data (midi-data *data event-type))))

(defun recv (&optional (*seq (mem-ref **seq :pointer)))
  "poll the alsa midi port at *seq and my-port, block until there is a midi event to read, then return that event"
  (let* ((npfds (snd_seq_poll_descriptors_count *seq POLLIN)))
    (cffi:with-foreign-objects ((pfds '(:struct pollfd) npfds)
                                (*event '(:struct snd_seq_event_t)))
      (snd_seq_poll_descriptors *seq pfds npfds POLLIN)
      (assert (> (poll pfds npfds -1) 0))
      (snd_seq_event_input *seq *event)
      (describe-note *event))))

(defun send-note-on (velocity note channel)
  (cffi:with-foreign-objects ((*event '(:struct snd_seq_event_t)))
    ))

(defun init ()
  (init-seq "foo")
  (create-port "bar"))

(defun deinit ()
  (deinit-seq)
  (setf *my-port* nil))
