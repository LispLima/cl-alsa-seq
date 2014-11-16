(in-package #:cl-alsaseq)

(load-foreign-library "libasound.so")

(defvar **seq nil)

(defun init-seq (client-name)
  (assert (null **seq))
  (setf **seq (foreign-alloc :pointer))
  (snd_seq_open **seq "default" SND_SEQ_OPEN_DUPLEX 0)
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
                                            SND_SEQ_PORT_TYPE_APPLICATION))))

(defun cond-lookup-test (event-type-key)
  "Helper function for debugging cond-lookup macro e.g
(cond-lookup-test :snd_seq_event_noteon)"
  (let ((event-type (FOREIGN-ENUM-VALUE
                     'SND_SEQ_EVENT_TYPE event-type-key)))
    (cond-lookup)))

(defun midi-data (*data event-type)
  "Mapping event-type and data pointer to lisp-readable midi data"
  (cffi:mem-ref *data (cond-lookup)))

(defun describe-event (event)
  (with-foreign-slots ((type (:pointer data) queue (:pointer source) (:pointer dest)) event (:struct snd_seq_event_t))
    (list :event-type
          (cffi:foreign-enum-keyword 'snd_seq_event_type type)
          :event-data
          (list data (midi-data data type))
          :queue
          queue
          :source
          (list source (mem-ref source '(:struct snd_seq_addr_t)))
          :dest
          (list dest (mem-ref dest '(:struct snd_seq_addr_t)))
          )))


(defmacro dump-event (*event)
  `(print (mem-ref ,*event '(:struct snd_seq_event_t))))

(defun recv (&optional (*seq (mem-ref **seq :pointer)))
  "poll the alsa midi port at *seq and my-port, block until there is a midi event to read, then return that event"
  (let* ((npfds (snd_seq_poll_descriptors_count *seq POLLIN)))
    (cffi:with-foreign-objects ((pfds '(:struct pollfd) npfds)
                                (*event '(:struct snd_seq_event_t)))
      (snd_seq_poll_descriptors *seq pfds npfds POLLIN)
      (assert (> (poll pfds npfds -1) 0))
      (snd_seq_event_input *seq *event)
      (describe-event (mem-ref *event :pointer)))))

(defun echo (&optional (*seq (mem-ref **seq :pointer)))
  "poll the alsa midi port at *seq and my-port, block until there is a midi event to read, then echo that event"
  (let* ((npfds (snd_seq_poll_descriptors_count *seq POLLIN)))
    (cffi:with-foreign-objects ((pfds '(:struct pollfd) npfds)
                                (*event '(:struct snd_seq_event_t)))
      (snd_seq_poll_descriptors *seq pfds npfds POLLIN)
      (assert (> (poll pfds npfds -1) 0))
      (snd_seq_event_input *seq *event)
      (let* ((event (mem-ref *event :pointer))
             (*source (cffi:foreign-slot-pointer
                       event '(:struct snd_seq_event_t) 'source))
             (*dest (cffi:foreign-slot-pointer
                       event '(:struct snd_seq_event_t) 'dest)))
        (setf (mem-ref (foreign-slot-pointer *source
                                             '(:struct snd_seq_addr_t) 'port)
                       :uchar)
              *my-port*)
        (setf (mem-ref (foreign-slot-pointer *dest
                                             '(:struct snd_seq_addr_t) 'client)
                       :uchar)
              SND_SEQ_ADDRESS_SUBSCRIBERS)
        (setf (mem-ref (foreign-slot-pointer *dest
                                             '(:struct snd_seq_addr_t) 'port)
                       :uchar)
              SND_SEQ_ADDRESS_UNKNOWN)
        (setf (mem-ref (foreign-slot-pointer *event
                                             '(:struct snd_seq_event_t)
                                             'queue) :uchar)
              SND_SEQ_QUEUE_DIRECT)
        (print (describe-event event))
        (snd_seq_event_output *seq event)
        (snd_seq_drain_output *seq)))))

(defcfun "memset" :pointer
  (*dest :pointer)
  (val :int)
  (*n :uchar))

(defun clear-foreign-type (*ev type)
  (memset *ev 0 (foreign-type-size type)))

(defmacro setf-snd_seq_event-slot (ptr slot val)
  `(setf (foreign-slot-value ,ptr
                             '(:struct snd_seq_event_t)
                             ,slot)
         ,val))

(defmacro with-snd_seq_ev_ctrl ((var channel unused param value)
                                &body body)
  `(let ((,var (convert-to-foreign (list
                                    'channel ,channel
                                    'unused ,unused
                                    'param ,param
                                    'value ,value
                                    )
                                   '(:struct snd_seq_ev_ctrl_t))))
     ,@body))

(defmacro with-snd_seq_ev_note ((var note velocity channel off_velocity duration)
                                &body body)
  `(let ((,var (convert-to-foreign (list
                                    'note ,note
                                    'velocity ,velocity
                                    'channel ,channel
                                    'off_velocity ,off_velocity
                                    'duration ,duration
                                    )
                                   '(:struct snd_seq_ev_note_t))))
     ,@body))

(defcvar "errno" :int)

(defun send-midi (*seq my-port data note-type)
  (let ((event (convert-to-foreign (list
                                      'type (foreign-enum-value
                                             'snd_seq_event_type
                                             note-type)
                                      'queue SND_SEQ_QUEUE_DIRECT
                                      )
                                     '(:struct snd_seq_event_t))))
      (setf-snd_seq_event-slot event 'data (mem-ref data :pointer))
      (with-foreign-slots (((:pointer dest) (:pointer source))
                           event
                           (:struct snd_seq_event_t))
        (with-foreign-slots (((:pointer port) (:pointer client))
                             source (:struct snd_seq_addr_t))
          (setf (mem-ref port :uchar) my-port))
        (with-foreign-slots (((:pointer port) (:pointer client))
                             dest (:struct snd_seq_addr_t))
          (setf (mem-ref port :uchar) SND_SEQ_ADDRESS_UNKNOWN)
          (setf (mem-ref client :uchar) SND_SEQ_ADDRESS_SUBSCRIBERS)))
      (describe-event event)
      (snd_seq_event_output *seq event)
      (snd_seq_drain_output *seq)))

(defun send-note (velocity note channel note-type
                     &optional (*seq (mem-ref **seq :pointer))
                       (my-port *my-port*))
  (assert (or (equal note-type :SND_SEQ_EVENT_NOTEOFF)
              (equal note-type :SND_SEQ_EVENT_NOTEON)))
  (with-snd_seq_ev_note (data note velocity channel 0 0)
    (send-midi *seq my-port data note-type )))

(defun event-type-assert (type type-min type-max)
  (let ((type-value (foreign-enum-value 'snd_seq_event_type type)))
    (print type-value)
    (assert (and (>= type-value (print (foreign-enum-value 'snd_seq_event_type
                                                    type-min)))
                 (< type-value (print (foreign-enum-value 'snd_seq_event_type
                                                   type-max)))))))

(defun send-ctrl (channel param value ctrl-type
                  &optional (*seq (mem-ref **seq :pointer))
                    (my-port *my-port*))
  (print ctrl-type)
  (event-type-assert ctrl-type :SND_SEQ_EVENT_CONTROLLER :SND_SEQ_EVENT_SONGPOS)
  (with-snd_seq_ev_ctrl (data channel (null-pointer) param value)
    (send-midi *seq my-port data ctrl-type)))

(defmacro def-event-func (event args &body body)
  `(defun ,(intern (format nil "SEND-~A" event))
       ,args ,@body))

(mapcar
 (lambda (event-type)
   (let ((ctrl-type (intern (format nil "SND_SEQ_EVENT_~A" event-type) :keyword)))
     (compile (intern (print (format nil "SEND-~A" event-type)))
              (print `(lambda (channel param value
                                            &optional (*seq (mem-ref **seq :pointer))
                                            (my-port *my-port*))
                 (send-ctrl channel param value ,ctrl-type *seq my-port))))))
 '(:PGMCHANGE
   :CHANPRESS
   :PITCHBEND
   :CONTROL
   :NONREGPARAM
   :REGPARAM))


(defun send-note-on (velocity note channel
                     &optional (*seq (mem-ref **seq :pointer))
                       (my-port *my-port*))
  (send-note velocity note channel :SND_SEQ_EVENT_NOTEON *seq my-port))

(defun send-note-off (velocity note channel
                     &optional (*seq (mem-ref **seq :pointer))
                       (my-port *my-port*))
  (send-note velocity note channel :SND_SEQ_EVENT_NOTEOFF *seq my-port))

(defun init ()
  (init-seq "foo")
  (create-port "bar"))

(defun deinit ()
  (deinit-seq)
  (setf *my-port* nil))
