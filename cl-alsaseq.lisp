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
(defun ev-key-int (key)
  (foreign-enum-value
   'snd_seq_event_type key))

(defun ev-int-key (int)
  (foreign-enum-keyword 'snd_seq_event_type int))

(defun cond-lookup-test (event-type-key)
  "Helper function for debugging cond-lookup macro e.g
(cond-lookup-test :snd_seq_event_noteon)"
  (let ((event-type (ev-key-int event-type-key)))
    (cond-lookup)))

(defun midi-data (*data event-type)
  "Mapping event-type and data pointer to lisp-readable midi data"
  (cffi:mem-ref *data (cond-lookup)))

(defun describe-event (event)
  (with-foreign-slots ((type (:pointer data) queue (:pointer source) (:pointer dest)) event (:struct snd_seq_event_t))
    (list ;; :pointer event
          :event-type
          (ev-int-key type)
          :event-data
          (midi-data data type)
          :source
          (list ;; source
           (mem-ref source '(:struct snd_seq_addr_t)))
          :dest
          (list ;; dest
           (mem-ref dest '(:struct snd_seq_addr_t))))))

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
        (snd_seq_event_output *seq event)
        (snd_seq_drain_output *seq)
        (describe-event event)))))

(defcfun "memset" :pointer
  (*dest :pointer)
  (val :int)
  (*n :uchar))

(defun clear-foreign-type (*ev type)
  (memset *ev 0 (foreign-type-size type)))

(defmacro with-snd_seq_ev_ctrl ((var channel unused param value)
                                &body body)
  (declare (ignore unused))
  `(let ((,var (convert-to-foreign (list
                                    'channel ,channel
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

(defmacro with-midi-event ((var type &key (queue SND_SEQ_QUEUE_DIRECT)) &body body)
`(let ((,var (convert-to-foreign (list
                                      'type ,type
                                      'queue ,queue
                                      )
                                   '(:struct snd_seq_event_t))))
   ,@body))

(defun set-addr-slots (addr *port *client)
  (with-foreign-slots (((:pointer port) (:pointer client))
                             addr (:struct snd_seq_addr_t))
          (setf (mem-ref port :uchar) *port )
          (setf (mem-ref client :uchar) *client )))

(defcfun "memcpy" :void
  (*dest :pointer)
  (*src :pointer)
  (*n :uchar))

(defun send-midi (*seq my-port *data note-type)
  (with-midi-event (event (ev-key-int
                           note-type))
    (with-foreign-slots (((:pointer dest) (:pointer source) (:pointer data))
                         event
                         (:struct snd_seq_event_t))
      (set-addr-slots source my-port 0)
      (set-addr-slots dest SND_SEQ_ADDRESS_UNKNOWN SND_SEQ_ADDRESS_SUBSCRIBERS)
      (memcpy data *data (foreign-type-size '(:union snd_seq_event_data))))
    (snd_seq_event_output *seq event)
    (snd_seq_drain_output *seq)))

(defun event-type-assert (type type-min type-max)
  (let ((type-value (foreign-enum-value 'snd_seq_event_type type)))
    (assert (and (>= type-value (foreign-enum-value 'snd_seq_event_type
                                                    type-min))
                 (< type-value (foreign-enum-value 'snd_seq_event_type
                                                   type-max))))))

(defun send-note (velocity note channel note-type
                     &optional (*seq (mem-ref **seq :pointer))
                       (my-port *my-port*))
  (event-type-assert note-type :SND_SEQ_EVENT_NOTE :SND_SEQ_EVENT_CONTROLLER)
  (with-snd_seq_ev_note (data note velocity channel 0 0)
    (send-midi *seq my-port data note-type )))

(defun send-ctrl (channel param value ctrl-type
                  &optional (*seq (mem-ref **seq :pointer))
                    (my-port *my-port*))
  (event-type-assert ctrl-type :SND_SEQ_EVENT_CONTROLLER :SND_SEQ_EVENT_SONGPOS)
  (with-snd_seq_ev_ctrl (data channel (null-pointer) param value)
    (send-midi *seq my-port data ctrl-type)))

(defmacro def-event-func (event args &body body)
  `(defun ,(intern (format nil "SEND-~A" event))
       ,args ,@body))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (mapcar
   (lambda (event-type)
     (let ((ctrl-type (intern (format nil "SND_SEQ_EVENT_~A" event-type) :keyword)))
       (compile (intern (format nil "SEND-~A" event-type))
                `(lambda (channel param value
                          &optional (*seq (mem-ref **seq :pointer))
                            (my-port *my-port*))
                   (send-ctrl channel param value ,ctrl-type *seq my-port)))))
   '(:PGMCHANGE
     :CHANPRESS
     :PITCHBEND
     :CONTROL
     :NONREGPARAM
     :REGPARAM)))


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

(defvar *in-chan* (make-instance 'calispel:channel
                                 :buffer (MAKE-INSTANCE 'jpl-queues:LOSSY-BOUNDED-FIFO-QUEUE
                                                        :CAPACITY 10)))

(defvar *in-chan* (make-instance 'calispel:channel
                                 :buffer (MAKE-INSTANCE 'jpl-queues:LOSSY-BOUNDED-FIFO-QUEUE
                                                        :CAPACITY 10)))
