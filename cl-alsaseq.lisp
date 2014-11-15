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

(defun describe-event (event)
  (with-foreign-slots ((type (:pointer data) queue (:pointer source) (:pointer dest)) event (:struct snd_seq_event_t))
    (list :event-type ;; (foreign-enum-keyword 'snd_seq_event_type
          ;;                       event-type)
          type
          :event-data ;; (midi-data *data event-type)
          (list data (midi-data data type))
          :queue ;; (getf (mem-ref event '(:struct snd_seq_event_t))
          ;;       'queue)
          queue
          :source ;; (mem-ref *source '(:struct snd_seq_addr_t))
          (list source (mem-ref source '(:struct snd_seq_addr_t)))
          :dest ;; (mem-ref *dest '(:struct snd_seq_addr_t))
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
        (dump-event *event)
        (print (describe-event event))
        ;; (snd_seq_ev_set_source *event *my-port*)
        (setf (mem-ref (foreign-slot-pointer *source
                                             '(:struct snd_seq_addr_t) 'port)
                       :uchar)
              *my-port*)
      ;; (snd_seq_ev_set_subs ev)
        (setf (mem-ref (foreign-slot-pointer *dest
                                             '(:struct snd_seq_addr_t) 'client)
                       :uchar)
              SND_SEQ_ADDRESS_SUBSCRIBERS)
        (setf (mem-ref (foreign-slot-pointer *dest
                                             '(:struct snd_seq_addr_t) 'port)
                       :uchar)
              SND_SEQ_ADDRESS_UNKNOWN)
      ;; snd_seq_ev_set_direct(ev);
        (setf (mem-ref (foreign-slot-pointer *event
                                             '(:struct snd_seq_event_t)
                                             'queue) :uchar)
              SND_SEQ_QUEUE_DIRECT)
        (print (describe-event event))
        (snd_seq_event_output *seq event)
        (snd_seq_drain_output *seq)))))
      ;; snd_seq_event_output(seq, ev);
      ;; snd_seq_drain_output(seq);
  ;; (with-snd_seq_addr (dest SND_SEQ_ADDRESS_SUBSCRIBERS
  ;;                            SND_SEQ_ADDRESS_UNKNOWN)
  ;;   (with-snd_seq_addr (source 0 my-port)
  ;;     (with-snd_seq_ev_note (data note velocity channel 0 0)
  ;;       (with-snd_seq_event (ev data dest source (null-pointer)
  ;;                               SND_SEQ_QUEUE_DIRECT 0 0
  ;;                               (FOREIGN-ENUM-VALUE
  ;;                                'SND_SEQ_EVENT_TYPE :SND_SEQ_EVENT_NOTEON))


(defcfun "memset" :pointer
  (*dest :pointer)
  (val :int)
  (*n :uchar))

(defun clear-foreign-type (*ev type)
   ;;(memset *ev 0 (sizeof snd_seq_event_t)))
  (memset *ev 0 (foreign-type-size type)))




(defmacro with-snd_seq_addr ((var client port) &body body)
  `(with-foreign-object (,var '(:struct snd_seq_addr_t))
     (setf (foreign-slot-value ,var '(:struct snd_seq_addr_t)
                               'client) ,client)
     (setf (foreign-slot-value ,var '(:struct snd_seq_addr_t)
                               'port) ,port)
     ,@body))

(defmacro setf-snd_seq_event-slot (ptr slot val)
  `(setf (foreign-slot-value ,ptr
                             '(:struct snd_seq_event_t)
                             ,slot)
         ,val))

(defmacro with-snd_seq_event ((var data dest source time
                                   queue tag flags type) &body body)
  (declare (ignore time))
  `(with-foreign-object (,var '(:struct snd_seq_event_t))
     (clear-foreign-type ,var '(:struct snd_seq_event_t))
     (setf-snd_seq_event-slot ,var 'data (mem-ref ,data :pointer))
     (setf-snd_seq_event-slot ,var 'dest (mem-ref ,dest :pointer))
     (setf-snd_seq_event-slot ,var 'source (mem-ref ,source :pointer))
     ;; (setf-snd_seq_event-slot ,var 'time ,time)
     (setf-snd_seq_event-slot ,var 'queue ,queue)
     (setf-snd_seq_event-slot ,var 'tag ,tag)
     (setf-snd_seq_event-slot ,var 'flags ,flags)
     (setf-snd_seq_event-slot ,var 'type ,type)
     ,@body))

(defmacro setf-snd_seq_ev_note-slot (ptr slot val)
  `(setf (foreign-slot-value ,ptr
                             '(:struct snd_seq_ev_note_t)
                             ,slot)
         ,val))

(defmacro with-snd_seq_ev_note ((var note velocity channel off_velocity duration)
                                &body body)
  `(with-foreign-object (,var '(:struct snd_seq_ev_note_t))
     (clear-foreign-type ,var '(:struct snd_seq_ev_note_t))
     (setf-snd_seq_ev_note-slot ,var 'note ,note)
     (setf-snd_seq_ev_note-slot ,var 'velocity ,velocity)
     (setf-snd_seq_ev_note-slot ,var 'channel ,channel)
     (setf-snd_seq_ev_note-slot ,var 'off_velocity ,off_velocity)
     (setf-snd_seq_ev_note-slot ,var 'duration ,duration)
     ,@body))

(defcvar "errno" :int)

(defun send-note-on (velocity note channel
                     &optional (*seq (mem-ref **seq :pointer))
                       (my-port *my-port*))
                       
  (with-snd_seq_addr (dest SND_SEQ_ADDRESS_SUBSCRIBERS
                             SND_SEQ_ADDRESS_UNKNOWN)
    (with-snd_seq_addr (source 0 my-port)
      (with-snd_seq_ev_note (data note velocity channel 0 0)
        (with-snd_seq_event (ev data dest source (null-pointer)
                                SND_SEQ_QUEUE_DIRECT 0 0
                                (FOREIGN-ENUM-VALUE
                                 'SND_SEQ_EVENT_TYPE :SND_SEQ_EVENT_NOTEON))
          ;;XXX DON'T DELETE
          ;;this snippet finally shows how to write &ev
          ;; (with-foreign-pointer (&ev 1)
          ;;   (setf (mem-ref &ev :pointer) ev)

          ;; (snd_seq_ev_set_source *event *my-port*)
          ;; (snd_seq_ev_set_subs ev)
          ;; snd_seq_ev_set_direct(ev);
          ;; snd_seq_event_output(seq, ev);
          ;; snd_seq_drain_output(seq);
          (dump-event ev)
          (print (describe-event ev))
          (setf *errno* (snd_seq_event_output_direct *seq ev))
          (foreign-funcall "strerror" :int *errno* :string))))))


(defun init ()
  (init-seq "foo")
  (create-port "bar"))

(defun deinit ()
  (deinit-seq)
  (setf *my-port* nil))
