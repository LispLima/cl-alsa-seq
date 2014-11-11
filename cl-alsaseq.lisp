;;;; cl-alsaseq.lisp

(in-package #:cl-alsaseq)

;;; "cl-alsaseq" goes here. Hacks and glory await!
(load-foreign-library "libasound.so")

(defparameter seq (foreign-alloc :pointer))

(defun init-seq ()
  (snd_seq_open seq "default" SND_SEQ_OPEN_DUPLEX 0)
  (snd_seq_set_client_name (mem-ref seq :pointer) "Common Lisp"))

(defparameter my-port nil)
(defun create-port (name)
  (setf my-port
        (snd_seq_create_simple_port (mem-ref seq :pointer) name
                                    (logior SND_SEQ_PORT_CAP_WRITE 
                                            SND_SEQ_PORT_CAP_SUBS_WRITE
                                            SND_SEQ_PORT_CAP_READ
                                            SND_SEQ_PORT_CAP_SUBS_READ)
                                    (logior SND_SEQ_PORT_TYPE_MIDI_GENERIC 
                                            SND_SEQ_PORT_TYPE_APPLICATION))))

(defparameter *event (foreign-alloc '(:struct snd_seq_event_t)))
(defparameter pfds nil)

(defun first-poll ()
  (setf pfds (foreign-alloc '(:struct pollfd)))
  (midi-poll))


(defun lookup-table ()
  (list
   :SND_SEQ_EVENT_SYSTEM nil
   :SND_SEQ_EVENT_NOTE '(:struct snd_seq_ev_note_t)
   :SND_SEQ_EVENT_CONTROLLER '(:struct snd_seq_ev_ctrl_t)
   :SND_SEQ_EVENT_START '(:struct snd_seq_ev_queue_control_t)
   :SND_SEQ_EVENT_TUNE_REQUEST nil
   :SND_SEQ_EVENT_CLIENT_START '(:struct snd_seq_addr_t)
   :SND_SEQ_EVENT_USR0 nil
   :SND_SEQ_EVENT_SYSEX '(:struct ext)
   :SND_SEQ_EVENT_USR_VAR0 nil))

(defun snd-seq-condition (key val)
  `((>= event-type (foreign-enum-value 'snd_seq_event_type ,key))
    ',val))

(defmacro cond-lookup ()
  (labels ((lookup (rest)
             (if rest
                 (cons (snd-seq-condition (car rest)
                                          (cadr rest))
                       (lookup (cddr rest))))))
    `(cond ,@(reverse (lookup (lookup-table))))))

(defun cond-lookup-test (event-type-key)
  (let ((event-type (FOREIGN-ENUM-VALUE
                     'SND_SEQ_EVENT_TYPE event-type-key)))
    (cond-lookup)))

(defun midi-data (*data event-type)
  (cffi:mem-ref *data (print (cond-lookup))))

;; (defun midi-data (event-type)
;;   (declare (ignore event-type))
;;   '(:STRUCT SND_SEQ_EV_NOTE_T))
;;This function missing some header stuff for the polling lib
(defun midi-poll ()
  (assert pfds)
  (snd_seq_poll_descriptors (mem-ref seq :pointer) pfds 1 POLLIN)
  (if (> (print (poll pfds 1 -1)) 0)
      (progn
        (snd_seq_event_input (mem-ref seq :pointer)
                             *event)
        (let* ((event (mem-ref
                      (mem-ref *event :pointer) '(:struct snd_seq_event_t)))
               (event-type (getf event 'type))
               (*data (cffi:foreign-slot-pointer
                            (mem-ref *event :pointer)
                            '(:struct snd_seq_event_t) 'data))
               (*dest (cffi:foreign-slot-pointer
                       (mem-ref *event :pointer)
                       '(:struct snd_seq_event_t) 'dest))
               (*source (cffi:foreign-slot-pointer
                         (mem-ref *event :pointer)
                         '(:struct snd_seq_event_t) 'source))
               )
          (print (list :event event
                       :source (mem-ref *source  '(:struct snd_seq_addr_t))
                       :dest (mem-ref *dest '(:struct snd_seq_addr_t))
                       :event-type (foreign-enum-keyword 'snd_seq_event_type
                                                         event-type)
                       :event-data (midi-data *data event-type)))

          ;; (snd_seq_ev_set_source (mem-ref event :pointer) my-port)
          (setf (getf (mem-ref *source  '(:struct snd_seq_addr_t)) 'port)
                my-port)

          ;; (snd_seq_ev_set_subs (mem-ref event :pointer) event)
          (setf (getf (mem-ref *dest  '(:struct snd_seq_addr_t)) 'client)
                SND_SEQ_ADDRESS_SUBSCRIBERS)
          (setf (getf (mem-ref *dest  '(:struct snd_seq_addr_t)) 'port)
                SND_SEQ_ADDRESS_UNKNOWN)
          
          ;; (snd_seq_ev_set_direct (mem-ref event))
          (setf (getf event 'queue)
                SND_SEQ_QUEUE_DIRECT)

          (snd_seq_event_output (mem-ref seq :pointer)
                                (mem-ref *event :pointer))
          (snd_seq_drain_output (mem-ref seq :pointer))
          (print (list :event event
                       :source (mem-ref *source  '(:struct snd_seq_addr_t))
                       :dest (mem-ref *dest '(:struct snd_seq_addr_t))
                       :event-type (foreign-enum-keyword 'snd_seq_event_type
                                                         event-type)
                       :event-data (midi-data *data event-type)))))))


(defun init ()
  (init-seq)
  (create-port "foo")
  (first-poll))
