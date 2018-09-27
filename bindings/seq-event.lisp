(in-package #:cl-alsaseq)


;; alsa/seq_event.h

(defctype snd_seq_event_type_t :unsigned-char)

(defcenum snd_seq_event_type
  (:snd_seq_event_system 0)
  :snd_seq_event_result
  (:snd_seq_event_note 5)
  :snd_seq_event_noteon
  :snd_seq_event_noteoff
  :snd_seq_event_keypress
  (:snd_seq_event_controller 10)
  :snd_seq_event_pgmchange
  :snd_seq_event_chanpress
  :snd_seq_event_pitchbend
  :snd_seq_event_control14
  :snd_seq_event_nonregparam
  :snd_seq_event_regparam
  (:snd_seq_event_songpos 20)
  :snd_seq_event_songsel
  :snd_seq_event_qframe
  :snd_seq_event_timesign
  :snd_seq_event_keysign
  (:snd_seq_event_start 30)
  :snd_seq_event_continue
  :snd_seq_event_stop
  :snd_seq_event_setpos_tick
  :snd_seq_event_setpos_time
  :snd_seq_event_tempo
  :snd_seq_event_clock
  :snd_seq_event_tick
  :snd_seq_event_queue_skew
  :snd_seq_event_sync_pos
  (:snd_seq_event_tune_request 40)
  :snd_seq_event_reset
  :snd_seq_event_sensing
  (:snd_seq_event_echo 50)
  :snd_seq_event_oss
  (:snd_seq_event_client_start 60)
  :snd_seq_event_client_exit
  :snd_seq_event_client_change
  :snd_seq_event_port_start
  :snd_seq_event_port_exit
  :snd_seq_event_port_change
  :snd_seq_event_port_subscribed
  :snd_seq_event_port_unsubscribed
  (:snd_seq_event_usr0 90)
  :snd_seq_event_usr1
  :snd_seq_event_usr2
  :snd_seq_event_usr3
  :snd_seq_event_usr4
  :snd_seq_event_usr5
  :snd_seq_event_usr6
  :snd_seq_event_usr7
  :snd_seq_event_usr8
  :snd_seq_event_usr9
  (:snd_seq_event_sysex 130)
  :snd_seq_event_bounce
  (:snd_seq_event_usr_var0 135)
  :snd_seq_event_usr_var1
  :snd_seq_event_usr_var2
  :snd_seq_event_usr_var3
  :snd_seq_event_usr_var4
  (:snd_seq_event_none 255))

(defcstruct snd_seq_addr_t
  (client :unsigned-char)
  (port :unsigned-char))

(defcstruct snd_seq_connect_t
  (sender (:struct snd_seq_addr_t))
  (dest (:struct snd_seq_addr_t)))

(defcstruct snd_seq_real_time_t
  (tv_sec :unsigned-int)
  (tv_nsec :unsigned-int))

(defctype snd_seq_tick_time_t :unsigned-int)

(defcunion snd_seq_timestamp_t
  (tick :unsigned-int)
  (time (:struct snd_seq_real_time_t)))

(defconstant SND_SEQ_TIME_STAMP_TICK (ash 0 0))
(defconstant SND_SEQ_TIME_STAMP_REAL (ash 1 0))
(defconstant SND_SEQ_TIME_STAMP_MASK (ash 1 0))

(defconstant SND_SEQ_TIME_MODE_ABS (ash 0 1))
(defconstant SND_SEQ_TIME_MODE_REL (ash 1 1))
(defconstant SND_SEQ_TIME_MODE_MASK (ash 1 1))

(defconstant SND_SEQ_EVENT_LENGTH_FIXED (ash 0 2))
(defconstant SND_SEQ_EVENT_LENGTH_VARIABLE (ash 1 2))
(defconstant SND_SEQ_EVENT_LENGTH_VARUSR (ash 2 2))

(defconstant SND_SEQ_EVENT_LENGTH_MASK (ash 3 2))

(defconstant SND_SEQ_PRIORITY_NORMAL (ash 0 4))
(defconstant SND_SEQ_PRIORITY_HIGH (ash 1 4))

(defconstant SND_SEQ_PRIORITY_MASK (ash 1 4))

(defcstruct snd_seq_ev_note_t
  (channel :unsigned-char)
  (note :unsigned-char)
  (velocity :unsigned-char)
  (off_velocity :unsigned-char)
  (duration :unsigned-int))

(defcstruct snd_seq_ev_ctrl_t
  (channel :unsigned-char)
  (param :unsigned-int)
  (value :int))

(defcstruct snd_seq_ev_raw8_t
  (d :pointer))

(defcstruct snd_seq_ev_raw32_t
  (d :pointer))

(defcstruct snd_seq_ev_ext_t
  (len :unsigned-int)
  (ptr :pointer)) ; void pointer

(defcstruct snd_seq_result_t
  (event :int)
  (result :int))

(defcstruct snd_seq_queue_skew_t
  (value :unsigned-int)
  (base :unsigned-int))

;; union, not a poiner snd_seq_ev_queue_control_t

(defcunion snd_seq_ev_queue_control_param
  (value :int)
  (time (:union snd_seq_timestamp_t))
  (position :unsigned-int)
  (skew (:struct snd_seq_queue_skew_t))
  (d32 (:array :unsigned-int 2))
  (d32 (:array :unsigned-char 8)))

(defcstruct snd_seq_ev_queue_control_t
  (queue :unsigned-char)
  (unused (:array :unsigned-char 3))
  (param (:union snd_seq_ev_queue_control_param)))

(defcunion snd_seq_event_data
  (note (:struct snd_seq_ev_note_t))
  (control (:struct snd_seq_ev_ctrl_t))
  (raw8 (:struct snd_seq_ev_raw8_t))
  (raw32 (:struct snd_seq_ev_raw32_t))
  (ext (:struct snd_seq_ev_ext_t))
  (queue (:struct snd_seq_ev_queue_control_t))
  (time (:union snd_seq_timestamp_t))
  (addr (:struct snd_seq_addr_t))
  (connect (:struct snd_seq_connect_t))
  (result  (:struct snd_seq_result_t)))

(cffi:defcstruct snd_seq_event_t
	(type snd_seq_event_type_t)
	(flags :unsigned-char)
	(tag :unsigned-char)
	(queue :unsigned-char)
	(time (:union snd_seq_timestamp_t))
        (source (:struct snd_seq_addr_t))
	(dest (:struct snd_seq_addr_t))
	(data (:union snd_seq_event_data)))
