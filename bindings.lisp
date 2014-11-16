;;; This file was automatically generated by SWIG (http://www.swig.org).
;;; Version 3.0.3
;;;
;;; Do not make changes to this file unless you know what you are doing--modify
;;; the SWIG interface file instead.

(in-package :cl-alsaseq)



(cl:defconstant _SYS_POLL_H 1)

(cffi:defcstruct pollfd
	(fd :int)
	(events :short)
	(revents :short))

(cffi:defcfun ("poll" poll) :int
  (__fds :pointer)
  (__nfds :unsigned-long)
  (__timeout :int))

(cl:defconstant POLLIN #x001)

(cl:defconstant POLLPRI #x002)

(cl:defconstant POLLOUT #x004)

(cl:defconstant POLLERR #x008)

(cl:defconstant POLLHUP #x010)

(cl:defconstant POLLNVAL #x020)

(cffi:defcenum snd_seq_event_type
  (:SND_SEQ_EVENT_SYSTEM #.0)
  :SND_SEQ_EVENT_RESULT
  (:SND_SEQ_EVENT_NOTE #.5)
  :SND_SEQ_EVENT_NOTEON
  :SND_SEQ_EVENT_NOTEOFF
  :SND_SEQ_EVENT_KEYPRESS
  (:SND_SEQ_EVENT_CONTROLLER #.10)
  :SND_SEQ_EVENT_PGMCHANGE
  :SND_SEQ_EVENT_CHANPRESS
  :SND_SEQ_EVENT_PITCHBEND
  :SND_SEQ_EVENT_CONTROL14
  :SND_SEQ_EVENT_NONREGPARAM
  :SND_SEQ_EVENT_REGPARAM
  (:SND_SEQ_EVENT_SONGPOS #.20)
  :SND_SEQ_EVENT_SONGSEL
  :SND_SEQ_EVENT_QFRAME
  :SND_SEQ_EVENT_TIMESIGN
  :SND_SEQ_EVENT_KEYSIGN
  (:SND_SEQ_EVENT_START #.30)
  :SND_SEQ_EVENT_CONTINUE
  :SND_SEQ_EVENT_STOP
  :SND_SEQ_EVENT_SETPOS_TICK
  :SND_SEQ_EVENT_SETPOS_TIME
  :SND_SEQ_EVENT_TEMPO
  :SND_SEQ_EVENT_CLOCK
  :SND_SEQ_EVENT_TICK
  :SND_SEQ_EVENT_QUEUE_SKEW
  :SND_SEQ_EVENT_SYNC_POS
  (:SND_SEQ_EVENT_TUNE_REQUEST #.40)
  :SND_SEQ_EVENT_RESET
  :SND_SEQ_EVENT_SENSING
  (:SND_SEQ_EVENT_ECHO #.50)
  :SND_SEQ_EVENT_OSS
  (:SND_SEQ_EVENT_CLIENT_START #.60)
  :SND_SEQ_EVENT_CLIENT_EXIT
  :SND_SEQ_EVENT_CLIENT_CHANGE
  :SND_SEQ_EVENT_PORT_START
  :SND_SEQ_EVENT_PORT_EXIT
  :SND_SEQ_EVENT_PORT_CHANGE
  :SND_SEQ_EVENT_PORT_SUBSCRIBED
  :SND_SEQ_EVENT_PORT_UNSUBSCRIBED
  (:SND_SEQ_EVENT_USR0 #.90)
  :SND_SEQ_EVENT_USR1
  :SND_SEQ_EVENT_USR2
  :SND_SEQ_EVENT_USR3
  :SND_SEQ_EVENT_USR4
  :SND_SEQ_EVENT_USR5
  :SND_SEQ_EVENT_USR6
  :SND_SEQ_EVENT_USR7
  :SND_SEQ_EVENT_USR8
  :SND_SEQ_EVENT_USR9
  (:SND_SEQ_EVENT_SYSEX #.130)
  :SND_SEQ_EVENT_BOUNCE
  (:SND_SEQ_EVENT_USR_VAR0 #.135)
  :SND_SEQ_EVENT_USR_VAR1
  :SND_SEQ_EVENT_USR_VAR2
  :SND_SEQ_EVENT_USR_VAR3
  :SND_SEQ_EVENT_USR_VAR4
  (:SND_SEQ_EVENT_NONE #.255))

(cffi:defcstruct snd_seq_addr_t
	(client :unsigned-char)
	(port :unsigned-char))

(cffi:defcstruct snd_seq_connect_t
	(sender (:pointer (:struct snd_seq_addr_t)))
	(dest (:pointer (:struct snd_seq_addr_t))))

(cffi:defcstruct snd_seq_real_time_t
	(tv_sec :unsigned-int)
	(tv_nsec :unsigned-int))

(cffi:defcunion snd_seq_timestamp_t
	(tick :unsigned-int)
	(time :pointer))

(cl:defconstant SND_SEQ_TIME_STAMP_TICK (cl:ash 0 0))

(cl:defconstant SND_SEQ_TIME_STAMP_REAL (cl:ash 1 0))

(cl:defconstant SND_SEQ_TIME_STAMP_MASK (cl:ash 1 0))

(cl:defconstant SND_SEQ_TIME_MODE_ABS (cl:ash 0 1))

(cl:defconstant SND_SEQ_TIME_MODE_REL (cl:ash 1 1))

(cl:defconstant SND_SEQ_TIME_MODE_MASK (cl:ash 1 1))

(cl:defconstant SND_SEQ_EVENT_LENGTH_FIXED (cl:ash 0 2))

(cl:defconstant SND_SEQ_EVENT_LENGTH_VARIABLE (cl:ash 1 2))

(cl:defconstant SND_SEQ_EVENT_LENGTH_VARUSR (cl:ash 2 2))

(cl:defconstant SND_SEQ_EVENT_LENGTH_MASK (cl:ash 3 2))

(cl:defconstant SND_SEQ_PRIORITY_NORMAL (cl:ash 0 4))

(cl:defconstant SND_SEQ_PRIORITY_HIGH (cl:ash 1 4))

(cl:defconstant SND_SEQ_PRIORITY_MASK (cl:ash 1 4))

(cffi:defcstruct snd_seq_ev_note_t
	(channel :unsigned-char)
	(note :unsigned-char)
	(velocity :unsigned-char)
	(off_velocity :unsigned-char)
	(duration :unsigned-int))

(cffi:defcstruct snd_seq_ev_ctrl_t
	(channel :unsigned-char)
	(param :unsigned-int)
	(value :int))

(cffi:defcstruct snd_seq_ev_raw8_t
	(d :pointer))

(cffi:defcstruct snd_seq_ev_raw32_t
	(d :pointer))

(cffi:defcstruct snd_seq_ev_ext_t
	(len :unsigned-int)
	(ptr :pointer))

(cffi:defcstruct snd_seq_result_t
	(event :int)
	(result :int))

(cffi:defcstruct snd_seq_queue_skew_t
	(value :unsigned-int)
	(base :unsigned-int))

(cffi:defcstruct snd_seq_ev_queue_control_t
	(queue :unsigned-char)
	(unused :pointer)
	(param :pointer))

(cffi:defcunion snd_seq_ev_queue_control_param
	(value :int)
	(time (:union snd_seq_timestamp_t))
	(position :unsigned-int)
	(skew (:pointer (:struct snd_seq_queue_skew_t)))
	(d32 :pointer)
	(d8 :pointer))

(cffi:defcunion snd_seq_event_data
	(note (:struct snd_seq_ev_note_t))
	(control (:struct snd_seq_ev_ctrl_t))
	(raw8 (:struct snd_seq_ev_raw8_t))
	(raw32 (:struct snd_seq_ev_raw32_t))
	(ext (:struct snd_seq_ev_ext_t))
	(queue (:struct snd_seq_ev_queue_control_t))
	;; (time (:union snd_seq_timestamp_t))
	(time :pointer)
	(addr (:struct snd_seq_addr_t))
	(connect (:struct snd_seq_connect_t))
	(result  (:struct snd_seq_result_t)))

(cffi:defcstruct snd_seq_event_t
	(type :unsigned-char)
	(flags :unsigned-char)
	(tag :unsigned-char)
	(queue :unsigned-char)
	;; (time (:union snd_seq_timestamp_t))
        (time :pointer)
        ;; (source (:pointer (:struct snd_seq_addr_t)))
	;; (dest (:pointer (:struct snd_seq_addr_t)))
        (source (:pointer (:struct snd_seq_addr_t)) :offset 12)
	(dest (:pointer (:struct snd_seq_addr_t)) :offset 14)
	(data (:union snd_seq_event_data) :offset 16)
        ;; (data (:union snd_seq_event_data))
        )

(cl:defconstant SND_SEQ_OPEN_OUTPUT 1)

(cl:defconstant SND_SEQ_OPEN_INPUT 2)

(cl:defconstant SND_SEQ_OPEN_DUPLEX (cl:logior 1 2))

(cl:defconstant SND_SEQ_NONBLOCK #x0001)

(cffi:defcenum snd_seq_type_t
	:SND_SEQ_TYPE_HW
	:SND_SEQ_TYPE_SHM
	:SND_SEQ_TYPE_INET)

(cl:defconstant SND_SEQ_ADDRESS_UNKNOWN 253)

(cl:defconstant SND_SEQ_ADDRESS_SUBSCRIBERS 254)

(cl:defconstant SND_SEQ_ADDRESS_BROADCAST 255)

(cl:defconstant SND_SEQ_CLIENT_SYSTEM 0)

(cffi:defcfun ("snd_seq_open" snd_seq_open) :int
  (handle :pointer)
  (name :string)
  (streams :int)
  (mode :int))

(cffi:defcfun ("snd_seq_open_lconf" snd_seq_open_lconf) :int
  (handle :pointer)
  (name :string)
  (streams :int)
  (mode :int)
  (lconf :pointer))

(cffi:defcfun ("snd_seq_name" snd_seq_name) :string
  (seq :pointer))

(cffi:defcfun ("snd_seq_type" snd_seq_type) snd_seq_type_t
  (seq :pointer))

(cffi:defcfun ("snd_seq_close" snd_seq_close) :int
  (handle :pointer))

(cffi:defcfun ("snd_seq_poll_descriptors_count" snd_seq_poll_descriptors_count) :int
  (handle :pointer)
  (events :short))

(cffi:defcfun ("snd_seq_poll_descriptors" snd_seq_poll_descriptors) :int
  (handle :pointer)
  (pfds :pointer)
  (space :unsigned-int)
  (events :short))

(cffi:defcfun ("snd_seq_poll_descriptors_revents" snd_seq_poll_descriptors_revents) :int
  (seq :pointer)
  (pfds :pointer)
  (nfds :unsigned-int)
  (revents :pointer))

(cffi:defcfun ("snd_seq_nonblock" snd_seq_nonblock) :int
  (handle :pointer)
  (nonblock :int))

(cffi:defcfun ("snd_seq_client_id" snd_seq_client_id) :int
  (handle :pointer))

(cffi:defcfun ("snd_seq_get_output_buffer_size" snd_seq_get_output_buffer_size) :pointer
  (handle :pointer))

(cffi:defcfun ("snd_seq_get_input_buffer_size" snd_seq_get_input_buffer_size) :pointer
  (handle :pointer))

(cffi:defcfun ("snd_seq_set_output_buffer_size" snd_seq_set_output_buffer_size) :int
  (handle :pointer)
  (size :pointer))

(cffi:defcfun ("snd_seq_set_input_buffer_size" snd_seq_set_input_buffer_size) :int
  (handle :pointer)
  (size :pointer))

(cffi:defcfun ("snd_seq_system_info_sizeof" snd_seq_system_info_sizeof) :pointer)

(cffi:defcfun ("snd_seq_system_info_malloc" snd_seq_system_info_malloc) :int
  (ptr :pointer))

(cffi:defcfun ("snd_seq_system_info_free" snd_seq_system_info_free) :void
  (ptr :pointer))

(cffi:defcfun ("snd_seq_system_info_copy" snd_seq_system_info_copy) :void
  (dst :pointer)
  (src :pointer))

(cffi:defcfun ("snd_seq_system_info_get_queues" snd_seq_system_info_get_queues) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_system_info_get_clients" snd_seq_system_info_get_clients) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_system_info_get_ports" snd_seq_system_info_get_ports) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_system_info_get_channels" snd_seq_system_info_get_channels) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_system_info_get_cur_clients" snd_seq_system_info_get_cur_clients) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_system_info_get_cur_queues" snd_seq_system_info_get_cur_queues) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_system_info" snd_seq_system_info) :int
  (handle :pointer)
  (info :pointer))

(cffi:defcenum snd_seq_client_type_t
	(:SND_SEQ_USER_CLIENT #.1)
	(:SND_SEQ_KERNEL_CLIENT #.2))

(cffi:defcfun ("snd_seq_client_info_sizeof" snd_seq_client_info_sizeof) :pointer)

(cffi:defcfun ("snd_seq_client_info_malloc" snd_seq_client_info_malloc) :int
  (ptr :pointer))

(cffi:defcfun ("snd_seq_client_info_free" snd_seq_client_info_free) :void
  (ptr :pointer))

(cffi:defcfun ("snd_seq_client_info_copy" snd_seq_client_info_copy) :void
  (dst :pointer)
  (src :pointer))

(cffi:defcfun ("snd_seq_client_info_get_client" snd_seq_client_info_get_client) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_client_info_get_type" snd_seq_client_info_get_type) snd_seq_client_type_t
  (info :pointer))

(cffi:defcfun ("snd_seq_client_info_get_name" snd_seq_client_info_get_name) :string
  (info :pointer))

(cffi:defcfun ("snd_seq_client_info_get_broadcast_filter" snd_seq_client_info_get_broadcast_filter) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_client_info_get_error_bounce" snd_seq_client_info_get_error_bounce) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_client_info_get_event_filter" snd_seq_client_info_get_event_filter) :pointer
  (info :pointer))

(cffi:defcfun ("snd_seq_client_info_get_num_ports" snd_seq_client_info_get_num_ports) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_client_info_get_event_lost" snd_seq_client_info_get_event_lost) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_client_info_set_client" snd_seq_client_info_set_client) :void
  (info :pointer)
  (client :int))

(cffi:defcfun ("snd_seq_client_info_set_name" snd_seq_client_info_set_name) :void
  (info :pointer)
  (name :string))

(cffi:defcfun ("snd_seq_client_info_set_broadcast_filter" snd_seq_client_info_set_broadcast_filter) :void
  (info :pointer)
  (val :int))

(cffi:defcfun ("snd_seq_client_info_set_error_bounce" snd_seq_client_info_set_error_bounce) :void
  (info :pointer)
  (val :int))

(cffi:defcfun ("snd_seq_client_info_set_event_filter" snd_seq_client_info_set_event_filter) :void
  (info :pointer)
  (filter :pointer))

(cffi:defcfun ("snd_seq_client_info_event_filter_clear" snd_seq_client_info_event_filter_clear) :void
  (info :pointer))

(cffi:defcfun ("snd_seq_client_info_event_filter_add" snd_seq_client_info_event_filter_add) :void
  (info :pointer)
  (event_type :int))

(cffi:defcfun ("snd_seq_client_info_event_filter_del" snd_seq_client_info_event_filter_del) :void
  (info :pointer)
  (event_type :int))

(cffi:defcfun ("snd_seq_client_info_event_filter_check" snd_seq_client_info_event_filter_check) :int
  (info :pointer)
  (event_type :int))

(cffi:defcfun ("snd_seq_get_client_info" snd_seq_get_client_info) :int
  (handle :pointer)
  (info :pointer))

(cffi:defcfun ("snd_seq_get_any_client_info" snd_seq_get_any_client_info) :int
  (handle :pointer)
  (client :int)
  (info :pointer))

(cffi:defcfun ("snd_seq_set_client_info" snd_seq_set_client_info) :int
  (handle :pointer)
  (info :pointer))

(cffi:defcfun ("snd_seq_query_next_client" snd_seq_query_next_client) :int
  (handle :pointer)
  (info :pointer))

(cffi:defcfun ("snd_seq_client_pool_sizeof" snd_seq_client_pool_sizeof) :pointer)

(cffi:defcfun ("snd_seq_client_pool_malloc" snd_seq_client_pool_malloc) :int
  (ptr :pointer))

(cffi:defcfun ("snd_seq_client_pool_free" snd_seq_client_pool_free) :void
  (ptr :pointer))

(cffi:defcfun ("snd_seq_client_pool_copy" snd_seq_client_pool_copy) :void
  (dst :pointer)
  (src :pointer))

(cffi:defcfun ("snd_seq_client_pool_get_client" snd_seq_client_pool_get_client) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_client_pool_get_output_pool" snd_seq_client_pool_get_output_pool) :pointer
  (info :pointer))

(cffi:defcfun ("snd_seq_client_pool_get_input_pool" snd_seq_client_pool_get_input_pool) :pointer
  (info :pointer))

(cffi:defcfun ("snd_seq_client_pool_get_output_room" snd_seq_client_pool_get_output_room) :pointer
  (info :pointer))

(cffi:defcfun ("snd_seq_client_pool_get_output_free" snd_seq_client_pool_get_output_free) :pointer
  (info :pointer))

(cffi:defcfun ("snd_seq_client_pool_get_input_free" snd_seq_client_pool_get_input_free) :pointer
  (info :pointer))

(cffi:defcfun ("snd_seq_client_pool_set_output_pool" snd_seq_client_pool_set_output_pool) :void
  (info :pointer)
  (size :pointer))

(cffi:defcfun ("snd_seq_client_pool_set_input_pool" snd_seq_client_pool_set_input_pool) :void
  (info :pointer)
  (size :pointer))

(cffi:defcfun ("snd_seq_client_pool_set_output_room" snd_seq_client_pool_set_output_room) :void
  (info :pointer)
  (size :pointer))

(cffi:defcfun ("snd_seq_get_client_pool" snd_seq_get_client_pool) :int
  (handle :pointer)
  (info :pointer))

(cffi:defcfun ("snd_seq_set_client_pool" snd_seq_set_client_pool) :int
  (handle :pointer)
  (info :pointer))

(cl:defconstant SND_SEQ_PORT_SYSTEM_TIMER 0)

(cl:defconstant SND_SEQ_PORT_SYSTEM_ANNOUNCE 1)

(cl:defconstant SND_SEQ_PORT_CAP_READ (cl:ash 1 0))

(cl:defconstant SND_SEQ_PORT_CAP_WRITE (cl:ash 1 1))

(cl:defconstant SND_SEQ_PORT_CAP_SYNC_READ (cl:ash 1 2))

(cl:defconstant SND_SEQ_PORT_CAP_SYNC_WRITE (cl:ash 1 3))

(cl:defconstant SND_SEQ_PORT_CAP_DUPLEX (cl:ash 1 4))

(cl:defconstant SND_SEQ_PORT_CAP_SUBS_READ (cl:ash 1 5))

(cl:defconstant SND_SEQ_PORT_CAP_SUBS_WRITE (cl:ash 1 6))

(cl:defconstant SND_SEQ_PORT_CAP_NO_EXPORT (cl:ash 1 7))

(cl:defconstant SND_SEQ_PORT_TYPE_SPECIFIC (cl:ash 1 0))

(cl:defconstant SND_SEQ_PORT_TYPE_MIDI_GENERIC (cl:ash 1 1))

(cl:defconstant SND_SEQ_PORT_TYPE_MIDI_GM (cl:ash 1 2))

(cl:defconstant SND_SEQ_PORT_TYPE_MIDI_GS (cl:ash 1 3))

(cl:defconstant SND_SEQ_PORT_TYPE_MIDI_XG (cl:ash 1 4))

(cl:defconstant SND_SEQ_PORT_TYPE_MIDI_MT32 (cl:ash 1 5))

(cl:defconstant SND_SEQ_PORT_TYPE_MIDI_GM2 (cl:ash 1 6))

(cl:defconstant SND_SEQ_PORT_TYPE_SYNTH (cl:ash 1 10))

(cl:defconstant SND_SEQ_PORT_TYPE_DIRECT_SAMPLE (cl:ash 1 11))

(cl:defconstant SND_SEQ_PORT_TYPE_SAMPLE (cl:ash 1 12))

(cl:defconstant SND_SEQ_PORT_TYPE_HARDWARE (cl:ash 1 16))

(cl:defconstant SND_SEQ_PORT_TYPE_SOFTWARE (cl:ash 1 17))

(cl:defconstant SND_SEQ_PORT_TYPE_SYNTHESIZER (cl:ash 1 18))

(cl:defconstant SND_SEQ_PORT_TYPE_PORT (cl:ash 1 19))

(cl:defconstant SND_SEQ_PORT_TYPE_APPLICATION (cl:ash 1 20))

(cffi:defcfun ("snd_seq_port_info_sizeof" snd_seq_port_info_sizeof) :pointer)

(cffi:defcfun ("snd_seq_port_info_malloc" snd_seq_port_info_malloc) :int
  (ptr :pointer))

(cffi:defcfun ("snd_seq_port_info_free" snd_seq_port_info_free) :void
  (ptr :pointer))

(cffi:defcfun ("snd_seq_port_info_copy" snd_seq_port_info_copy) :void
  (dst :pointer)
  (src :pointer))

(cffi:defcfun ("snd_seq_port_info_get_client" snd_seq_port_info_get_client) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_port_info_get_port" snd_seq_port_info_get_port) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_port_info_get_addr" snd_seq_port_info_get_addr) :pointer
  (info :pointer))

(cffi:defcfun ("snd_seq_port_info_get_name" snd_seq_port_info_get_name) :string
  (info :pointer))

(cffi:defcfun ("snd_seq_port_info_get_capability" snd_seq_port_info_get_capability) :unsigned-int
  (info :pointer))

(cffi:defcfun ("snd_seq_port_info_get_type" snd_seq_port_info_get_type) :unsigned-int
  (info :pointer))

(cffi:defcfun ("snd_seq_port_info_get_midi_channels" snd_seq_port_info_get_midi_channels) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_port_info_get_midi_voices" snd_seq_port_info_get_midi_voices) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_port_info_get_synth_voices" snd_seq_port_info_get_synth_voices) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_port_info_get_read_use" snd_seq_port_info_get_read_use) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_port_info_get_write_use" snd_seq_port_info_get_write_use) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_port_info_get_port_specified" snd_seq_port_info_get_port_specified) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_port_info_get_timestamping" snd_seq_port_info_get_timestamping) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_port_info_get_timestamp_real" snd_seq_port_info_get_timestamp_real) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_port_info_get_timestamp_queue" snd_seq_port_info_get_timestamp_queue) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_port_info_set_client" snd_seq_port_info_set_client) :void
  (info :pointer)
  (client :int))

(cffi:defcfun ("snd_seq_port_info_set_port" snd_seq_port_info_set_port) :void
  (info :pointer)
  (port :int))

(cffi:defcfun ("snd_seq_port_info_set_addr" snd_seq_port_info_set_addr) :void
  (info :pointer)
  (addr :pointer))

(cffi:defcfun ("snd_seq_port_info_set_name" snd_seq_port_info_set_name) :void
  (info :pointer)
  (name :string))

(cffi:defcfun ("snd_seq_port_info_set_capability" snd_seq_port_info_set_capability) :void
  (info :pointer)
  (capability :unsigned-int))

(cffi:defcfun ("snd_seq_port_info_set_type" snd_seq_port_info_set_type) :void
  (info :pointer)
  (type :unsigned-int))

(cffi:defcfun ("snd_seq_port_info_set_midi_channels" snd_seq_port_info_set_midi_channels) :void
  (info :pointer)
  (channels :int))

(cffi:defcfun ("snd_seq_port_info_set_midi_voices" snd_seq_port_info_set_midi_voices) :void
  (info :pointer)
  (voices :int))

(cffi:defcfun ("snd_seq_port_info_set_synth_voices" snd_seq_port_info_set_synth_voices) :void
  (info :pointer)
  (voices :int))

(cffi:defcfun ("snd_seq_port_info_set_port_specified" snd_seq_port_info_set_port_specified) :void
  (info :pointer)
  (val :int))

(cffi:defcfun ("snd_seq_port_info_set_timestamping" snd_seq_port_info_set_timestamping) :void
  (info :pointer)
  (enable :int))

(cffi:defcfun ("snd_seq_port_info_set_timestamp_real" snd_seq_port_info_set_timestamp_real) :void
  (info :pointer)
  (realtime :int))

(cffi:defcfun ("snd_seq_port_info_set_timestamp_queue" snd_seq_port_info_set_timestamp_queue) :void
  (info :pointer)
  (queue :int))

(cffi:defcfun ("snd_seq_create_port" snd_seq_create_port) :int
  (handle :pointer)
  (info :pointer))

(cffi:defcfun ("snd_seq_delete_port" snd_seq_delete_port) :int
  (handle :pointer)
  (port :int))

(cffi:defcfun ("snd_seq_get_port_info" snd_seq_get_port_info) :int
  (handle :pointer)
  (port :int)
  (info :pointer))

(cffi:defcfun ("snd_seq_get_any_port_info" snd_seq_get_any_port_info) :int
  (handle :pointer)
  (client :int)
  (port :int)
  (info :pointer))

(cffi:defcfun ("snd_seq_set_port_info" snd_seq_set_port_info) :int
  (handle :pointer)
  (port :int)
  (info :pointer))

(cffi:defcfun ("snd_seq_query_next_port" snd_seq_query_next_port) :int
  (handle :pointer)
  (info :pointer))

(cffi:defcfun ("snd_seq_port_subscribe_sizeof" snd_seq_port_subscribe_sizeof) :pointer)

(cffi:defcfun ("snd_seq_port_subscribe_malloc" snd_seq_port_subscribe_malloc) :int
  (ptr :pointer))

(cffi:defcfun ("snd_seq_port_subscribe_free" snd_seq_port_subscribe_free) :void
  (ptr :pointer))

(cffi:defcfun ("snd_seq_port_subscribe_copy" snd_seq_port_subscribe_copy) :void
  (dst :pointer)
  (src :pointer))

(cffi:defcfun ("snd_seq_port_subscribe_get_sender" snd_seq_port_subscribe_get_sender) :pointer
  (info :pointer))

(cffi:defcfun ("snd_seq_port_subscribe_get_dest" snd_seq_port_subscribe_get_dest) :pointer
  (info :pointer))

(cffi:defcfun ("snd_seq_port_subscribe_get_queue" snd_seq_port_subscribe_get_queue) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_port_subscribe_get_exclusive" snd_seq_port_subscribe_get_exclusive) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_port_subscribe_get_time_update" snd_seq_port_subscribe_get_time_update) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_port_subscribe_get_time_real" snd_seq_port_subscribe_get_time_real) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_port_subscribe_set_sender" snd_seq_port_subscribe_set_sender) :void
  (info :pointer)
  (addr :pointer))

(cffi:defcfun ("snd_seq_port_subscribe_set_dest" snd_seq_port_subscribe_set_dest) :void
  (info :pointer)
  (addr :pointer))

(cffi:defcfun ("snd_seq_port_subscribe_set_queue" snd_seq_port_subscribe_set_queue) :void
  (info :pointer)
  (q :int))

(cffi:defcfun ("snd_seq_port_subscribe_set_exclusive" snd_seq_port_subscribe_set_exclusive) :void
  (info :pointer)
  (val :int))

(cffi:defcfun ("snd_seq_port_subscribe_set_time_update" snd_seq_port_subscribe_set_time_update) :void
  (info :pointer)
  (val :int))

(cffi:defcfun ("snd_seq_port_subscribe_set_time_real" snd_seq_port_subscribe_set_time_real) :void
  (info :pointer)
  (val :int))

(cffi:defcfun ("snd_seq_get_port_subscription" snd_seq_get_port_subscription) :int
  (handle :pointer)
  (sub :pointer))

(cffi:defcfun ("snd_seq_subscribe_port" snd_seq_subscribe_port) :int
  (handle :pointer)
  (sub :pointer))

(cffi:defcfun ("snd_seq_unsubscribe_port" snd_seq_unsubscribe_port) :int
  (handle :pointer)
  (sub :pointer))

(cffi:defcenum snd_seq_query_subs_type_t
	:SND_SEQ_QUERY_SUBS_READ
	:SND_SEQ_QUERY_SUBS_WRITE)

(cffi:defcfun ("snd_seq_query_subscribe_sizeof" snd_seq_query_subscribe_sizeof) :pointer)

(cffi:defcfun ("snd_seq_query_subscribe_malloc" snd_seq_query_subscribe_malloc) :int
  (ptr :pointer))

(cffi:defcfun ("snd_seq_query_subscribe_free" snd_seq_query_subscribe_free) :void
  (ptr :pointer))

(cffi:defcfun ("snd_seq_query_subscribe_copy" snd_seq_query_subscribe_copy) :void
  (dst :pointer)
  (src :pointer))

(cffi:defcfun ("snd_seq_query_subscribe_get_client" snd_seq_query_subscribe_get_client) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_query_subscribe_get_port" snd_seq_query_subscribe_get_port) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_query_subscribe_get_root" snd_seq_query_subscribe_get_root) :pointer
  (info :pointer))

(cffi:defcfun ("snd_seq_query_subscribe_get_type" snd_seq_query_subscribe_get_type) snd_seq_query_subs_type_t
  (info :pointer))

(cffi:defcfun ("snd_seq_query_subscribe_get_index" snd_seq_query_subscribe_get_index) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_query_subscribe_get_num_subs" snd_seq_query_subscribe_get_num_subs) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_query_subscribe_get_addr" snd_seq_query_subscribe_get_addr) :pointer
  (info :pointer))

(cffi:defcfun ("snd_seq_query_subscribe_get_queue" snd_seq_query_subscribe_get_queue) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_query_subscribe_get_exclusive" snd_seq_query_subscribe_get_exclusive) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_query_subscribe_get_time_update" snd_seq_query_subscribe_get_time_update) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_query_subscribe_get_time_real" snd_seq_query_subscribe_get_time_real) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_query_subscribe_set_client" snd_seq_query_subscribe_set_client) :void
  (info :pointer)
  (client :int))

(cffi:defcfun ("snd_seq_query_subscribe_set_port" snd_seq_query_subscribe_set_port) :void
  (info :pointer)
  (port :int))

(cffi:defcfun ("snd_seq_query_subscribe_set_root" snd_seq_query_subscribe_set_root) :void
  (info :pointer)
  (addr :pointer))

(cffi:defcfun ("snd_seq_query_subscribe_set_type" snd_seq_query_subscribe_set_type) :void
  (info :pointer)
  (type snd_seq_query_subs_type_t))

(cffi:defcfun ("snd_seq_query_subscribe_set_index" snd_seq_query_subscribe_set_index) :void
  (info :pointer)
  (_index :int))

(cffi:defcfun ("snd_seq_query_port_subscribers" snd_seq_query_port_subscribers) :int
  (seq :pointer)
  (subs :pointer))

(cl:defconstant SND_SEQ_QUEUE_DIRECT 253)

(cffi:defcfun ("snd_seq_queue_info_sizeof" snd_seq_queue_info_sizeof) :pointer)

(cffi:defcfun ("snd_seq_queue_info_malloc" snd_seq_queue_info_malloc) :int
  (ptr :pointer))

(cffi:defcfun ("snd_seq_queue_info_free" snd_seq_queue_info_free) :void
  (ptr :pointer))

(cffi:defcfun ("snd_seq_queue_info_copy" snd_seq_queue_info_copy) :void
  (dst :pointer)
  (src :pointer))

(cffi:defcfun ("snd_seq_queue_info_get_queue" snd_seq_queue_info_get_queue) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_queue_info_get_name" snd_seq_queue_info_get_name) :string
  (info :pointer))

(cffi:defcfun ("snd_seq_queue_info_get_owner" snd_seq_queue_info_get_owner) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_queue_info_get_locked" snd_seq_queue_info_get_locked) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_queue_info_get_flags" snd_seq_queue_info_get_flags) :unsigned-int
  (info :pointer))

(cffi:defcfun ("snd_seq_queue_info_set_name" snd_seq_queue_info_set_name) :void
  (info :pointer)
  (name :string))

(cffi:defcfun ("snd_seq_queue_info_set_owner" snd_seq_queue_info_set_owner) :void
  (info :pointer)
  (owner :int))

(cffi:defcfun ("snd_seq_queue_info_set_locked" snd_seq_queue_info_set_locked) :void
  (info :pointer)
  (locked :int))

(cffi:defcfun ("snd_seq_queue_info_set_flags" snd_seq_queue_info_set_flags) :void
  (info :pointer)
  (flags :unsigned-int))

(cffi:defcfun ("snd_seq_create_queue" snd_seq_create_queue) :int
  (seq :pointer)
  (info :pointer))

(cffi:defcfun ("snd_seq_alloc_named_queue" snd_seq_alloc_named_queue) :int
  (seq :pointer)
  (name :string))

(cffi:defcfun ("snd_seq_alloc_queue" snd_seq_alloc_queue) :int
  (handle :pointer))

(cffi:defcfun ("snd_seq_free_queue" snd_seq_free_queue) :int
  (handle :pointer)
  (q :int))

(cffi:defcfun ("snd_seq_get_queue_info" snd_seq_get_queue_info) :int
  (seq :pointer)
  (q :int)
  (info :pointer))

(cffi:defcfun ("snd_seq_set_queue_info" snd_seq_set_queue_info) :int
  (seq :pointer)
  (q :int)
  (info :pointer))

(cffi:defcfun ("snd_seq_query_named_queue" snd_seq_query_named_queue) :int
  (seq :pointer)
  (name :string))

(cffi:defcfun ("snd_seq_get_queue_usage" snd_seq_get_queue_usage) :int
  (handle :pointer)
  (q :int))

(cffi:defcfun ("snd_seq_set_queue_usage" snd_seq_set_queue_usage) :int
  (handle :pointer)
  (q :int)
  (used :int))

(cffi:defcfun ("snd_seq_queue_status_sizeof" snd_seq_queue_status_sizeof) :pointer)

(cffi:defcfun ("snd_seq_queue_status_malloc" snd_seq_queue_status_malloc) :int
  (ptr :pointer))

(cffi:defcfun ("snd_seq_queue_status_free" snd_seq_queue_status_free) :void
  (ptr :pointer))

(cffi:defcfun ("snd_seq_queue_status_copy" snd_seq_queue_status_copy) :void
  (dst :pointer)
  (src :pointer))

(cffi:defcfun ("snd_seq_queue_status_get_queue" snd_seq_queue_status_get_queue) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_queue_status_get_events" snd_seq_queue_status_get_events) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_queue_status_get_tick_time" snd_seq_queue_status_get_tick_time) :unsigned-int
  (info :pointer))

(cffi:defcfun ("snd_seq_queue_status_get_real_time" snd_seq_queue_status_get_real_time) :pointer
  (info :pointer))

(cffi:defcfun ("snd_seq_queue_status_get_status" snd_seq_queue_status_get_status) :unsigned-int
  (info :pointer))

(cffi:defcfun ("snd_seq_get_queue_status" snd_seq_get_queue_status) :int
  (handle :pointer)
  (q :int)
  (status :pointer))

(cffi:defcfun ("snd_seq_queue_tempo_sizeof" snd_seq_queue_tempo_sizeof) :pointer)

(cffi:defcfun ("snd_seq_queue_tempo_malloc" snd_seq_queue_tempo_malloc) :int
  (ptr :pointer))

(cffi:defcfun ("snd_seq_queue_tempo_free" snd_seq_queue_tempo_free) :void
  (ptr :pointer))

(cffi:defcfun ("snd_seq_queue_tempo_copy" snd_seq_queue_tempo_copy) :void
  (dst :pointer)
  (src :pointer))

(cffi:defcfun ("snd_seq_queue_tempo_get_queue" snd_seq_queue_tempo_get_queue) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_queue_tempo_get_tempo" snd_seq_queue_tempo_get_tempo) :unsigned-int
  (info :pointer))

(cffi:defcfun ("snd_seq_queue_tempo_get_ppq" snd_seq_queue_tempo_get_ppq) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_queue_tempo_get_skew" snd_seq_queue_tempo_get_skew) :unsigned-int
  (info :pointer))

(cffi:defcfun ("snd_seq_queue_tempo_get_skew_base" snd_seq_queue_tempo_get_skew_base) :unsigned-int
  (info :pointer))

(cffi:defcfun ("snd_seq_queue_tempo_set_tempo" snd_seq_queue_tempo_set_tempo) :void
  (info :pointer)
  (tempo :unsigned-int))

(cffi:defcfun ("snd_seq_queue_tempo_set_ppq" snd_seq_queue_tempo_set_ppq) :void
  (info :pointer)
  (ppq :int))

(cffi:defcfun ("snd_seq_queue_tempo_set_skew" snd_seq_queue_tempo_set_skew) :void
  (info :pointer)
  (skew :unsigned-int))

(cffi:defcfun ("snd_seq_queue_tempo_set_skew_base" snd_seq_queue_tempo_set_skew_base) :void
  (info :pointer)
  (base :unsigned-int))

(cffi:defcfun ("snd_seq_get_queue_tempo" snd_seq_get_queue_tempo) :int
  (handle :pointer)
  (q :int)
  (tempo :pointer))

(cffi:defcfun ("snd_seq_set_queue_tempo" snd_seq_set_queue_tempo) :int
  (handle :pointer)
  (q :int)
  (tempo :pointer))

(cffi:defcenum snd_seq_queue_timer_type_t
	(:SND_SEQ_TIMER_ALSA #.0)
	(:SND_SEQ_TIMER_MIDI_CLOCK #.1)
	(:SND_SEQ_TIMER_MIDI_TICK #.2))

(cffi:defcfun ("snd_seq_queue_timer_sizeof" snd_seq_queue_timer_sizeof) :pointer)

(cffi:defcfun ("snd_seq_queue_timer_malloc" snd_seq_queue_timer_malloc) :int
  (ptr :pointer))

(cffi:defcfun ("snd_seq_queue_timer_free" snd_seq_queue_timer_free) :void
  (ptr :pointer))

(cffi:defcfun ("snd_seq_queue_timer_copy" snd_seq_queue_timer_copy) :void
  (dst :pointer)
  (src :pointer))

(cffi:defcfun ("snd_seq_queue_timer_get_queue" snd_seq_queue_timer_get_queue) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_queue_timer_get_type" snd_seq_queue_timer_get_type) snd_seq_queue_timer_type_t
  (info :pointer))

(cffi:defcfun ("snd_seq_queue_timer_get_id" snd_seq_queue_timer_get_id) :pointer
  (info :pointer))

(cffi:defcfun ("snd_seq_queue_timer_get_resolution" snd_seq_queue_timer_get_resolution) :unsigned-int
  (info :pointer))

(cffi:defcfun ("snd_seq_queue_timer_set_type" snd_seq_queue_timer_set_type) :void
  (info :pointer)
  (type snd_seq_queue_timer_type_t))

(cffi:defcfun ("snd_seq_queue_timer_set_id" snd_seq_queue_timer_set_id) :void
  (info :pointer)
  (id :pointer))

(cffi:defcfun ("snd_seq_queue_timer_set_resolution" snd_seq_queue_timer_set_resolution) :void
  (info :pointer)
  (resolution :unsigned-int))

(cffi:defcfun ("snd_seq_get_queue_timer" snd_seq_get_queue_timer) :int
  (handle :pointer)
  (q :int)
  (timer :pointer))

(cffi:defcfun ("snd_seq_set_queue_timer" snd_seq_set_queue_timer) :int
  (handle :pointer)
  (q :int)
  (timer :pointer))

(cffi:defcfun ("snd_seq_free_event" snd_seq_free_event) :int
  (ev :pointer))

(cffi:defcfun ("snd_seq_event_length" snd_seq_event_length) :pointer
  (ev :pointer))

(cffi:defcfun ("snd_seq_event_output" snd_seq_event_output) :int
  (handle :pointer)
  (ev :pointer))

(cffi:defcfun ("snd_seq_event_output_buffer" snd_seq_event_output_buffer) :int
  (handle :pointer)
  (ev :pointer))

(cffi:defcfun ("snd_seq_event_output_direct" snd_seq_event_output_direct) :int
  (handle :pointer)
  (ev :pointer))

(cffi:defcfun ("snd_seq_event_input" snd_seq_event_input) :int
  (handle :pointer)
  (ev :pointer))

(cffi:defcfun ("snd_seq_event_input_pending" snd_seq_event_input_pending) :int
  (seq :pointer)
  (fetch_sequencer :int))

(cffi:defcfun ("snd_seq_drain_output" snd_seq_drain_output) :int
  (handle :pointer))

(cffi:defcfun ("snd_seq_event_output_pending" snd_seq_event_output_pending) :int
  (seq :pointer))

(cffi:defcfun ("snd_seq_extract_output" snd_seq_extract_output) :int
  (handle :pointer)
  (ev :pointer))

(cffi:defcfun ("snd_seq_drop_output" snd_seq_drop_output) :int
  (handle :pointer))

(cffi:defcfun ("snd_seq_drop_output_buffer" snd_seq_drop_output_buffer) :int
  (handle :pointer))

(cffi:defcfun ("snd_seq_drop_input" snd_seq_drop_input) :int
  (handle :pointer))

(cffi:defcfun ("snd_seq_drop_input_buffer" snd_seq_drop_input_buffer) :int
  (handle :pointer))

(cl:defconstant SND_SEQ_REMOVE_INPUT (cl:ash 1 0))

(cl:defconstant SND_SEQ_REMOVE_OUTPUT (cl:ash 1 1))

(cl:defconstant SND_SEQ_REMOVE_DEST (cl:ash 1 2))

(cl:defconstant SND_SEQ_REMOVE_DEST_CHANNEL (cl:ash 1 3))

(cl:defconstant SND_SEQ_REMOVE_TIME_BEFORE (cl:ash 1 4))

(cl:defconstant SND_SEQ_REMOVE_TIME_AFTER (cl:ash 1 5))

(cl:defconstant SND_SEQ_REMOVE_TIME_TICK (cl:ash 1 6))

(cl:defconstant SND_SEQ_REMOVE_EVENT_TYPE (cl:ash 1 7))

(cl:defconstant SND_SEQ_REMOVE_IGNORE_OFF (cl:ash 1 8))

(cl:defconstant SND_SEQ_REMOVE_TAG_MATCH (cl:ash 1 9))

(cffi:defcfun ("snd_seq_remove_events_sizeof" snd_seq_remove_events_sizeof) :pointer)

(cffi:defcfun ("snd_seq_remove_events_malloc" snd_seq_remove_events_malloc) :int
  (ptr :pointer))

(cffi:defcfun ("snd_seq_remove_events_free" snd_seq_remove_events_free) :void
  (ptr :pointer))

(cffi:defcfun ("snd_seq_remove_events_copy" snd_seq_remove_events_copy) :void
  (dst :pointer)
  (src :pointer))

(cffi:defcfun ("snd_seq_remove_events_get_condition" snd_seq_remove_events_get_condition) :unsigned-int
  (info :pointer))

(cffi:defcfun ("snd_seq_remove_events_get_queue" snd_seq_remove_events_get_queue) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_remove_events_get_time" snd_seq_remove_events_get_time) :pointer
  (info :pointer))

(cffi:defcfun ("snd_seq_remove_events_get_dest" snd_seq_remove_events_get_dest) :pointer
  (info :pointer))

(cffi:defcfun ("snd_seq_remove_events_get_channel" snd_seq_remove_events_get_channel) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_remove_events_get_event_type" snd_seq_remove_events_get_event_type) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_remove_events_get_tag" snd_seq_remove_events_get_tag) :int
  (info :pointer))

(cffi:defcfun ("snd_seq_remove_events_set_condition" snd_seq_remove_events_set_condition) :void
  (info :pointer)
  (flags :unsigned-int))

(cffi:defcfun ("snd_seq_remove_events_set_queue" snd_seq_remove_events_set_queue) :void
  (info :pointer)
  (queue :int))

(cffi:defcfun ("snd_seq_remove_events_set_time" snd_seq_remove_events_set_time) :void
  (info :pointer)
  (time :pointer))

(cffi:defcfun ("snd_seq_remove_events_set_dest" snd_seq_remove_events_set_dest) :void
  (info :pointer)
  (addr :pointer))

(cffi:defcfun ("snd_seq_remove_events_set_channel" snd_seq_remove_events_set_channel) :void
  (info :pointer)
  (channel :int))

(cffi:defcfun ("snd_seq_remove_events_set_event_type" snd_seq_remove_events_set_event_type) :void
  (info :pointer)
  (type :int))

(cffi:defcfun ("snd_seq_remove_events_set_tag" snd_seq_remove_events_set_tag) :void
  (info :pointer)
  (tag :int))

(cffi:defcfun ("snd_seq_remove_events" snd_seq_remove_events) :int
  (handle :pointer)
  (info :pointer))

(cffi:defcfun ("snd_seq_set_bit" snd_seq_set_bit) :void
  (nr :int)
  (array :pointer))

(cffi:defcfun ("snd_seq_unset_bit" snd_seq_unset_bit) :void
  (nr :int)
  (array :pointer))

(cffi:defcfun ("snd_seq_change_bit" snd_seq_change_bit) :int
  (nr :int)
  (array :pointer))

(cffi:defcfun ("snd_seq_get_bit" snd_seq_get_bit) :int
  (nr :int)
  (array :pointer))

;; (defanonenum
;; 	SND_SEQ_EVFLG_RESULT
;; 	SND_SEQ_EVFLG_NOTE
;; 	SND_SEQ_EVFLG_CONTROL
;; 	SND_SEQ_EVFLG_QUEUE
;; 	SND_SEQ_EVFLG_SYSTEM
;; 	SND_SEQ_EVFLG_MESSAGE
;; 	SND_SEQ_EVFLG_CONNECTION
;; 	SND_SEQ_EVFLG_SAMPLE
;; 	SND_SEQ_EVFLG_USERS
;; 	SND_SEQ_EVFLG_INSTR
;; 	SND_SEQ_EVFLG_QUOTE
;; 	SND_SEQ_EVFLG_NONE
;; 	SND_SEQ_EVFLG_RAW
;; 	SND_SEQ_EVFLG_FIXED
;; 	SND_SEQ_EVFLG_VARIABLE
;; 	SND_SEQ_EVFLG_VARUSR)

;; (defanonenum
;; 	SND_SEQ_EVFLG_NOTE_ONEARG
;; 	SND_SEQ_EVFLG_NOTE_TWOARG)

;; (defanonenum
;; 	SND_SEQ_EVFLG_QUEUE_NOARG
;; 	SND_SEQ_EVFLG_QUEUE_TICK
;; 	SND_SEQ_EVFLG_QUEUE_TIME
;; 	SND_SEQ_EVFLG_QUEUE_VALUE)

(cffi:defcvar ("snd_seq_event_types" snd_seq_event_types)
 :pointer)

(cffi:defcfun ("snd_seq_control_queue" snd_seq_control_queue) :int
  (seq :pointer)
  (q :int)
  (type :int)
  (value :int)
  (ev :pointer))

(cffi:defcfun ("snd_seq_create_simple_port" snd_seq_create_simple_port) :int
  (seq :pointer)
  (name :string)
  (caps :unsigned-int)
  (type :unsigned-int))

(cffi:defcfun ("snd_seq_delete_simple_port" snd_seq_delete_simple_port) :int
  (seq :pointer)
  (port :int))

(cffi:defcfun ("snd_seq_connect_from" snd_seq_connect_from) :int
  (seq :pointer)
  (my_port :int)
  (src_client :int)
  (src_port :int))

(cffi:defcfun ("snd_seq_connect_to" snd_seq_connect_to) :int
  (seq :pointer)
  (my_port :int)
  (dest_client :int)
  (dest_port :int))

(cffi:defcfun ("snd_seq_disconnect_from" snd_seq_disconnect_from) :int
  (seq :pointer)
  (my_port :int)
  (src_client :int)
  (src_port :int))

(cffi:defcfun ("snd_seq_disconnect_to" snd_seq_disconnect_to) :int
  (seq :pointer)
  (my_port :int)
  (dest_client :int)
  (dest_port :int))

(cffi:defcfun ("snd_seq_set_client_name" snd_seq_set_client_name) :int
  (seq :pointer)
  (name :string))

(cffi:defcfun ("snd_seq_set_client_event_filter" snd_seq_set_client_event_filter) :int
  (seq :pointer)
  (event_type :int))

(cffi:defcfun ("snd_seq_set_client_pool_output" snd_seq_set_client_pool_output) :int
  (seq :pointer)
  (size :pointer))

(cffi:defcfun ("snd_seq_set_client_pool_output_room" snd_seq_set_client_pool_output_room) :int
  (seq :pointer)
  (size :pointer))

(cffi:defcfun ("snd_seq_set_client_pool_input" snd_seq_set_client_pool_input) :int
  (seq :pointer)
  (size :pointer))

(cffi:defcfun ("snd_seq_sync_output_queue" snd_seq_sync_output_queue) :int
  (seq :pointer))

(cffi:defcfun ("snd_seq_parse_address" snd_seq_parse_address) :int
  (seq :pointer)
  (addr :pointer)
  (str :string))

(cffi:defcfun ("snd_seq_reset_pool_output" snd_seq_reset_pool_output) :int
  (seq :pointer))

(cffi:defcfun ("snd_seq_reset_pool_input" snd_seq_reset_pool_input) :int
  (seq :pointer))

(cffi:defcfun ("snd_midi_event_new" snd_midi_event_new) :int
  (bufsize :pointer)
  (rdev :pointer))

(cffi:defcfun ("snd_midi_event_resize_buffer" snd_midi_event_resize_buffer) :int
  (dev :pointer)
  (bufsize :pointer))

(cffi:defcfun ("snd_midi_event_free" snd_midi_event_free) :void
  (dev :pointer))

(cffi:defcfun ("snd_midi_event_init" snd_midi_event_init) :void
  (dev :pointer))

(cffi:defcfun ("snd_midi_event_reset_encode" snd_midi_event_reset_encode) :void
  (dev :pointer))

(cffi:defcfun ("snd_midi_event_reset_decode" snd_midi_event_reset_decode) :void
  (dev :pointer))

(cffi:defcfun ("snd_midi_event_no_status" snd_midi_event_no_status) :void
  (dev :pointer)
  (on :int))

(cffi:defcfun ("snd_midi_event_encode" snd_midi_event_encode) :long
  (dev :pointer)
  (buf :pointer)
  (count :long)
  (ev :pointer))

(cffi:defcfun ("snd_midi_event_encode_byte" snd_midi_event_encode_byte) :int
  (dev :pointer)
  (c :int)
  (ev :pointer))

(cffi:defcfun ("snd_midi_event_decode" snd_midi_event_decode) :long
  (dev :pointer)
  (buf :pointer)
  (count :long)
  (ev :pointer))
