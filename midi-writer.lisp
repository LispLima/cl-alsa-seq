(in-package :cl-alsaseq)

(defun send-event (description seq port)
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

(defun parse-echo (seq port)
  (send-event (recv seq) seq port))
