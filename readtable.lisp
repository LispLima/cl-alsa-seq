(in-package :midiloops)

(defparameter +quneo-chan+ 15)
(defparameter +quneo-led-chan+ 0)

(defun quneo-map-event (in-event)
  (match in-event
    ((plist :EVENT-TYPE :SND_SEQ_EVENT_NOTEON
            :EVENT-DATA (plist VELOCITY (not 0)
                               NOTE 26
                               CHANNEL +quneo-chan+))
     (list (ev-loop-play)))
    ((plist :EVENT-TYPE :SND_SEQ_EVENT_NOTEON
            :EVENT-DATA (plist VELOCITY (not 0)
                               NOTE 25
                               CHANNEL +quneo-chan+))
     (list (ev-loop-stop)))
    ((plist :EVENT-TYPE :SND_SEQ_EVENT_NOTEON
            :EVENT-DATA (plist VELOCITY (not 0)
                               NOTE 24
                               CHANNEL +quneo-chan+))
     (list (ev-loop-cycle)))
    ((plist :EVENT-TYPE :SND_SEQ_EVENT_NOTEON
            :EVENT-DATA (plist VELOCITY _
                               NOTE 11
                               CHANNEL +quneo-chan+))
     (list (ev-active-loop 1)))
    ((plist :EVENT-TYPE :SND_SEQ_EVENT_NOTEON
            :EVENT-DATA (plist VELOCITY _
                              NOTE 13
                              CHANNEL +quneo-chan+))
     (list (ev-active-loop 2)))
    ((plist :EVENT-TYPE :SND_SEQ_EVENT_NOTEON
            :EVENT-DATA (plist VELOCITY _
                              NOTE 15
                              CHANNEL +quneo-chan+))
     (list (ev-active-loop 3)))
    ((plist :EVENT-TYPE :SND_SEQ_EVENT_NOTEON
            :EVENT-DATA (plist VELOCITY _
                              NOTE 17
                              CHANNEL +quneo-chan+))
     (list (ev-active-loop 4)))
    ((plist :EVENT-TYPE :SND_SEQ_EVENT_NOTEON
            :EVENT-DATA (plist VELOCITY _
                              NOTE 12
                              CHANNEL +quneo-chan+))
     (list (ev-loop-erase 1)))
    ((plist :EVENT-TYPE :SND_SEQ_EVENT_NOTEON
            :EVENT-DATA (plist VELOCITY _
                              NOTE 14
                              CHANNEL +quneo-chan+))
     (list (ev-loop-erase 2)))
    ((plist :EVENT-TYPE :SND_SEQ_EVENT_NOTEON
            :EVENT-DATA (plist VELOCITY _
                              NOTE 16
                              CHANNEL +quneo-chan+))
     (list (ev-loop-erase 3)))
    ((plist :EVENT-TYPE :SND_SEQ_EVENT_NOTEON
            :EVENT-DATA (plist VELOCITY _
                              NOTE 18
                              CHANNEL +quneo-chan+))
     (list (ev-loop-erase 4)))
    ((plist :EVENT-TYPE :SND_SEQ_EVENT_NOTEON
            :EVENT-DATA (plist VELOCITY _
                              NOTE 106
                              CHANNEL +quneo-chan+))
     (list (ev-loop-overdub)))
    ;; ((plist :EVENT-TYPE :SND_SEQ_EVENT_NOTEON
    ;;          :EVENT-DATA (plist VELOCITY _
    ;;                            NOTE 107
    ;;                            CHANNEL +quneo-chan+))
    ;;   (list (ev-loop-overwrite)))
    ((plist :EVENT-TYPE :SND_SEQ_EVENT_NOTEON
            :EVENT-DATA (plist VELOCITY _
                              NOTE 108
                              CHANNEL +quneo-chan+))
     (list (ev-loop-push-extend)))
    ((plist :EVENT-TYPE :SND_SEQ_EVENT_NOTEON
            :EVENT-DATA (plist VELOCITY _
                              NOTE 109
                              CHANNEL +quneo-chan+))
     (list (ev-loop-continue)))
    (_ (list in-event))))

(defun quneo-reader (in-events)
  (apply #'append
         (mapcar #'quneo-map-event
                 in-events)));;monoidal space for in-events list-list
