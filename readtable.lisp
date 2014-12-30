(in-package :midiloops)

(defparameter +quneo-chan+ 15)
(defparameter +quneo-led-chan+ 0)

(defun quneo-map-event (in-event)
  (match in-event
    ;;Transport buttons
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
     (list (ev-loop-erase)))
    ;;TODO make 'rec' button do something

    ;;TODO add ev-loop-group to midiloops program
    ;; odd numbers are loop groups,
    ;; even numbers are loops in group
    ((plist :EVENT-TYPE :SND_SEQ_EVENT_NOTEON
            :EVENT-DATA (plist VELOCITY _
                               NOTE 12
                               CHANNEL +quneo-chan+))
     (list (ev-active-loop 1)))
    ((plist :EVENT-TYPE :SND_SEQ_EVENT_NOTEON
            :EVENT-DATA (plist VELOCITY _
                               NOTE 11
                               CHANNEL +quneo-chan+))
     (list (ev-loop-group 1)))
    ((plist :EVENT-TYPE :SND_SEQ_EVENT_NOTEON
            :EVENT-DATA (plist VELOCITY _
                              NOTE 14
                              CHANNEL +quneo-chan+))
     (list (ev-active-loop 2)))
    ((plist :EVENT-TYPE :SND_SEQ_EVENT_NOTEON
            :EVENT-DATA (plist VELOCITY _
                              NOTE 13
                              CHANNEL +quneo-chan+))
     (list (ev-loop-group 2)))
    ((plist :EVENT-TYPE :SND_SEQ_EVENT_NOTEON
            :EVENT-DATA (plist VELOCITY _
                              NOTE 16
                              CHANNEL +quneo-chan+))
     (list (ev-active-loop 3)))
    ((plist :EVENT-TYPE :SND_SEQ_EVENT_NOTEON
            :EVENT-DATA (plist VELOCITY _
                              NOTE 15
                              CHANNEL +quneo-chan+))
     (list (ev-loop-group 3)))
    ((plist :EVENT-TYPE :SND_SEQ_EVENT_NOTEON
            :EVENT-DATA (plist VELOCITY _
                              NOTE 18
                              CHANNEL +quneo-chan+))
     (list (ev-active-loop 4)))
    ((plist :EVENT-TYPE :SND_SEQ_EVENT_NOTEON
            :EVENT-DATA (plist VELOCITY _
                              NOTE 17
                              CHANNEL +quneo-chan+))
     (list (ev-loop-group 4)))

    ;;toggle metronome
    ((plist :EVENT-TYPE :SND_SEQ_EVENT_NOTEON
            :EVENT-DATA (plist VELOCITY vel
                              NOTE 19
                              CHANNEL +quneo-chan+))
     (print (list (ev-toggle-metronome))))

    ;;Left hand big circle for overdub toggle
    ((plist :EVENT-TYPE :SND_SEQ_EVENT_NOTEON
            :EVENT-DATA (plist VELOCITY _
                              NOTE 4
                              CHANNEL +quneo-chan+))
     (list (ev-loop-overdub)))
    ;;Right hand big circle for usual loop-cycle
    ((plist :EVENT-TYPE :SND_SEQ_EVENT_NOTEON
            :EVENT-DATA (plist VELOCITY _
                              NOTE 5
                              CHANNEL +quneo-chan+))
     (list (ev-loop-cycle)))
    (_ (list in-event))))

(defun quneo-reader (in-events)
  (apply #'append
         (mapcar #'quneo-map-event
                 in-events)));;monoidal space for in-events list-list
