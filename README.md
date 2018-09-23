# CL-ALSASEQ

This is cl-alsaseq, a Common Lisp library for accessing the ALSA MIDI
interface. With it, you can send MIDI messages to any program or device that is
supported by ALSA.


## Example usage

```lisp
  (ql:quickload :cl-alsaseq) ;; Load the system.

  (midihelper:start-midihelper) ;; Start the ALSA MIDI client.

  ;; ...Then connect the "CL" ALSA MIDI source to the destination of your choice.
  ;; I usually use Qjackctl to manage MIDI connections.

  (defparameter *midi-channel* 0)

  ;; Send a MIDI note on event. 69 is the note number, 127 is the velocity
  (midihelper:send-event (midihelper:ev-noteon *midi-channel* 69 127))

  ;; Send a MIDI note off to stop the previous note.
  (midihelper:send-event (midihelper:ev-noteoff *midi-channel* 69 127))

  ;; Send a program change message to switch to program #2.
  (midihelper:send-event (midihelper:ev-pgmchange *midi-channel* 2))
```

Note: The example is from [modula-t][modula-t]'s [repository][modula-t-repo].


[modula-t]: https://github.com/defaultxr
[modula-t-repo]: https://github.com/defaultxr/cl-alsaseq
