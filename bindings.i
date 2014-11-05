%module bindings

/* %feature("intern_function", "lispify"); */

%insert("lisphead") %{
(in-package :cl-alsaseq)
%}

/* %{ */
/* #include <stdio.h> */
/* #include "stdlib.h" */
/* #include "string.h" */
/* #include "signal.h" */
/* #include "getopt.h" */
/* #include "sys/poll.h" */
/* #include "alsa/asoundlib.h" */
/* #include "aconfig.h" */
/* #include "version.h" */
/*   %} */

%include "stdint.i"
/* %include "stl.i" */
/* %include "cstring.i" */
/* %include "signal.h" */
/* %include "getopt.h" */
%include "/usr/include/poll.h"
%include "/usr/include/alsa/mixer.h"
/* %include "/usr/include/alsa/seq_event.h" */
%include "/usr/include/alsa/seq.h"
%include "/usr/include/alsa/mixer.h"
%include "/usr/include/alsa/seqmid.h"
%include "/usr/include/alsa/seq_midi_event.h"
%include "/home/rick/midi-loops/alsa-utils-1.0.9/include/aconfig.h"
%include "/home/rick/midi-loops/alsa-utils-1.0.9/include/version.h"

/* typedef unsigned int size_t; */

/* %include "/home/rick/git_checkouts/nanomsg/src/nn.h" */
/* %include "/home/rick/git_checkouts/nanomsg/src/bus.h" */
/* %include "/home/rick/git_checkouts/nanomsg/src/inproc.h" */
/* %include "/home/rick/git_checkouts/nanomsg/src/ipc.h" */
/* %include "/home/rick/git_checkouts/nanomsg/src/pair.h" */
/* %include "/home/rick/git_checkouts/nanomsg/src/pipeline.h" */
/* %include "/home/rick/git_checkouts/nanomsg/src/pubsub.h" */
/* %include "/home/rick/git_checkouts/nanomsg/src/reqrep.h" */
/* %include "/home/rick/git_checkouts/nanomsg/src/survey.h" */
/* %include "/home/rick/git_checkouts/nanomsg/src/tcp.h" */

