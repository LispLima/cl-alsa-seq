#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <signal.h>
#include <sys/poll.h>
#include <alsa/asoundlib.h>
static snd_seq_t *seq;
static int port_count;
static volatile sig_atomic_t stop = 0;

static void init_seq(void)
{
  snd_seq_open(&seq, "default", SND_SEQ_OPEN_DUPLEX, 0);
  snd_seq_set_client_name(seq, "aseqdump");
}

int create_port(void)
{
  return snd_seq_create_simple_port(seq, "aseqdump",
                                    SND_SEQ_PORT_CAP_WRITE |
                                    SND_SEQ_PORT_CAP_SUBS_WRITE |
                                    SND_SEQ_PORT_CAP_READ |
                                    SND_SEQ_PORT_CAP_SUBS_READ,
                                    SND_SEQ_PORT_TYPE_MIDI_GENERIC |
                                    SND_SEQ_PORT_TYPE_APPLICATION);
}

int main(int argc, char *argv[])
{
  struct pollfd *pfds;
  int npfds;
  int err;

  init_seq();

  int my_port = create_port();

  err = snd_seq_nonblock(seq, 1);
	
  if (port_count > 0)
    printf("Waiting for data.");
  else
    printf("Waiting for data at port %d:0.",
           snd_seq_client_id(seq));
  printf(" Press Ctrl+C to end.\n");
  printf("Source_ Event_________________ Ch _Data__\n");
	

  npfds = snd_seq_poll_descriptors_count(seq, POLLIN);
  pfds = alloca(sizeof(*pfds) * npfds);
  for (;;) {
    snd_seq_poll_descriptors(seq, pfds, npfds, POLLIN);
    if (poll(pfds, npfds, 69) < 0)
      break;
    do {
      snd_seq_event_t *event;
      err = snd_seq_event_input(seq, &event);
      if (err < 0) {
        break;
      }
      if (event) {
        snd_seq_ev_set_source(event, my_port);
        snd_seq_ev_set_subs(event);
        snd_seq_ev_set_direct(event);
        snd_seq_event_output(seq, event);
        snd_seq_drain_output(seq);
      }
    } while (err > 0);
  }

  snd_seq_close(seq);
  return 0;
}
