#include <stdio.h>
#include <unistd.h>
#include <signal.h>

static int signum;
static void sighandler1() {signum = 1; signal(SIGUSR2, SIG_DFL);}
static void sighandler2() {signum = 2; signal(SIGUSR1, SIG_DFL);}

static int waitfor = 0;
void sigset_() {waitfor = 1;}
int  sigref_() {return waitfor;}

void sgwait_(int *num)
{
  signum = 0;
  if (waitfor) {
    printf("PAUSE: "); fflush(stdout);
    while (!signum) {
      signal(SIGUSR1, sighandler1);
      signal(SIGUSR2, sighandler2);
      pause();
    }
  }
  *num = signum;
}
