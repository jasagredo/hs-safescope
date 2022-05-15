#define _GNU_SOURCE
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include "Rts.h"
#include "Trace.h"
#include "Capability.h"

void __cyg_profile_func_enter(void *this_fn, void *call_site) __attribute__((no_instrument_function));
void __cyg_profile_func_exit(void *this_fn, void *call_site)  __attribute__((no_instrument_function));

void bar_c(int num) {
  printf("Received %d\n", num);
  sleep(2);
}

void __cyg_profile_func_enter ( void *this_fn, void *call_site ) {
  pid_t tid = gettid();
  ssize_t bufsz = snprintf(NULL, 0, "ANN_CALL_TO %p %p %d", this_fn, call_site, tid);
  char* buf = malloc(bufsz + 1);
  snprintf(buf, bufsz + 1, "ANN_CALL_TO %p %p %d", this_fn, call_site, tid);

  // there is no Haskell capability running this, so just assign it to the MainCapability
  traceUserMsg(&MainCapability, buf);

  free(buf);
}

void __cyg_profile_func_exit ( void *this_fn, void *call_site ) {
  pid_t tid = gettid();
  ssize_t bufsz = snprintf(NULL, 0, "ANN_CALL_RET %p %p %d", this_fn, call_site, tid);
  char* buf = malloc(bufsz + 1);
  snprintf(buf, bufsz + 1, "ANN_CALL_RET %p %p %d", this_fn, call_site, tid);

  // there is no Haskell capability running this, so just assign it to the MainCapability
  traceUserMsg(&MainCapability, buf);

  free(buf);
}
