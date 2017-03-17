/**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <math.h>
#include <time.h>
#include <stdint.h>

#if defined(__unix__)
#include <sys/time.h>
#elif defined(_WIN32)
#include <sys/timeb.h>
#endif

CAMLprim value c_now() {
#if defined(__unix__)
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return caml_copy_double(floor(tv.tv_sec * 1000.0 + tv.tv_usec / 1000.0));
#elif defined(_WIN32)
  struct _timeb tv;
  _ftime(&tv);
  return caml_copy_double(tv.time * 1000.0 + tv.millitm);
#else
  return caml_copy_double(time(NULL) * 1000.0);
#endif
}

CAMLprim value c_locoff() {
  time_t t = time(NULL);
  struct tm gmt;
  struct tm lt;
  gmtime_r(&t, &gmt);
  localtime_r(&t, &lt);
  time_t utc = mktime(&gmt);
  time_t loc = mktime(&lt);
  return caml_copy_double((loc - utc) * 1000);
}

/* returns -1.0 on error */
CAMLprim value c_tmoff(value unixtime) {
  /* unixtime in range [0..100,000,000] days after unix time epoch */
  double uxtime = Double_val(unixtime) / 1000.0;
  if (sizeof(time_t) == 4 && uxtime > UINT32_MAX) { // 32 bit
    return caml_copy_double(-1.0);
  } // otherwise 64 bits is enough to hold any of these days
  time_t t = (time_t) uxtime;
  struct tm gmt;
  struct tm lt;
  gmtime_r(&t, &gmt);
  localtime_r(&t, &lt);
  time_t utc = mktime(&gmt);
  time_t loc = mktime(&lt);
  return caml_copy_double((loc - utc) * 1000);
}
