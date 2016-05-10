/*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 */

#include "ctypes_primitives.h"

#define _XOPEN_SOURCE 500
#include <caml/mlvalues.h>

#include <assert.h>

#include <sys/types.h>
#include <unistd.h>
#include <signal.h>
#if (!defined _WIN32 || defined __CYGWIN__) && !defined MINIOS
#include <pthread.h>
#endif
#include <time.h>

#include <stdint.h>

#define EXPOSE_TYPEINFO_COMMON(TYPENAME,STYPENAME)           \
  value posix_types_typeof_ ## TYPENAME(value unit)               \
  {                                                          \
    enum ctypes_arithmetic_type underlying =                 \
      CTYPES_CLASSIFY_ARITHMETIC_TYPE(STYPENAME);            \
    return Val_int(underlying);                              \
  }

#define EXPOSE_ALIGNMENT_COMMON(TYPENAME,STYPENAME)          \
  value posix_types_alignmentof_ ## TYPENAME(value unit)          \
  {                                                          \
    struct s { char c; STYPENAME t; };                       \
    return Val_int(offsetof(struct s, t));                   \
  }

#define EXPOSE_TYPESIZE_COMMON(TYPENAME,STYPENAME)           \
  value posix_types_sizeof_ ## TYPENAME(value unit)               \
  {                                                          \
    return Val_int(sizeof(STYPENAME));                       \
  }

#if !defined _WIN32 || defined __CYGWIN__
  #define UNDERSCORE(X) X
#else
  #define UNDERSCORE(X) _## X
#endif

#define EXPOSE_TYPEINFO(X) EXPOSE_TYPEINFO_COMMON(X, X)
#define EXPOSE_TYPEINFO_S(X) EXPOSE_TYPEINFO_COMMON(X, UNDERSCORE(X))
#define EXPOSE_TYPESIZE(X) EXPOSE_TYPESIZE_COMMON(X, X)
#define EXPOSE_TYPESIZE_S(X) EXPOSE_TYPESIZE_COMMON(X, UNDERSCORE(X))
#define EXPOSE_ALIGNMENT(X) EXPOSE_ALIGNMENT_COMMON(X, X)
#define EXPOSE_ALIGNMENT_S(X) EXPOSE_ALIGNMENT_COMMON(X, UNDERSCORE(X))

EXPOSE_TYPEINFO(clock_t)
EXPOSE_TYPEINFO_S(dev_t)
EXPOSE_TYPEINFO_S(ino_t)
EXPOSE_TYPEINFO_S(mode_t)
EXPOSE_TYPEINFO_S(off_t)
EXPOSE_TYPEINFO_S(pid_t)
EXPOSE_TYPEINFO(ssize_t)
EXPOSE_TYPEINFO(time_t)
EXPOSE_TYPEINFO(useconds_t)
#if !defined _WIN32 || defined __CYGWIN__
  EXPOSE_TYPEINFO(nlink_t)
#else
  /* the mingw port of fts uses an int for nlink_t */
  EXPOSE_TYPEINFO_COMMON(nlink_t, int)
#endif
EXPOSE_TYPEINFO(blkcnt_t)
EXPOSE_TYPEINFO(blksize_t)
EXPOSE_TYPEINFO(fsblkcnt_t)
EXPOSE_TYPEINFO(fsfilcnt_t)
EXPOSE_TYPEINFO(gid_t)
EXPOSE_TYPEINFO(id_t)
/* EXPOSE_TYPEINFO(timer_t) */
EXPOSE_TYPEINFO(uid_t)

EXPOSE_TYPESIZE(pthread_attr_t)
EXPOSE_TYPESIZE(pthread_cond_t)
EXPOSE_TYPESIZE(pthread_condattr_t)
EXPOSE_TYPESIZE(pthread_key_t)
EXPOSE_TYPESIZE(pthread_mutex_t)
EXPOSE_TYPESIZE(pthread_mutexattr_t)
EXPOSE_TYPESIZE(pthread_once_t)
EXPOSE_TYPESIZE(pthread_rwlock_t)
EXPOSE_TYPESIZE(pthread_rwlockattr_t)
EXPOSE_TYPESIZE(pthread_t)

EXPOSE_ALIGNMENT(pthread_attr_t)
EXPOSE_ALIGNMENT(pthread_cond_t)
EXPOSE_ALIGNMENT(pthread_condattr_t)
EXPOSE_ALIGNMENT(pthread_key_t)
EXPOSE_ALIGNMENT(pthread_mutex_t)
EXPOSE_ALIGNMENT(pthread_mutexattr_t)
EXPOSE_ALIGNMENT(pthread_once_t)
EXPOSE_ALIGNMENT(pthread_rwlock_t)
EXPOSE_ALIGNMENT(pthread_rwlockattr_t)
EXPOSE_ALIGNMENT(pthread_t)
