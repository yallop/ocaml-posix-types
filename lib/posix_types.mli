(*
 * Copyright (c) 2016 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(** {1 Types defined in <sys/types.h>} *)

(* Reference: http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/sys_types.h.html *)

module Blkcnt : Signed.S
(** Used for file block counts. *)

module Blksize : Signed.S
(** Used for block sizes. *)

module Clock : Unsigned.S
(** Used for system times in clock ticks or CLOCKS_PER_SEC; see <time.h>. *)

module Dev : Unsigned.S
  with type t = PosixTypes.Dev.t
(** Used for device IDs. *)

module Fsblkcnt : Unsigned.S
(** Used for file system block counts. *)

module Fsfilcnt : Unsigned.S
(** Used for file system file counts. *)

module Gid : Unsigned.S
(** Used for group IDs. *)

module Id : Unsigned.S
(** Used as a general identifier; can be used to contain at least a
    pid_t, uid_t, or gid_t. *)

module Ino : Unsigned.S
  with type t = PosixTypes.Ino.t
(** Used for file serial numbers. *)

module Mode : Unsigned.S
  with type t = PosixTypes.Mode.t
(** Used for some file attributes. *)

module Nlink : Unsigned.S
  with type t = PosixTypes.Nlink.t
(** Used for link counts. *)

module Off : Signed.S
  with type t = PosixTypes.Off.t
(** Used for file sizes. *)

module Pid : Signed.S
  with type t = PosixTypes.Pid.t
(** Used for process IDs and process group IDs. *)

module Pthread :
sig
  module Attr: sig type t val t : t Ctypes.typ end
  (** Used to identify a thread attribute object. *)

  module Cond: sig type t val t : t Ctypes.typ end
  (** Used for condition variables. *)

  module Condattr: sig type t val t : t Ctypes.typ end
  (** Used to identify a condition attribute object. *)

  module Key: sig type t val t : t Ctypes.typ end
  (** Used for thread-specific data keys. *)

  module Mutex: sig type t val t : t Ctypes.typ end
  (** Used for mutexes. *)

  module Mutexattr: sig type t val t : t Ctypes.typ end
  (** Used to identify a mutex attribute object. *)

  module Once: sig type t val t : t Ctypes.typ end
  (** Used for dynamic package initialization. *)

  module Rwlock: sig type t val t : t Ctypes.typ end
  (** Used for read-write locks. *)

  module Rwlockattr: sig type t val t : t Ctypes.typ end
  (** Used for read-write lock attributes. *)

  module T: sig type t val t : t Ctypes.typ end
  (** Used to identify a thread. *)

  type attr_t = Attr.t
  type cond_t = Cond.t
  type condattr_t = Condattr.t
  type key_t = Key.t
  type mutex_t = Mutex.t
  type mutexattr_t = Mutexattr.t
  type once_t = Once.t
  type rwlock_t = Rwlock.t
  type rwlockattr_t = Rwlockattr.t
  type t = T.t
end

module Size : Unsigned.S
  with type t = Unsigned.Size_t.t
(** Used for sizes of objects. *)

module Ssize : Signed.S
  with type t = PosixTypes.Ssize.t
(** Used for a count of bytes or an error indication. *)

module Time : Unsigned.S
  with type t = PosixTypes.Time.t
(** Used for time in seconds. *)

module Uid : Unsigned.S
(** Used for user IDs. *)

type blkcnt_t = Blkcnt.t
type blksize_t = Blksize.t
type clock_t = Clock.t
type dev_t = Dev.t
type fsblkcnt_t = Fsblkcnt.t
type fsfilcnt_t = Fsfilcnt.t
type gid_t = Gid.t
type id_t = Id.t
type ino_t = Ino.t
type mode_t = Mode.t
type nlink_t = Nlink.t
type off_t = Off.t
type pid_t = Pid.t
type pthread_attr_t = Pthread.Attr.t
type pthread_cond_t = Pthread.Cond.t
type pthread_condattr_t = Pthread.Condattr.t
type pthread_key_t = Pthread.Key.t
type pthread_mutex_t = Pthread.Mutex.t
type pthread_mutexattr_t = Pthread.Mutexattr.t
type pthread_once_t = Pthread.Once.t
type pthread_rwlock_t = Pthread.Rwlock.t
type pthread_rwlockattr_t = Pthread.Rwlockattr.t
type pthread_t = Pthread.T.t
type size_t = Size.t
type ssize_t = Ssize.t
type time_t = Time.t
type uid_t = Uid.t

val blkcnt_t : blkcnt_t Ctypes.typ
val blksize_t : blksize_t Ctypes.typ
val clock_t : clock_t Ctypes.typ
val dev_t : dev_t Ctypes.typ
val fsblkcnt_t : fsblkcnt_t Ctypes.typ
val fsfilcnt_t : fsfilcnt_t Ctypes.typ
val gid_t : gid_t Ctypes.typ
val id_t : id_t Ctypes.typ
val ino_t : ino_t Ctypes.typ
val mode_t : mode_t Ctypes.typ
val nlink_t : nlink_t Ctypes.typ
val off_t : off_t Ctypes.typ
val pid_t : pid_t Ctypes.typ
val pthread_attr_t : pthread_attr_t Ctypes.typ
val pthread_cond_t : pthread_cond_t Ctypes.typ
val pthread_condattr_t : pthread_condattr_t Ctypes.typ
val pthread_key_t : pthread_key_t Ctypes.typ
val pthread_mutex_t : pthread_mutex_t Ctypes.typ
val pthread_mutexattr_t : pthread_mutexattr_t Ctypes.typ
val pthread_once_t : pthread_once_t Ctypes.typ
val pthread_rwlock_t : pthread_rwlock_t Ctypes.typ
val pthread_rwlockattr_t : pthread_rwlockattr_t Ctypes.typ
val pthread_t : pthread_t Ctypes.typ
val size_t : size_t Ctypes.typ
val ssize_t : ssize_t Ctypes.typ
val time_t : time_t Ctypes.typ
val uid_t : uid_t Ctypes.typ
