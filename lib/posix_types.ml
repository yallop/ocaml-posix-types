(*
 * Copyright (c) 2016 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* This corresponds to the enum in ctypes_primitives.h *)
type arithmetic =
    Int8
  | Int16
  | Int32
  | Int64
  | Uint8
  | Uint16
  | Uint32
  | Uint64
  | Float
  | Double

module type Abstract =
sig
  type t
  val t : t Ctypes.typ
end

module type Signed_S =
sig
  include Signed.S
  val t : t Ctypes.typ
end

module type Unsigned_S =
sig
  include Unsigned.S
  val t : t Ctypes.typ
end

let mkAbstractSized : name:string -> size:int -> alignment:int -> (module Abstract)
  = fun ~name ~size ~alignment:a ->
    (module
     struct
       open Ctypes
       type t = unit Ctypes.abstract
       let t = abstract ~name ~size ~alignment:a
     end : Abstract)

let signed_typedef name ~size : (module Signed_S) =
  match size with
    1 -> (module struct include Signed.Int
           let t = Ctypes.(typedef int8_t name) end)
  | 2 -> (module struct include Signed.Int
           let t = Ctypes.(typedef int16_t name) end)
  | 4 -> (module struct include Signed.Int32
           let t = Ctypes.(typedef int32_t name) end)
  | 8 -> (module struct include Signed.Int64
           let t = Ctypes.(typedef int64_t name) end)
  | n -> Printf.kprintf failwith "size %d not supported for %s\n" n name

let unsigned_typedef name ~size : (module Unsigned_S) =
  match size with
  | 1 -> (module struct include Unsigned.UInt8
           let t = Ctypes.(typedef uint8_t name) end)
  | 2 -> (module struct include Unsigned.UInt16
           let t = Ctypes.(typedef uint16_t name) end)
  | 4 -> (module struct include Unsigned.UInt32
           let t = Ctypes.(typedef uint32_t name) end)
  | 8 -> (module struct include Unsigned.UInt64
           let t = Ctypes.(typedef uint64_t name) end)
  | n -> Printf.kprintf failwith "size %d not supported for %s\n" n name

let mkSigned name = function
  | Int8  -> signed_typedef name ~size:1
  | Int16 -> signed_typedef name ~size:2
  | Int32 -> signed_typedef name ~size:4
  | Int64 -> signed_typedef name ~size:8
  | _ -> assert false

let mkUnsigned name = function
  | Uint8  -> unsigned_typedef name ~size:1
  | Uint16 -> unsigned_typedef name ~size:2
  | Uint32 -> unsigned_typedef name ~size:4
  | Uint64 -> unsigned_typedef name ~size:8
  | _ -> assert false

let mkArithmetic name : _ -> (module Unsigned_S) =
  function
  | Uint8 | Uint16 | Uint32 | Uint64 as u ->
    let module U = (val mkUnsigned name u) in (module U)
  | Int8 | Int16 | Int32 | Int64 as u ->
    let module S = (val mkSigned name u) in (module S)
  | _ -> assert false

module Dev = PosixTypes.Dev
module Ino = PosixTypes.Ino
module Mode = PosixTypes.Mode
module Nlink = PosixTypes.Nlink
module Off = PosixTypes.Off
module Pid = PosixTypes.Pid
module Size = Unsigned.Size_t
module Ssize = PosixTypes.Ssize
module Time = PosixTypes.Time

external typeof_blkcnt_t : unit -> arithmetic =
  "posix_types_typeof_blkcnt_t"
external typeof_blksize_t : unit -> arithmetic =
  "posix_types_typeof_blksize_t"
external typeof_clock_t : unit -> arithmetic =
  "posix_types_typeof_clock_t"
external typeof_fsblkcnt_t : unit -> arithmetic =
  "posix_types_typeof_fsblkcnt_t"
external typeof_fsfilcnt_t : unit -> arithmetic =
  "posix_types_typeof_fsfilcnt_t"
external typeof_gid_t : unit -> arithmetic =
  "posix_types_typeof_gid_t"
external typeof_id_t : unit -> arithmetic =
  "posix_types_typeof_id_t"
external typeof_uid_t : unit -> arithmetic =
  "posix_types_typeof_uid_t"

module Blkcnt = (val mkSigned "blkcnt_t" (typeof_blkcnt_t ()))
module Blksize = (val mkSigned "blksize_t" (typeof_blksize_t ()))
module Clock = (val mkArithmetic "clock_t" (typeof_clock_t ()))
module Fsblkcnt = (val mkArithmetic "fsblkcnt_t" (typeof_fsblkcnt_t ()))
module Fsfilcnt = (val mkArithmetic "fsfilcnt_t" (typeof_fsfilcnt_t ()))
module Gid = (val mkArithmetic "gid_t" (typeof_gid_t ()))
module Id = (val mkArithmetic "id_t" (typeof_id_t ()))
module Uid = (val mkArithmetic "uid_t" (typeof_uid_t ()))

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
type size_t = Size.t
type ssize_t = Ssize.t
type time_t = Time.t
type uid_t = Uid.t

let blkcnt_t = Blkcnt.t
let blksize_t = Blksize.t
let clock_t = Clock.t
let dev_t = PosixTypes.dev_t
let fsblkcnt_t = Fsblkcnt.t
let fsfilcnt_t = Fsfilcnt.t
let gid_t = Gid.t
let id_t = Id.t
let ino_t = PosixTypes.ino_t
let mode_t = PosixTypes.mode_t
let nlink_t = PosixTypes.nlink_t
let off_t = PosixTypes.off_t
let pid_t = PosixTypes.pid_t
let size_t = Ctypes.size_t
let ssize_t = PosixTypes.ssize_t
let time_t = PosixTypes.time_t
let uid_t = Uid.t

external sizeof_pthread_attr_t : unit -> int =
  "posix_types_sizeof_pthread_attr_t"
external sizeof_pthread_cond_t : unit -> int =
  "posix_types_sizeof_pthread_cond_t"
external sizeof_pthread_condattr_t : unit -> int =
  "posix_types_sizeof_pthread_condattr_t"
external sizeof_pthread_key_t : unit -> int =
  "posix_types_sizeof_pthread_key_t"
external sizeof_pthread_mutex_t : unit -> int =
  "posix_types_sizeof_pthread_mutex_t"
external sizeof_pthread_mutexattr_t : unit -> int =
  "posix_types_sizeof_pthread_mutexattr_t"
external sizeof_pthread_once_t : unit -> int =
  "posix_types_sizeof_pthread_once_t"
external sizeof_pthread_rwlock_t : unit -> int =
  "posix_types_sizeof_pthread_rwlock_t"
external sizeof_pthread_rwlockattr_t : unit -> int =
  "posix_types_sizeof_pthread_rwlockattr_t"
external sizeof_pthread_t : unit -> int =
  "posix_types_sizeof_pthread_t"

external alignmentof_pthread_attr_t : unit -> int =
  "posix_types_alignmentof_pthread_attr_t"
external alignmentof_pthread_cond_t : unit -> int =
  "posix_types_alignmentof_pthread_cond_t"
external alignmentof_pthread_condattr_t : unit -> int =
  "posix_types_alignmentof_pthread_condattr_t"
external alignmentof_pthread_key_t : unit -> int =
  "posix_types_alignmentof_pthread_key_t"
external alignmentof_pthread_mutex_t : unit -> int =
  "posix_types_alignmentof_pthread_mutex_t"
external alignmentof_pthread_mutexattr_t : unit -> int =
  "posix_types_alignmentof_pthread_mutexattr_t"
external alignmentof_pthread_once_t : unit -> int =
  "posix_types_alignmentof_pthread_once_t"
external alignmentof_pthread_rwlock_t : unit -> int =
  "posix_types_alignmentof_pthread_rwlock_t"
external alignmentof_pthread_rwlockattr_t : unit -> int =
  "posix_types_alignmentof_pthread_rwlockattr_t"
external alignmentof_pthread_t : unit -> int =
  "posix_types_alignmentof_pthread_t"

module Pthread =
struct
  module Attr = (val mkAbstractSized
                   ~name:"pthread_attr_t"
                   ~size:(sizeof_pthread_attr_t ())
                   ~alignment:(alignmentof_pthread_attr_t ()))
  module Cond = (val mkAbstractSized
                   ~name:"pthread_cond_t"
                   ~size:(alignmentof_pthread_cond_t ())
                   ~alignment:(sizeof_pthread_cond_t ()))
  module Condattr = (val mkAbstractSized
                   ~name:"pthread_condattr_t"
                   ~size:(alignmentof_pthread_condattr_t ())
                   ~alignment:(sizeof_pthread_condattr_t ()))
  module Key = (val mkAbstractSized
                   ~name:"pthread_key_t"
                   ~size:(alignmentof_pthread_key_t ())
                   ~alignment:(sizeof_pthread_key_t ()))
  module Mutex = (val mkAbstractSized
                   ~name:"pthread_mutex_t"
                   ~size:(alignmentof_pthread_mutex_t ())
                   ~alignment:(sizeof_pthread_mutex_t ()))
  module Mutexattr = (val mkAbstractSized
                   ~name:"pthread_mutexattr_t"
                   ~size:(alignmentof_pthread_mutexattr_t ())
                   ~alignment:(sizeof_pthread_mutexattr_t ()))
  module Once = (val mkAbstractSized
                   ~name:"pthread_once_t"
                   ~size:(alignmentof_pthread_once_t ())
                   ~alignment:(sizeof_pthread_once_t ()))
  module Rwlock = (val mkAbstractSized
                   ~name:"pthread_rwlock_t"
                   ~size:(alignmentof_pthread_rwlock_t ())
                   ~alignment:(sizeof_pthread_rwlock_t ()))
  module Rwlockattr = (val mkAbstractSized
                   ~name:"pthread_rwlockattr_t"
                   ~size:(alignmentof_pthread_rwlockattr_t ())
                   ~alignment:(sizeof_pthread_rwlockattr_t ()))
  module T = (val mkAbstractSized
                   ~name:"pthread_t"
                   ~size:(alignmentof_pthread_t ())
                   ~alignment:(sizeof_pthread_t ()))
             
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

let pthread_attr_t = Pthread.Attr.t
let pthread_cond_t = Pthread.Cond.t
let pthread_condattr_t = Pthread.Condattr.t
let pthread_key_t = Pthread.Key.t
let pthread_mutex_t = Pthread.Mutex.t
let pthread_mutexattr_t = Pthread.Mutexattr.t
let pthread_once_t = Pthread.Once.t
let pthread_rwlock_t = Pthread.Rwlock.t
let pthread_rwlockattr_t = Pthread.Rwlockattr.t
let pthread_t = Pthread.T.t
