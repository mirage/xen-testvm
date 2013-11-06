(*
 * Copyright (C) 2011-2013 Citrix Inc
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Xen_api

val upload: pool:string -> username:string -> password:string -> kernel:string -> API.ref_VDI Lwt.t
(** [upload pool username password kernel] creates a bootable disk
    containing a partition table, filesystem and grub config containing
    the PV kernel [kernel] and uploads it to the xapi at URL [pool]
    with [username] and [password]. *)
