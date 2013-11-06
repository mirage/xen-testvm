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

val create: string -> string -> API.ref_VM Lwt.t
(** [create kernel password] creates a Mirage VM from the [kernel]
    path, logging into xapi using [password] *)

val destroy: API.ref_VM -> string -> unit Lwt.t
(** [destroy vm] cleans up all resources used by the given VMm
    logging into xapi using [password] *)

val start: rpc:(Rpc.call -> Rpc.response Lwt.t) -> session_id:API.ref_session -> API.ref_VM -> unit Lwt.t
(** [start rpc session_id vm] gets the test VM into the Running state *)

val shutdown: rpc:(Rpc.call -> Rpc.response Lwt.t) -> session_id:API.ref_session -> API.ref_VM -> unit Lwt.t
(** [shutdown rpc session_id vm] gets the test VM into the Halted state *)
