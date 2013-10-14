external shutdown : unit -> unit = ""
external reboot : unit -> unit = ""
external crash : unit -> unit = ""

type vbdid = string

module Vbd = struct
  external write_sector : vbdid -> int64 -> string -> unit = ""
  external read_sector : vbdid -> int64 -> string = ""
  external list : unit -> vbdid list = ""

  external start_hammer : vbdid -> unit = ""
  external stop_hammer : vbdid -> unit = ""

  external start_tickle : vbdid -> unit = ""
  external stop_tickle : vbdid -> unit = ""

  external start_junk_writer : vbdid -> int -> unit = ""
  external stop_junk_writer : vbdid -> bool = ""
end

type vifid = string

module Vif = struct
  external list : unit -> vifid list = ""
  external get_ipv4 : vifid -> string = ""
  external inject_packet : vifid -> string -> unit = ""
end
