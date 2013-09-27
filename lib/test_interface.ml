external shutdown : unit -> unit = ""
external reboot : unit -> unit = ""
external crash : unit -> unit = ""

type junk = (ExtentlistSet.Int64extentlist.t * char) list


module Block = struct
  external hammer : unit -> unit = ""
  external tickle : unit -> unit = ""
  external write_junk : int -> junk -> junk = ""
end

