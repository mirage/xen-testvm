external shutdown : unit -> unit = ""
external reboot : unit -> unit = ""
external crash : unit -> unit = ""

module Int64extentlist = ExtentlistSet.ExtentlistSet(struct type t=int64 let add=Int64.add let sub=Int64.sub let zero=0L end)

