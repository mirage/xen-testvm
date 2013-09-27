type __exn_ty2 = string

type __exn_ty1 = (string * int * int)

type __exn_ty0 = string

exception Unknown_RPC of __exn_ty2
  
exception Message_param_count_mismatch of __exn_ty1
  
exception Internal_error of __exn_ty0
  
module Exception =
  struct
    type exnty =
      | Internal_error of string
      | Message_param_count_mismatch of (string * int * int)
      | Unknown_RPC of string
    
    let rpc_of_exnty __x8__ =
      match __x8__ with
      | Unknown_RPC __x9__ ->
          Rpc.Enum [ Rpc.String "Unknown_RPC"; Rpc.String __x9__ ]
      | Message_param_count_mismatch __x10__ ->
          Rpc.Enum
            [ Rpc.String "Message_param_count_mismatch";
              (let (__x11__, __x12__, __x13__) = __x10__
               in
                 Rpc.Enum
                   [ Rpc.String __x11__; Rpc.Int (Int64.of_int __x12__);
                     Rpc.Int (Int64.of_int __x13__) ]) ]
      | Internal_error __x14__ ->
          Rpc.Enum [ Rpc.String "Internal_error"; Rpc.String __x14__ ]
      
    let exnty_of_rpc __x1__ =
      match Rpc.lowerfn __x1__ with
      | Rpc.Enum ([ Rpc.String "unknown_rpc"; __x2__ ]) ->
          Unknown_RPC
            (match __x2__ with
             | Rpc.String x -> x
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                      "exnty" "__x2__" (Rpc.to_string __x__) "String(string)"
                  else ();
                  raise (Rpc.Runtime_error (("String(string)", __x__)))))
      | Rpc.Enum ([ Rpc.String "message_param_count_mismatch"; __x3__ ]) ->
          Message_param_count_mismatch
            (match __x3__ with
             | Rpc.Enum ([ __x4__; __x5__; __x6__ ]) ->
                 ((match __x4__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x4__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error (("String(string)", __x__))))),
                  (match __x5__ with
                   | Rpc.Int x -> Int64.to_int x
                   | Rpc.String s -> int_of_string s
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x5__" (Rpc.to_string __x__) "Int(int)"
                        else ();
                        raise (Rpc.Runtime_error (("Int(int)", __x__))))),
                  (match __x6__ with
                   | Rpc.Int x -> Int64.to_int x
                   | Rpc.String s -> int_of_string s
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x6__" (Rpc.to_string __x__) "Int(int)"
                        else ();
                        raise (Rpc.Runtime_error (("Int(int)", __x__))))))
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                      "exnty" "__x3__" (Rpc.to_string __x__) "List"
                  else ();
                  raise (Rpc.Runtime_error (("List", __x__)))))
      | Rpc.Enum ([ Rpc.String "internal_error"; __x7__ ]) ->
          Internal_error
            (match __x7__ with
             | Rpc.String x -> x
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                      "exnty" "__x7__" (Rpc.to_string __x__) "String(string)"
                  else ();
                  raise (Rpc.Runtime_error (("String(string)", __x__)))))
      | __x__ ->
          (if Rpc.get_debug ()
           then
             Printf.eprintf
               "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
               "exnty" "__x1__" (Rpc.to_string __x__) "Enum[String s;...]"
           else ();
           raise (Rpc.Runtime_error (("Enum[String s;...]", __x__))))
      
  end
  
let exnty_of_exn x =
  match x with
  | Internal_error x -> Exception.Internal_error x
  | Message_param_count_mismatch x ->
      Exception.Message_param_count_mismatch x
  | Unknown_RPC x -> Exception.Unknown_RPC x
  | e -> Exception.Internal_error (Printexc.to_string e)
  
let exn_of_exnty x =
  match x with
  | Exception.Internal_error x -> Internal_error x
  | Exception.Message_param_count_mismatch x ->
      Message_param_count_mismatch x
  | Exception.Unknown_RPC x -> Unknown_RPC x
  
module Args =
  struct
    module Shutdown =
      struct
        let rpc_of___x1__ __x16__ = Rpc.Null
        and __x1___of_rpc __x15__ =
          match __x15__ with
          | Rpc.Null -> ()
          | __x__ ->
              (if Rpc.get_debug ()
               then
                 Printf.eprintf
                   "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                   "__x1__" "__x15__" (Rpc.to_string __x__) "Null"
               else ();
               raise (Rpc.Runtime_error (("Null", __x__))))
          
        type response = unit
        
        let rpc_of_response __x18__ = Rpc.Null
          
        let response_of_rpc __x17__ =
          match __x17__ with
          | Rpc.Null -> ()
          | __x__ ->
              (if Rpc.get_debug ()
               then
                 Printf.eprintf
                   "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                   "response" "__x17__" (Rpc.to_string __x__) "Null"
               else ();
               raise (Rpc.Runtime_error (("Null", __x__))))
          
        let call_of_shutdown __x1__ =
          Rpc.call "shutdown" [ rpc_of___x1__ __x1__ ]
          
      end
      
    module Reboot =
      struct
        let rpc_of___x1__ __x20__ = Rpc.Null
        and __x1___of_rpc __x19__ =
          match __x19__ with
          | Rpc.Null -> ()
          | __x__ ->
              (if Rpc.get_debug ()
               then
                 Printf.eprintf
                   "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                   "__x1__" "__x19__" (Rpc.to_string __x__) "Null"
               else ();
               raise (Rpc.Runtime_error (("Null", __x__))))
          
        type response = unit
        
        let rpc_of_response __x22__ = Rpc.Null
          
        let response_of_rpc __x21__ =
          match __x21__ with
          | Rpc.Null -> ()
          | __x__ ->
              (if Rpc.get_debug ()
               then
                 Printf.eprintf
                   "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                   "response" "__x21__" (Rpc.to_string __x__) "Null"
               else ();
               raise (Rpc.Runtime_error (("Null", __x__))))
          
        let call_of_reboot __x1__ =
          Rpc.call "reboot" [ rpc_of___x1__ __x1__ ]
          
      end
      
    module Crash =
      struct
        let rpc_of___x1__ __x24__ = Rpc.Null
        and __x1___of_rpc __x23__ =
          match __x23__ with
          | Rpc.Null -> ()
          | __x__ ->
              (if Rpc.get_debug ()
               then
                 Printf.eprintf
                   "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                   "__x1__" "__x23__" (Rpc.to_string __x__) "Null"
               else ();
               raise (Rpc.Runtime_error (("Null", __x__))))
          
        type response = unit
        
        let rpc_of_response __x26__ = Rpc.Null
          
        let response_of_rpc __x25__ =
          match __x25__ with
          | Rpc.Null -> ()
          | __x__ ->
              (if Rpc.get_debug ()
               then
                 Printf.eprintf
                   "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                   "response" "__x25__" (Rpc.to_string __x__) "Null"
               else ();
               raise (Rpc.Runtime_error (("Null", __x__))))
          
        let call_of_crash __x1__ = Rpc.call "crash" [ rpc_of___x1__ __x1__ ]
          
      end
      
  end
  
module type RPCM =
  sig
    type 'a t
    
    val rpc : Rpc.call -> Rpc.response t
      
    val bind : 'a t -> ('a -> 'b t) -> 'b t
      
    val return : 'a -> 'a t
      
    val fail : exn -> 'b t
      
  end
  
module type RPC = sig val rpc : Rpc.call -> Rpc.response
                         end
  
module ClientM (R : RPCM) =
  struct
    let shutdown __x0__ =
      let call = Args.Shutdown.call_of_shutdown __x0__
      in
        R.bind (R.rpc call)
          (fun response ->
             if response.Rpc.success
             then
               R.return (Args.Shutdown.response_of_rpc response.Rpc.contents)
             else
               (let e =
                  exn_of_exnty (Exception.exnty_of_rpc response.Rpc.contents)
                in R.fail e))
      
    let reboot __x0__ =
      let call = Args.Reboot.call_of_reboot __x0__
      in
        R.bind (R.rpc call)
          (fun response ->
             if response.Rpc.success
             then
               R.return (Args.Reboot.response_of_rpc response.Rpc.contents)
             else
               (let e =
                  exn_of_exnty (Exception.exnty_of_rpc response.Rpc.contents)
                in R.fail e))
      
    let crash __x0__ =
      let call = Args.Crash.call_of_crash __x0__
      in
        R.bind (R.rpc call)
          (fun response ->
             if response.Rpc.success
             then R.return (Args.Crash.response_of_rpc response.Rpc.contents)
             else
               (let e =
                  exn_of_exnty (Exception.exnty_of_rpc response.Rpc.contents)
                in R.fail e))
      
  end
  
module Client (R : RPC) =
  struct
    module RPCM =
      struct
        type 'a t = 'a
        
        let rpc = R.rpc
          
        let bind a f = f a
          
        let return x = x
          
        let fail = raise
          
      end
      
    include ClientM(RPCM)
      
  end
  
module type Server_impl =
  sig
    type context
    
    val shutdown : context -> unit -> unit
      
    val reboot : context -> unit -> unit
      
    val crash : context -> unit -> unit
      
  end
  
module type Server_implM =
  sig
    type 'a t
    
    val bind : 'a t -> ('a -> 'b t) -> 'b t
      
    val return : 'a -> 'a t
      
    val fail : exn -> 'a t
      
    val handle_failure : (unit -> 'b t) -> (exn -> 'b t) -> 'b t
      
    type context
    
    val shutdown : context -> unit -> unit t
      
    val reboot : context -> unit -> unit t
      
    val crash : context -> unit -> unit t
      
  end
  
module ServerM (Impl : Server_implM) =
  struct
    let process x call =
      Impl.handle_failure
        (fun () ->
           let contents =
             match ((call.Rpc.name), (call.Rpc.params)) with
             | ("shutdown", [ __x1__ ]) ->
                 Impl.bind
                   (Impl.shutdown x (Args.Shutdown.__x1___of_rpc __x1__))
                   (fun x -> Impl.return (Args.Shutdown.rpc_of_response x))
             | ("shutdown", _) ->
                 Impl.fail
                   (Message_param_count_mismatch
                      (("shutdown", 1, (List.length call.Rpc.params))))
             | ("reboot", [ __x1__ ]) ->
                 Impl.bind (Impl.reboot x (Args.Reboot.__x1___of_rpc __x1__))
                   (fun x -> Impl.return (Args.Reboot.rpc_of_response x))
             | ("reboot", _) ->
                 Impl.fail
                   (Message_param_count_mismatch
                      (("reboot", 1, (List.length call.Rpc.params))))
             | ("crash", [ __x1__ ]) ->
                 Impl.bind (Impl.crash x (Args.Crash.__x1___of_rpc __x1__))
                   (fun x -> Impl.return (Args.Crash.rpc_of_response x))
             | ("crash", _) ->
                 Impl.fail
                   (Message_param_count_mismatch
                      (("crash", 1, (List.length call.Rpc.params))))
             | (x, _) -> Impl.fail (Unknown_RPC x)
           in
             Impl.bind contents
               (fun contents ->
                  Impl.return
                    { Rpc.success = true; Rpc.contents = contents; }))
        (fun e ->
           Impl.return
             {
               Rpc.success = false;
               Rpc.contents = Exception.rpc_of_exnty (exnty_of_exn e);
             })
      
  end
  
module Server (Impl : Server_impl) =
  struct
    module ImplM =
      struct
        type 'a t = 'a
        
        let bind a f = f a
          
        let return a = a
          
        let fail = raise
          
        let handle_failure f g = try f () with | e -> g e
          
        include Impl
          
      end
      
    module M = ServerM(ImplM)
      
    include M
      
  end
  

