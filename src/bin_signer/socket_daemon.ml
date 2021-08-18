(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Signer_messages
module Events = Signer_events.Socket_daemon

let handle_client_step ?magic_bytes ?timeout ~check_high_watermark ~require_auth
    cctxt fd =
  Tezos_base_unix.Socket.recv ?timeout fd Request.encoding >>=? function
  | Sign req ->
      let encoding = result_encoding Sign.Response.encoding in
      Handler.sign cctxt req ?magic_bytes ~check_high_watermark ~require_auth
      >>= fun res -> Tezos_base_unix.Socket.send fd encoding res
  | Deterministic_nonce req ->
      let encoding = result_encoding Deterministic_nonce.Response.encoding in
      Handler.deterministic_nonce cctxt req ~require_auth >>= fun res ->
      Tezos_base_unix.Socket.send fd encoding res
  | Deterministic_nonce_hash req ->
      let encoding =
        result_encoding Deterministic_nonce_hash.Response.encoding
      in
      Handler.deterministic_nonce_hash cctxt req ~require_auth >>= fun res ->
      Tezos_base_unix.Socket.send fd encoding res
  | Supports_deterministic_nonces req ->
      let encoding =
        result_encoding Supports_deterministic_nonces.Response.encoding
      in
      Handler.supports_deterministic_nonces cctxt req >>= fun res ->
      Tezos_base_unix.Socket.send fd encoding res
  | Public_key pkh ->
      let encoding = result_encoding Public_key.Response.encoding in
      Handler.public_key cctxt pkh >>= fun res ->
      Tezos_base_unix.Socket.send fd encoding res
  | Authorized_keys ->
      let encoding = result_encoding Authorized_keys.Response.encoding in
      (if require_auth then
       Handler.Authorized_key.load cctxt >>=? fun keys ->
       return
         (Authorized_keys.Response.Authorized_keys
            (keys |> List.split |> snd |> List.map Signature.Public_key.hash))
      else return Authorized_keys.Response.No_authentication)
      >>= fun res -> Tezos_base_unix.Socket.send fd encoding res

let handle_client_loop ?magic_bytes ?timeout ~check_high_watermark ~require_auth
    cctxt fd =
  let rec loop () =
    handle_client_step
      ?magic_bytes
      ?timeout
      ~check_high_watermark
      ~require_auth
      cctxt
      fd
    >>=? loop
  in
  loop ()

let run ?magic_bytes ?timeout ~check_high_watermark ~require_auth
    (cctxt : #Client_context.wallet) path =
  let open Tezos_base_unix.Socket in
  (match path with
  | Tcp (host, service, _opts) ->
      Events.(emit accepting_tcp_requests) (host, service)
  | Unix path ->
      ListLabels.iter
        Sys.[sigint; sigterm]
        ~f:(fun signal ->
          Sys.set_signal
            signal
            (Signal_handle
               (fun _ ->
                 Format.printf "Removing the local socket file and quitting.@." ;
                 Unix.unlink path ;
                 exit 0))) ;
      Events.(emit accepting_unix_requests) path)
  >>= fun () ->
  bind path >>=? fun fds ->
  let rec loop fd =
    Lwt_unix.accept fd >>= fun (cfd, _) ->
    Lwt_utils.dont_wait
      (fun exc ->
        Format.eprintf "Uncaught exception: %s\n%!" (Printexc.to_string exc))
      (fun () ->
        protect
          ~on_error:(function
            | Exn End_of_file :: _ -> return_unit
            | errs -> Lwt.return_error errs)
          (fun () ->
            Lwt.finalize
              (fun () ->
                handle_client_loop
                  ?magic_bytes
                  ?timeout
                  ~check_high_watermark
                  ~require_auth
                  cctxt
                  cfd)
              (fun () ->
                Lwt_utils_unix.safe_close cfd >>= function
                | Error trace ->
                    Format.eprintf "Uncaught error: %a\n%!" pp_print_error trace ;
                    Lwt.return_unit
                | Ok () -> Lwt.return_unit))
        >>= fun _ -> Lwt.return_unit) ;
    loop fd
  in
  List.map_p loop fds >>= return
