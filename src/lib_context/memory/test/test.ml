(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 DaiLambda, Inc. <contact@dailambda.jp>                 *)
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

(* shell context *)
module C = struct
  include Tezos_context.Context

  (** Basic blocks *)

  let genesis_block =
    Block_hash.of_b58check_exn
      "BLockGenesisGenesisGenesisGenesisGenesisGeneskvg68z"

  let genesis_protocol =
    Protocol_hash.of_b58check_exn
      "ProtoDemoNoopsDemoNoopsDemoNoopsDemoNoopsDemo6XBoYp"

  let genesis_time = Time.Protocol.of_seconds 0L

  let chain_id = Chain_id.of_block_hash genesis_block

  let make_context () =
    (* there is no simple way to build a context *)
    Lwt_utils_unix.with_tempdir "tezos_test_" (fun base_dir ->
        let open Filename.Infix in
        let root = base_dir // "context" in
        init root >>= fun idx ->
        commit_genesis
          idx
          ~chain_id
          ~time:genesis_time
          ~protocol:genesis_protocol
        >>= function
        | Error _ -> assert false
        | Ok genesis -> (
            checkout idx genesis >|= function
            | None -> assert false
            | Some ctxt -> ctxt))
end

(* memory context *)
module M = struct
  include Tezos_context_memory.Context

  let make_context () = Lwt.return empty
end

module Make (A : sig
  include Tezos_context_sigs.Context.S

  val make_context : unit -> t Lwt.t
end) =
struct
  let hash_of_dir n =
    A.make_context () >>= fun ctxt ->
    Lwt_list.fold_left_s
      (fun t x -> A.Tree.add t [x] (Bytes.of_string x))
      (A.Tree.empty ctxt)
      (Stdlib.List.init n (fun i -> string_of_int i))
    >|= A.Tree.hash
end

(* Compare the shell and memory contexts have the same hashes for large directories *)
let test_hash =
  let test n _lwt_switch () =
    let module C = Make (C) in
    let module M = Make (M) in
    C.hash_of_dir n >>= fun ch ->
    M.hash_of_dir n >>= fun mh ->
    if not @@ Context_hash.equal ch mh then
      Stdlib.failwith
        (Format.asprintf
           "hash mismatch: n=%d context: %a memory: %a"
           n
           Context_hash.pp
           ch
           Context_hash.pp
           mh)
    else Lwt.return ()
  in
  List.map
    (fun n ->
      Alcotest_lwt.test_case (Printf.sprintf "large-dir-%d" n) `Quick @@ test n)
    [100; 200; 255; 256; 257; 300; 1000; 10000]

let () =
  Lwt_main.run (Alcotest_lwt.run "tezos-context-memory" [("hash", test_hash)])