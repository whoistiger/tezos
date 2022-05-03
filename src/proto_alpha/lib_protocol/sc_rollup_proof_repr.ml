(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

open Sc_rollup_repr

type pvm_ops = {
  eval : string option -> Context.tree -> (Context.tree * unit) Lwt.t;
  expect_input : Context.tree -> (Context.tree * (int * int) option) Lwt.t;
}

type t =
  | Computation_step of {
      step : Context.Proof.tree Context.Proof.t;
      not_input : Context.Proof.tree Context.Proof.t;
    }
  | Input_step of {
      step : Context.Proof.tree Context.Proof.t;
      input : Context.Proof.tree Context.Proof.t;
      next : Sc_rollup_inbox_repr.inclusion_proof;
      inclusion : Sc_rollup_inbox_repr.inclusion_proof;
    }
  | Blocked_step of {
      input : Context.Proof.tree Context.Proof.t;
      no_next : Sc_rollup_inbox_repr.inclusion_proof;
      inclusion : Sc_rollup_inbox_repr.inclusion_proof;
    }

let encoding =
  Data_encoding.(
    union
      ~tag_size:`Uint8
      [
        case
          ~title:"Proof of a normal computation step"
          (Tag 0)
          (tup2
             Context.Proof_encoding.V2.Tree32.tree_proof_encoding
             Context.Proof_encoding.V2.Tree32.tree_proof_encoding)
          (function
            | Computation_step {step; not_input} -> Some (step, not_input)
            | _ -> None)
          (fun (step, not_input) -> Computation_step {step; not_input});
        case
          ~title:"Proof of an input step"
          (Tag 1)
          (tup4
             Context.Proof_encoding.V2.Tree32.tree_proof_encoding
             Context.Proof_encoding.V2.Tree32.tree_proof_encoding
             Sc_rollup_inbox_repr.inclusion_proof_encoding
             Sc_rollup_inbox_repr.inclusion_proof_encoding)
          (function
            | Input_step {step; input; next; inclusion} ->
                Some (step, input, next, inclusion)
            | _ -> None)
          (fun (step, input, next, inclusion) ->
            Input_step {step; input; next; inclusion});
        case
          ~title:"Proof that the PVM is blocked"
          (Tag 2)
          (tup3
             Context.Proof_encoding.V2.Tree32.tree_proof_encoding
             Sc_rollup_inbox_repr.inclusion_proof_encoding
             Sc_rollup_inbox_repr.inclusion_proof_encoding)
          (function
            | Blocked_step {input; no_next; inclusion} ->
                Some (input, no_next, inclusion)
            | _ -> None)
          (fun (input, no_next, inclusion) ->
            Blocked_step {input; no_next; inclusion});
      ])

(* TODO: #2759 *)
let pp _ _ = ()

let start p =
  match p with
  | Computation_step x -> State_hash.of_kinded_hash x.step.before
  | Input_step x -> State_hash.of_kinded_hash x.step.before
  | Blocked_step x -> State_hash.of_kinded_hash x.input.before

let stop p =
  match p with
  | Computation_step x -> Some (State_hash.of_kinded_hash x.step.after)
  | Input_step x -> Some (State_hash.of_kinded_hash x.step.after)
  | Blocked_step _ -> None

let kinded_hash_equal a b =
  match (a, b) with
  | (`Node x, `Node y) -> Context_hash.equal x y
  | (`Value x, `Value y) -> Context_hash.equal x y
  | _ -> false

let valid pvm_ops _inbox p =
  let result =
    let open Lwt_result_syntax in
    match p with
    | Computation_step x ->
        let hashes_match = kinded_hash_equal x.step.before x.not_input.before in
        let* _ = Context.verify_tree_proof x.step (pvm_ops.eval None) in
        let* (_, not_input_result) =
          Context.verify_tree_proof x.not_input pvm_ops.expect_input
        in
        return (hashes_match && Option.is_none not_input_result)
    | Input_step x ->
        let hashes_match = kinded_hash_equal x.step.before x.input.before in
        let* _ = Context.verify_tree_proof x.step (pvm_ops.eval (Some "")) in
        let* (_, input_location) =
          Context.verify_tree_proof x.input pvm_ops.expect_input
        in
        let* _ =
          match input_location with
          | None -> return ()
          | Some (_level, _counter) -> return ()
        in
        return hashes_match
    | Blocked_step _ -> return false
  in
  (* XXX: should we translate the error somehow? *)
  Lwt.map (Result.map_error (fun _ -> ())) result
