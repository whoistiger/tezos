(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

(** Testing
    -------
    Component:  Protocol (Ticket_scanner)
    Invocation: dune exec \
                src/proto_alpha/lib_protocol/test/integration/michelson/main.exe \
                -- test "^ticket operations diff"
    Subject:    Ticket scanner tests
*)

open Protocol
open Alpha_context
open Script_typed_ir

(** A local non-private type that mirrors
    [Ticket_operations_diff.ticket_token_diff]. *)
type ticket_token_diff = {
  ticket_token : Ticket_token.ex_token;
  total_amount : Script_int.n Script_int.num;
  destinations : (Destination.t * Script_int.n Script_int.num) list;
}

let to_local_ticket_token_diff
    {Ticket_operations_diff.ticket_token; total_amount; destinations} =
  {ticket_token; total_amount; destinations}

let ( let* ) m f = m >>=? f

let wrap m = m >|= Environment.wrap_tzresult

let big_map_updates_of_key_values ctxt key_values =
  List.fold_right_es
    (fun (key, value) (kvs, ctxt) ->
      let* (key_hash, ctxt) =
        wrap
          (Script_ir_translator.hash_comparable_data
             ctxt
             Script_typed_ir.int_t
             (Script_int.of_int key))
      in
      return
        ( {
            Big_map.key = Expr.from_string @@ string_of_int key;
            key_hash;
            value = Option.map Expr.from_string value;
          }
          :: kvs,
          ctxt ))
    key_values
    ([], ctxt)

let new_int_key_big_map ctxt contract ~value_type entries =
  let* (ctxt, big_map_id) = wrap @@ Big_map.fresh ~temporary:false ctxt in
  let key_type = Expr.from_string "int" in
  let value_type = Expr.from_string value_type in
  let* (updates, ctxt) =
    big_map_updates_of_key_values ctxt
    @@ List.map (fun (k, v) -> (k, Some v)) entries
  in
  let alloc =
    Lazy_storage.make
      Lazy_storage.Kind.Big_map
      big_map_id
      (Update
         {init = Lazy_storage.Alloc Big_map.{key_type; value_type}; updates})
  in
  let storage = Expr.from_string "{}" in
  let* ctxt =
    wrap @@ Contract.update_script_storage ctxt contract storage (Some [alloc])
  in
  return (big_map_id, ctxt)

let assert_equal_string_list ~loc msg =
  Assert.assert_equal_list ~loc String.equal msg Format.pp_print_string

let string_of_ticket_token ctxt
    (Ticket_token.Ex_token {ticketer; contents_type; contents}) =
  let* (x, _) =
    wrap
    @@ Script_ir_translator.unparse_comparable_data
         ctxt
         ~loc:()
         Script_ir_translator.Readable
         contents_type
         contents
  in
  return
  @@ Format.asprintf
       {|("%a", %a)|}
       Contract.pp
       ticketer
       Michelson_v1_printer.print_expr
       (Micheline.strip_locations x)

let string_of_destination_and_amounts cas =
  Format.asprintf
    "[%a]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
       (fun fmt (contract, amount) ->
         Format.fprintf
           fmt
           {|("%a", %s)|}
           Destination.pp
           contract
           (Script_int.to_string amount)))
    cas

let string_of_ticket_operations_diff ctxt
    {ticket_token; total_amount; destinations} =
  let* ticket_token = string_of_ticket_token ctxt ticket_token in
  let destinations = string_of_destination_and_amounts destinations in
  return
    (Printf.sprintf
       "(%s, %s, %s)"
       ticket_token
       (Script_int.to_string total_amount)
       destinations)

let assert_equal_ticket_token_diffs ctxt ~loc ticket_diffs
    ~(expected : ticket_token_diff list) =
  (* Sort destinations by contract and the strings alphabetically so that order
     does not matter for comparison. *)
  let sorted_strings ticket_diffs =
    List.map
      (fun {ticket_token; total_amount; destinations} ->
        {
          ticket_token;
          total_amount;
          destinations =
            List.sort
              (fun (c1, _) (c2, _) -> Destination.compare c1 c2)
              destinations;
        })
      ticket_diffs
    |> List.map_es (string_of_ticket_operations_diff ctxt)
    >|=? List.sort String.compare
  in
  let* exp_str_diffs = sorted_strings expected in
  let* str_diffs =
    sorted_strings @@ List.map to_local_ticket_token_diff ticket_diffs
  in
  assert_equal_string_list
    ~loc
    "Equal ticket-token-diffs"
    exp_str_diffs
    str_diffs

let string_token ~ticketer content =
  let contents =
    Result.value_f ~default:(fun _ -> assert false)
    @@ Script_string.of_string content
  in
  Ticket_token.Ex_token
    {ticketer; contents_type = Script_typed_ir.string_t; contents}

(** Initializes one address for operations and one baker. *)
let init ?tx_rollup_enable () =
  Context.init2 ?tx_rollup_enable ~consensus_threshold:0 ()
  >|=? fun (block, (src0, src1)) ->
  let baker = Context.Contract.pkh src0 in
  (baker, src1, block)

let originate block ~script ~storage ~src ~baker ~forges_tickets =
  let code = Expr.toplevel_from_string script in
  let storage = Expr.from_string storage in
  let script =
    Alpha_context.Script.{code = lazy_expr code; storage = lazy_expr storage}
  in
  let* (operation, destination) =
    Op.contract_origination (B block) src ~fee:(Test_tez.of_int 10) ~script
  in
  let* incr =
    Incremental.begin_construction ~policy:Block.(By_account baker) block
  in
  let* incr =
    Incremental.add_operation
      ?expect_failure:
        (if forges_tickets then Some (fun _ -> return ()) else None)
      incr
      operation
  in
  Incremental.finalize_block incr >|=? fun block -> (destination, script, block)

let two_ticketers block =
  let* ctxt =
    Incremental.begin_construction block >|=? Incremental.alpha_ctxt
  in
  let* cs = Lwt.map Result.ok @@ Contract.list ctxt in
  match cs with c1 :: c2 :: _ -> return (c1, c2) | _ -> assert false

let one_ticketer block = two_ticketers block >|=? fst

let nat n = Script_int.(abs @@ of_int n)

let origination_operation block ~src ~baker ~script ~storage ~forges_tickets =
  let* (orig_contract, script, block) =
    originate block ~script ~storage ~src ~baker ~forges_tickets
  in
  let* incr =
    Incremental.begin_construction ~policy:Block.(By_account baker) block
  in
  let ctxt = Incremental.alpha_ctxt incr in
  let* ( Script_ir_translator.Ex_script
           (Script
             {
               storage_type;
               storage;
               code = _;
               arg_type = _;
               event_type = _;
               views = _;
               entrypoints = _;
               code_size = _;
             }),
         ctxt ) =
    wrap
    @@ Script_ir_translator.parse_script
         ctxt
         ~legacy:true
         ~allow_forged_in_storage:true
         script
  in
  let operation =
    Script_typed_ir.Internal_operation
      {
        source = src;
        operation =
          Origination
            {
              origination = {delegate = None; script; credit = Tez.one};
              preorigination = orig_contract;
              storage_type;
              storage;
            };
        nonce = 1;
      }
  in
  let incr = Incremental.set_alpha_ctxt incr ctxt in
  return (orig_contract, operation, incr)

let delegation_operation ~src =
  Script_typed_ir.Internal_operation
    {source = src; operation = Delegation None; nonce = 1}

let originate block ~src ~baker ~script ~storage ~forges_tickets =
  let* (orig_contract, _script, block) =
    originate block ~script ~storage ~src ~baker ~forges_tickets
  in
  let* incr =
    Incremental.begin_construction ~policy:Block.(By_account baker) block
  in
  return (orig_contract, incr)

let transfer_operation ~incr ~src ~destination ~parameters_ty ~parameters =
  let open Lwt_result_syntax in
  let ctxt = Incremental.alpha_ctxt incr in
  let* (params_node, ctxt) =
    wrap
      (Script_ir_translator.unparse_data
         ctxt
         Script_ir_translator.Readable
         parameters_ty
         parameters)
  in
  let incr = Incremental.set_alpha_ctxt incr ctxt in
  return
    ( Script_typed_ir.Internal_operation
        {
          source = src;
          operation =
            Transaction
              {
                amount = Tez.zero;
                unparsed_parameters =
                  Script.lazy_expr @@ Micheline.strip_locations params_node;
                entrypoint = Entrypoint.default;
                destination = Destination.Contract destination;
                location = Micheline.dummy_location;
                parameters_ty;
                parameters;
              };
          nonce = 1;
        },
      incr )

let transfer_operation_to_tx_rollup ~incr ~src ~parameters_ty ~parameters
    ~tx_rollup =
  let open Lwt_result_syntax in
  let ctxt = Incremental.alpha_ctxt incr in
  let* (params_node, ctxt) =
    wrap
      (Script_ir_translator.unparse_data
         ctxt
         Script_ir_translator.Optimized_legacy
         parameters_ty
         parameters)
  in
  let incr = Incremental.set_alpha_ctxt incr ctxt in
  return
    ( Script_typed_ir.Internal_operation
        {
          source = src;
          operation =
            Transaction
              {
                amount = Tez.zero;
                unparsed_parameters =
                  Script.lazy_expr @@ Micheline.strip_locations params_node;
                entrypoint = Tx_rollup.deposit_entrypoint;
                destination = Destination.Tx_rollup tx_rollup;
                location = Micheline.dummy_location;
                parameters_ty;
                parameters;
              };
          nonce = 1;
        },
      incr )

let ticket_diffs_of_operations incr operations =
  wrap
  @@ Ticket_operations_diff.ticket_diffs_of_operations
       (Incremental.alpha_ctxt incr)
       operations

let unit_script =
  {|
      { parameter unit;
        storage unit;
        code { CAR; NIL operation ; PAIR } }
  |}

let ticket_list_script =
  {|
      { parameter (list (ticket string));
        storage (list (ticket string));
        code { CAR; NIL operation ; PAIR } }
  |}

let ticket_big_map_script =
  {|
      { parameter (big_map int (ticket string));
        storage (big_map int (ticket string));
        code { CAR; NIL operation ; PAIR } }
  |}

let list_ticket_string_ty =
  ticket_t Micheline.dummy_location string_t >>? fun ticket_ty ->
  list_t Micheline.dummy_location ticket_ty

let make_ticket (ticketer, contents, amount) =
  Script_string.of_string contents >>?= fun contents ->
  return {ticketer; contents; amount = nat amount}

let make_tickets ts =
  let* elements = List.map_es make_ticket ts in
  return {elements; length = List.length elements}

let transfer_tickets_operation ~incr ~src ~destination tickets =
  let open Lwt_result_syntax in
  let*? parameters_ty = Environment.wrap_tzresult list_ticket_string_ty in
  let* parameters = wrap @@ make_tickets tickets in
  transfer_operation ~incr ~src ~destination ~parameters_ty ~parameters

(** Test that no tickets are returned for operations that do not contain
    tickets. *)
let test_non_ticket_operations () =
  let* (_baker, src, block) = init () in
  let* incr = Incremental.begin_construction block in
  let operations = [delegation_operation ~src] in
  let* (ticket_diffs, ctxt) = ticket_diffs_of_operations incr operations in
  assert_equal_ticket_token_diffs ctxt ~loc:__LOC__ ticket_diffs ~expected:[]

(** Test transfer to a contract that does not take tickets. *)
let test_transfer_to_non_ticket_contract () =
  let* (baker, src, block) = init () in
  let* (orig_contract, incr) =
    originate
      block
      ~src
      ~baker
      ~script:unit_script
      ~storage:"Unit"
      ~forges_tickets:false
  in
  let* (operation, incr) =
    transfer_operation
      ~incr
      ~src
      ~destination:orig_contract
      ~parameters_ty:unit_t
      ~parameters:()
  in
  let* (ticket_diffs, ctxt) = ticket_diffs_of_operations incr [operation] in
  assert_equal_ticket_token_diffs ctxt ~loc:__LOC__ ticket_diffs ~expected:[]

(** Test transfer an empty list of tickets. *)
let test_transfer_empty_ticket_list () =
  let* (baker, src, block) = init () in
  let* (orig_contract, incr) =
    originate
      block
      ~src
      ~baker
      ~script:ticket_list_script
      ~storage:"{}"
      ~forges_tickets:false
  in
  let* (operation, incr) =
    transfer_tickets_operation ~incr ~src ~destination:orig_contract []
  in
  let* (ticket_diffs, ctxt) = ticket_diffs_of_operations incr [operation] in
  assert_equal_ticket_token_diffs ctxt ~loc:__LOC__ ticket_diffs ~expected:[]

(** Test transfer a list of one ticket. *)
let test_transfer_one_ticket () =
  let* (baker, src, block) = init () in
  let* ticketer = one_ticketer block in
  let* (orig_contract, incr) =
    originate
      block
      ~src
      ~baker
      ~script:ticket_list_script
      ~storage:"{}"
      ~forges_tickets:false
  in
  let* (operation, incr) =
    transfer_tickets_operation
      ~incr
      ~src
      ~destination:orig_contract
      [(ticketer, "white", 1)]
  in
  let* (ticket_diffs, ctxt) = ticket_diffs_of_operations incr [operation] in
  assert_equal_ticket_token_diffs
    ctxt
    ~loc:__LOC__
    ticket_diffs
    ~expected:
      [
        {
          ticket_token = string_token ~ticketer "white";
          total_amount = nat 1;
          destinations = [(Destination.Contract orig_contract, nat 1)];
        };
      ]

(** Test transfer a list of multiple tickets. *)
let test_transfer_multiple_tickets () =
  let* (baker, src, block) = init () in
  let* ticketer = one_ticketer block in
  let* (orig_contract, incr) =
    originate
      block
      ~src
      ~baker
      ~script:ticket_list_script
      ~storage:"{}"
      ~forges_tickets:false
  in
  let* (operation, incr) =
    transfer_tickets_operation
      ~incr
      ~src
      ~destination:orig_contract
      [
        (ticketer, "red", 1);
        (ticketer, "blue", 2);
        (ticketer, "green", 3);
        (ticketer, "red", 4);
      ]
  in
  let* (ticket_diffs, ctxt) = ticket_diffs_of_operations incr [operation] in
  assert_equal_ticket_token_diffs
    ctxt
    ~loc:__LOC__
    ticket_diffs
    ~expected:
      [
        {
          ticket_token = string_token ~ticketer "red";
          total_amount = nat 5;
          destinations = [(Destination.Contract orig_contract, nat 5)];
        };
        {
          ticket_token = string_token ~ticketer "blue";
          total_amount = nat 2;
          destinations = [(Destination.Contract orig_contract, nat 2)];
        };
        {
          ticket_token = string_token ~ticketer "green";
          total_amount = nat 3;
          destinations = [(Destination.Contract orig_contract, nat 3)];
        };
      ]

(** Test transfer a list of tickets of different types. *)
let test_transfer_different_tickets () =
  let* (baker, src, block) = init () in
  let* (ticketer1, ticketer2) = two_ticketers block in
  let* (destination, incr) =
    originate
      block
      ~src
      ~baker
      ~script:ticket_list_script
      ~storage:"{}"
      ~forges_tickets:false
  in
  let* (operation, incr) =
    transfer_tickets_operation
      ~incr
      ~src
      ~destination
      [
        (ticketer1, "red", 1);
        (ticketer1, "green", 1);
        (ticketer1, "blue", 1);
        (ticketer2, "red", 1);
        (ticketer2, "green", 1);
        (ticketer2, "blue", 1);
        (ticketer1, "red", 1);
        (ticketer1, "green", 1);
        (ticketer1, "blue", 1);
      ]
  in
  let* (ticket_diffs, ctxt) = ticket_diffs_of_operations incr [operation] in
  assert_equal_ticket_token_diffs
    ctxt
    ~loc:__LOC__
    ticket_diffs
    ~expected:
      [
        {
          ticket_token = string_token ~ticketer:ticketer1 "red";
          total_amount = nat 2;
          destinations = [(Destination.Contract destination, nat 2)];
        };
        {
          ticket_token = string_token ~ticketer:ticketer1 "green";
          total_amount = nat 2;
          destinations = [(Destination.Contract destination, nat 2)];
        };
        {
          ticket_token = string_token ~ticketer:ticketer1 "blue";
          total_amount = nat 2;
          destinations = [(Destination.Contract destination, nat 2)];
        };
        {
          ticket_token = string_token ~ticketer:ticketer2 "red";
          total_amount = nat 1;
          destinations = [(Destination.Contract destination, nat 1)];
        };
        {
          ticket_token = string_token ~ticketer:ticketer2 "green";
          total_amount = nat 1;
          destinations = [(Destination.Contract destination, nat 1)];
        };
        {
          ticket_token = string_token ~ticketer:ticketer2 "blue";
          total_amount = nat 1;
          destinations = [(Destination.Contract destination, nat 1)];
        };
      ]

(** Test transfer to two contracts with different types of tickets. *)
let test_transfer_to_two_contracts_with_different_tickets () =
  let* (baker, src, block) = init () in
  let* ticketer = one_ticketer block in
  let parameters =
    [(ticketer, "red", 1); (ticketer, "green", 1); (ticketer, "blue", 1)]
  in
  let* (destination1, incr) =
    originate
      block
      ~src
      ~baker
      ~script:ticket_list_script
      ~storage:"{}"
      ~forges_tickets:false
  in
  let* (operation1, incr) =
    transfer_tickets_operation ~incr ~src ~destination:destination1 parameters
  in
  let* block = Incremental.finalize_block incr in
  let* (destination2, incr) =
    originate
      block
      ~src
      ~baker
      ~script:ticket_list_script
      ~storage:"{}"
      ~forges_tickets:false
  in
  let* (operation2, incr) =
    transfer_tickets_operation ~incr ~src ~destination:destination2 parameters
  in
  let* (ticket_diffs, ctxt) =
    ticket_diffs_of_operations incr [operation1; operation2]
  in
  assert_equal_ticket_token_diffs
    ctxt
    ~loc:__LOC__
    ticket_diffs
    ~expected:
      [
        {
          ticket_token = string_token ~ticketer "red";
          total_amount = nat 2;
          destinations =
            [
              (Destination.Contract destination2, nat 1);
              (Destination.Contract destination1, nat 1);
            ];
        };
        {
          ticket_token = string_token ~ticketer "green";
          total_amount = nat 2;
          destinations =
            [
              (Destination.Contract destination2, nat 1);
              (Destination.Contract destination1, nat 1);
            ];
        };
        {
          ticket_token = string_token ~ticketer "blue";
          total_amount = nat 2;
          destinations =
            [
              (Destination.Contract destination2, nat 1);
              (Destination.Contract destination1, nat 1);
            ];
        };
      ]

(** Test originate a contract that does not contain tickets. *)
let test_originate_non_ticket_contract () =
  let* (baker, src, block) = init () in
  let* (_orig_contract, operation, incr) =
    origination_operation
      block
      ~src
      ~baker
      ~script:unit_script
      ~storage:"Unit"
      ~forges_tickets:false
  in
  let* (ticket_diffs, ctxt) = ticket_diffs_of_operations incr [operation] in
  assert_equal_ticket_token_diffs ctxt ~loc:__LOC__ ticket_diffs ~expected:[]

(** Test originate a contract with an empty list of tickets. *)
let test_originate_with_empty_tickets_list () =
  let* (baker, src, block) = init () in
  let storage = "{}" in
  let* (_orig_contract, operation, incr) =
    origination_operation
      block
      ~src
      ~baker
      ~script:ticket_list_script
      ~storage
      ~forges_tickets:false
  in
  let* (ticket_diffs, ctxt) = ticket_diffs_of_operations incr [operation] in
  assert_equal_ticket_token_diffs ctxt ~loc:__LOC__ ticket_diffs ~expected:[]

(** Test originate a contract with a single ticket. *)
let test_originate_with_one_ticket () =
  let* (baker, src, block) = init () in
  let* ticketer = one_ticketer block in
  let storage =
    Printf.sprintf {|{Pair %S "white" 1}|} (Contract.to_b58check ticketer)
  in
  let* (orig_contract, operation, ctxt) =
    origination_operation
      block
      ~src
      ~baker
      ~script:ticket_list_script
      ~storage
      ~forges_tickets:true
  in
  let* (ticket_diffs, ctxt) = ticket_diffs_of_operations ctxt [operation] in
  assert_equal_ticket_token_diffs
    ctxt
    ~loc:__LOC__
    ticket_diffs
    ~expected:
      [
        {
          ticket_token = string_token ~ticketer "white";
          total_amount = nat 1;
          destinations = [(Destination.Contract orig_contract, nat 1)];
        };
      ]

(** Test originate a contract with multiple tickets. *)
let test_originate_with_multiple_tickets () =
  let* (baker, src, block) = init () in
  let* ticketer = one_ticketer block in
  let storage =
    let ticketer_addr = Contract.to_b58check ticketer in
    Printf.sprintf
      {|{
        Pair %S "red" 1;
        Pair %S "blue" 2 ;
        Pair %S "green" 3;
        Pair %S "red" 4;}
      |}
      ticketer_addr
      ticketer_addr
      ticketer_addr
      ticketer_addr
  in
  let* (orig_contract, operation, ctxt) =
    origination_operation
      block
      ~src
      ~baker
      ~script:ticket_list_script
      ~storage
      ~forges_tickets:true
  in
  let* (ticket_diffs, ctxt) = ticket_diffs_of_operations ctxt [operation] in
  assert_equal_ticket_token_diffs
    ctxt
    ~loc:__LOC__
    ticket_diffs
    ~expected:
      [
        {
          ticket_token = string_token ~ticketer "red";
          total_amount = nat 5;
          destinations = [(Destination.Contract orig_contract, nat 5)];
        };
        {
          ticket_token = string_token ~ticketer "blue";
          total_amount = nat 2;
          destinations = [(Destination.Contract orig_contract, nat 2)];
        };
        {
          ticket_token = string_token ~ticketer "green";
          total_amount = nat 3;
          destinations = [(Destination.Contract orig_contract, nat 3)];
        };
      ]

(** Test originate a contract with multiple tickets of different types. *)
let test_originate_with_different_tickets () =
  let* (baker, src, block) = init () in
  let* (ticketer1, ticketer2) = two_ticketers block in
  let storage =
    let ticketer1_addr = Contract.to_b58check ticketer1 in
    let ticketer2_addr = Contract.to_b58check ticketer2 in
    Printf.sprintf
      {|{
        Pair %S "red" 1;
        Pair %S "green" 1;
        Pair %S "blue" 1;
        Pair %S "red" 1;
        Pair %S "green" 1;
        Pair %S "blue" 1 ;
        Pair %S "red" 1;
        Pair %S "green" 1;
        Pair %S "blue" 1; }
      |}
      ticketer1_addr
      ticketer1_addr
      ticketer1_addr
      ticketer2_addr
      ticketer2_addr
      ticketer2_addr
      ticketer1_addr
      ticketer1_addr
      ticketer1_addr
  in
  let* (orig_contract, operation, ctxt) =
    origination_operation
      block
      ~src
      ~baker
      ~script:ticket_list_script
      ~storage
      ~forges_tickets:true
  in
  let* (ticket_diffs, ctxt) = ticket_diffs_of_operations ctxt [operation] in
  assert_equal_ticket_token_diffs
    ctxt
    ~loc:__LOC__
    ticket_diffs
    ~expected:
      [
        {
          ticket_token = string_token ~ticketer:ticketer1 "red";
          total_amount = nat 2;
          destinations = [(Destination.Contract orig_contract, nat 2)];
        };
        {
          ticket_token = string_token ~ticketer:ticketer1 "green";
          total_amount = nat 2;
          destinations = [(Destination.Contract orig_contract, nat 2)];
        };
        {
          ticket_token = string_token ~ticketer:ticketer1 "blue";
          total_amount = nat 2;
          destinations = [(Destination.Contract orig_contract, nat 2)];
        };
        {
          ticket_token = string_token ~ticketer:ticketer2 "red";
          total_amount = nat 1;
          destinations = [(Destination.Contract orig_contract, nat 1)];
        };
        {
          ticket_token = string_token ~ticketer:ticketer2 "green";
          total_amount = nat 1;
          destinations = [(Destination.Contract orig_contract, nat 1)];
        };
        {
          ticket_token = string_token ~ticketer:ticketer2 "blue";
          total_amount = nat 1;
          destinations = [(Destination.Contract orig_contract, nat 1)];
        };
      ]

(** Test originate two contracts with multiple tickets of different types. *)
let test_originate_two_contracts_with_different_tickets () =
  let* (baker, src, block) = init () in
  let* ticketer = one_ticketer block in
  let storage =
    let ticketer_addr = Contract.to_b58check ticketer in
    Printf.sprintf
      {|{Pair %S "red" 1; Pair %S "green" 1; Pair %S "blue" 1; }|}
      ticketer_addr
      ticketer_addr
      ticketer_addr
  in
  let* (orig_contract1, operation1, incr) =
    origination_operation
      block
      ~src
      ~baker
      ~script:ticket_list_script
      ~storage
      ~forges_tickets:true
  in
  let* block = Incremental.finalize_block incr in
  let* (orig_contract2, operations2, incr) =
    origination_operation
      block
      ~src
      ~baker
      ~script:ticket_list_script
      ~storage
      ~forges_tickets:true
  in
  let* (ticket_diffs, ctxt) =
    ticket_diffs_of_operations incr [operation1; operations2]
  in
  assert_equal_ticket_token_diffs
    ctxt
    ~loc:__LOC__
    ticket_diffs
    ~expected:
      [
        {
          ticket_token = string_token ~ticketer "red";
          total_amount = nat 2;
          destinations =
            [
              (Destination.Contract orig_contract2, nat 1);
              (Destination.Contract orig_contract1, nat 1);
            ];
        };
        {
          ticket_token = string_token ~ticketer "green";
          total_amount = nat 2;
          destinations =
            [
              (Destination.Contract orig_contract2, nat 1);
              (Destination.Contract orig_contract1, nat 1);
            ];
        };
        {
          ticket_token = string_token ~ticketer "blue";
          total_amount = nat 2;
          destinations =
            [
              (Destination.Contract orig_contract2, nat 1);
              (Destination.Contract orig_contract1, nat 1);
            ];
        };
      ]

(** Test originate and transfer tickets. *)
let test_originate_and_transfer () =
  let* (baker, src, block) = init () in
  let* ticketer = one_ticketer block in
  let ticketer_addr = Contract.to_b58check ticketer in
  let storage =
    Printf.sprintf
      {|{Pair %S "red" 1; Pair %S "green" 1; Pair %S "blue" 1; }|}
      ticketer_addr
      ticketer_addr
      ticketer_addr
  in
  let* (orig_contract1, operation1, incr) =
    origination_operation
      block
      ~src
      ~baker
      ~script:ticket_list_script
      ~storage
      ~forges_tickets:true
  in
  let* block = Incremental.finalize_block incr in
  let* (destination2, incr) =
    originate
      block
      ~src
      ~baker
      ~script:ticket_list_script
      ~storage:"{}"
      ~forges_tickets:false
  in
  let* (operation2, incr) =
    transfer_tickets_operation
      ~incr
      ~src
      ~destination:destination2
      [(ticketer, "red", 1); (ticketer, "green", 1); (ticketer, "blue", 1)]
  in
  let* (ticket_diffs, ctxt) =
    ticket_diffs_of_operations incr [operation1; operation2]
  in
  assert_equal_ticket_token_diffs
    ctxt
    ~loc:__LOC__
    ticket_diffs
    ~expected:
      [
        {
          ticket_token = string_token ~ticketer "red";
          total_amount = nat 2;
          destinations =
            [
              (Destination.Contract destination2, nat 1);
              (Destination.Contract orig_contract1, nat 1);
            ];
        };
        {
          ticket_token = string_token ~ticketer "green";
          total_amount = nat 2;
          destinations =
            [
              (Destination.Contract destination2, nat 1);
              (Destination.Contract orig_contract1, nat 1);
            ];
        };
        {
          ticket_token = string_token ~ticketer "blue";
          total_amount = nat 2;
          destinations =
            [
              (Destination.Contract destination2, nat 1);
              (Destination.Contract orig_contract1, nat 1);
            ];
        };
      ]

(** Test originate a contract with a big-map with tickets inside. *)
let test_originate_big_map_with_tickets () =
  let* (baker, ticketer, block) = init () in
  let* (operation, originated) =
    Op.contract_origination (B block) ticketer ~script:Op.dummy_script
  in
  let* block = Block.bake ~operation block in
  let* incr = Incremental.begin_construction block in
  let ticketer_addr = Contract.to_b58check ticketer in
  let* (big_map_id, ctxt) =
    new_int_key_big_map
      (Incremental.alpha_ctxt incr)
      originated
      ~value_type:"ticket string"
      [
        (1, Printf.sprintf {|Pair %S "red" 1|} ticketer_addr);
        (2, Printf.sprintf {|Pair %S "green" 1|} ticketer_addr);
        (3, Printf.sprintf {|Pair %S "blue" 1|} ticketer_addr);
      ]
  in
  let incr = Incremental.set_alpha_ctxt incr ctxt in
  let* block = Incremental.finalize_block incr in
  let* (orig_contract, operation, incr) =
    let storage =
      Printf.sprintf "%d" @@ Z.to_int (Big_map.Id.unparse_to_z big_map_id)
    in
    origination_operation
      block
      ~src:ticketer
      ~baker
      ~script:ticket_big_map_script
      ~storage
      ~forges_tickets:true
  in
  let* (ticket_diffs, ctxt) = ticket_diffs_of_operations incr [operation] in
  assert_equal_ticket_token_diffs
    ctxt
    ~loc:__LOC__
    ticket_diffs
    ~expected:
      [
        {
          ticket_token = string_token ~ticketer "red";
          total_amount = nat 1;
          destinations = [(Destination.Contract orig_contract, nat 1)];
        };
        {
          ticket_token = string_token ~ticketer "green";
          total_amount = nat 1;
          destinations = [(Destination.Contract orig_contract, nat 1)];
        };
        {
          ticket_token = string_token ~ticketer "blue";
          total_amount = nat 1;
          destinations = [(Destination.Contract orig_contract, nat 1)];
        };
      ]

(** Test transfer a big-map with tickets. *)
let test_transfer_big_map_with_tickets () =
  let* (baker, ticketer_contract, block) = init () in
  let* (operation, originated) =
    Op.contract_origination (B block) ticketer_contract ~script:Op.dummy_script
  in
  let* block = Block.bake ~operation block in
  let* incr = Incremental.begin_construction block in
  let ticketer_addr = Contract.to_b58check ticketer_contract in
  let* (big_map_id, ctxt) =
    new_int_key_big_map
      (Incremental.alpha_ctxt incr)
      originated
      ~value_type:"ticket string"
      [
        (1, Printf.sprintf {|Pair %S "red" 1|} ticketer_addr);
        (2, Printf.sprintf {|Pair %S "green" 1|} ticketer_addr);
        (3, Printf.sprintf {|Pair %S "blue" 1|} ticketer_addr);
      ]
  in
  let incr = Incremental.set_alpha_ctxt incr ctxt in
  let* block = Incremental.finalize_block incr in
  let* (orig_contract, incr) =
    originate
      block
      ~src:ticketer_contract
      ~baker
      ~script:ticket_big_map_script
      ~storage:"{}"
      ~forges_tickets:false
  in
  let open Lwt_result_syntax in
  let*? value_type =
    Environment.wrap_tzresult @@ ticket_t Micheline.dummy_location string_t
  in
  let*? parameters_ty =
    Environment.wrap_tzresult
    @@ big_map_t Micheline.dummy_location int_t value_type
  in
  let parameters =
    Big_map
      {
        id = Some big_map_id;
        diff = {map = Big_map_overlay.empty; size = 0};
        key_type = int_t;
        value_type;
      }
  in
  let* (operation, incr) =
    transfer_operation
      ~incr
      ~src:ticketer_contract
      ~destination:orig_contract
      ~parameters_ty
      ~parameters
  in
  let* (ticket_diffs, ctxt) = ticket_diffs_of_operations incr [operation] in
  assert_equal_ticket_token_diffs
    ctxt
    ~loc:__LOC__
    ticket_diffs
    ~expected:
      [
        {
          ticket_token = string_token ~ticketer:ticketer_contract "red";
          total_amount = nat 1;
          destinations = [(Destination.Contract orig_contract, nat 1)];
        };
        {
          ticket_token = string_token ~ticketer:ticketer_contract "green";
          total_amount = nat 1;
          destinations = [(Destination.Contract orig_contract, nat 1)];
        };
        {
          ticket_token = string_token ~ticketer:ticketer_contract "blue";
          total_amount = nat 1;
          destinations = [(Destination.Contract orig_contract, nat 1)];
        };
      ]

(** Test transfer a ticket to a tx_rollup. *)
let test_tx_rollup_deposit_one_ticket () =
  let open Lwt_result_syntax in
  let* (_baker, src, block) = init ~tx_rollup_enable:true () in
  let* ticketer = one_ticketer block in
  let* incr = Incremental.begin_construction block in
  let* (operation, tx_rollup) =
    Op.tx_rollup_origination (I incr) src ~fee:(Test_tez.of_int 10)
  in
  let* incr = Incremental.add_operation incr operation in

  let*? ticket_ty =
    Script_typed_ir.(ticket_t Micheline.dummy_location string_t)
    |> Environment.wrap_tzresult
  in
  let*? (Ty_ex_c parameters_ty) =
    Script_typed_ir.(
      pair_t Micheline.dummy_location ticket_ty tx_rollup_l2_address_t)
    |> Environment.wrap_tzresult
  in
  let amount =
    Script_int.(is_nat @@ of_int 1) |> WithExceptions.Option.get ~loc:__LOC__
  in
  let*? contents =
    Script_string.of_string "white" |> Environment.wrap_tzresult
  in
  let l2_destination =
    Indexable.value
    @@ Tx_rollup_l2_address.of_b58check_exn
         "tz4MSfZsn6kMDczShy8PMeB628TNukn9hi2K"
  in
  let parameters =
    (Script_typed_ir.{ticketer; contents; amount}, l2_destination)
  in

  let* (operation, incr) =
    transfer_operation_to_tx_rollup
      ~incr
      ~src
      ~tx_rollup
      ~parameters_ty
      ~parameters
  in
  let* (ticket_diffs, ctxt) = ticket_diffs_of_operations incr [operation] in
  assert_equal_ticket_token_diffs
    ctxt
    ~loc:__LOC__
    ticket_diffs
    ~expected:
      [
        {
          ticket_token = string_token ~ticketer "white";
          total_amount = nat 1;
          destinations = [(Destination.Tx_rollup tx_rollup, nat 1)];
        };
      ]

let tests =
  [
    Tztest.tztest
      "Test operations that do not involve tickets"
      `Quick
      test_non_ticket_operations;
    Tztest.tztest
      "Test transfer to non-ticket contract"
      `Quick
      test_transfer_to_non_ticket_contract;
    Tztest.tztest
      "Test transfer empty ticket list"
      `Quick
      test_transfer_empty_ticket_list;
    Tztest.tztest "Test transfer one ticket" `Quick test_transfer_one_ticket;
    Tztest.tztest
      "Test transfer multiple tickets"
      `Quick
      test_transfer_multiple_tickets;
    Tztest.tztest
      "Test transfer different tickets"
      `Quick
      test_transfer_different_tickets;
    Tztest.tztest
      "Test transfer to two contracts with different tickets"
      `Quick
      test_transfer_to_two_contracts_with_different_tickets;
    Tztest.tztest
      "Test originate contract that does not contain tickets"
      `Quick
      test_originate_non_ticket_contract;
    Tztest.tztest
      "Test originate with empty ticket list"
      `Quick
      test_originate_with_empty_tickets_list;
    Tztest.tztest
      "Test originate with one ticket"
      `Quick
      test_originate_with_one_ticket;
    Tztest.tztest
      "Test originate with multiple tickets"
      `Quick
      test_originate_with_multiple_tickets;
    Tztest.tztest
      "Test originate with different tickets"
      `Quick
      test_originate_with_different_tickets;
    Tztest.tztest
      "Test originate two contracts with different tickets"
      `Quick
      test_originate_two_contracts_with_different_tickets;
    Tztest.tztest
      "Test originate and transfer"
      `Quick
      test_originate_and_transfer;
    Tztest.tztest
      "Test originate big-map with tickets"
      `Quick
      test_originate_big_map_with_tickets;
    Tztest.tztest
      "Test transfer big-map with tickets"
      `Quick
      test_transfer_big_map_with_tickets;
    Tztest.tztest
      "Testt tx rollup deposit one ticket"
      `Quick
      test_tx_rollup_deposit_one_ticket;
  ]
