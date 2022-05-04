(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Clic

let l1_destination_parameter =
  parameter (fun _ s ->
      match Signature.Public_key_hash.of_b58check_opt s with
      | Some addr -> return addr
      | None -> failwith "cannot parse %s to get a valid destination" s)

let l2_destination_parameter =
  let open Lwt_result_syntax in
  parameter (fun _ s ->
      match Tx_rollup_l2_address.of_b58check_opt s with
      | Some pkh -> return pkh
      | None -> failwith "cannot parse %s to get a valid destination" s)

let parse_file parse path =
  Lwt_utils_unix.read_file path >>= fun contents -> parse contents

let file_or_text_parameter ~from_text
    ?(from_path = parse_file (from_text ~heuristic:false)) () =
  Clic.parameter @@ fun _ p ->
  match String.split ~limit:1 ':' p with
  | ["text"; text] -> from_text ~heuristic:false text
  | ["file"; path] -> from_path path
  | _ -> if Sys.file_exists p then from_path p else from_text ~heuristic:true p

let json_file_or_text_parameter =
  let from_text ~heuristic s =
    try return (Ezjsonm.from_string s)
    with Ezjsonm.Parse_error _ when heuristic ->
      failwith "Neither an existing file nor valid JSON: '%s'" s
  in
  let from_path = Lwt_utils_unix.Json.read_file in
  file_or_text_parameter ~from_text ~from_path ()

let block_id_param =
  let open Lwt_result_syntax in
  parameter (fun _ s ->
      match RPC.destruct_block_id s with
      | Ok v -> return v
      | Error e -> failwith "%s" e)

let secret_key_parameter =
  let open Lwt_result_syntax in
  parameter (fun _ s ->
      match Bls.Secret_key.of_b58check_opt s with
      | Some sk -> return sk
      | None -> failwith "cannot parse %s to get a valid BLS secret key" s)

let signer_parameter =
  let open Lwt_result_syntax in
  parameter (fun _ s ->
      match Tx_rollup_l2_address.of_b58check_opt s with
      | Some pkh -> return @@ Tx_rollup_l2_batch.L2_addr pkh
      | None -> (
          match Bls.Public_key.of_b58check_opt s with
          | Some pk -> return @@ Tx_rollup_l2_batch.Bls_pk pk
          | None -> failwith "cannot parse %s to get a valid signer" s))

let ticket_hash_parameter =
  let open Lwt_result_syntax in
  parameter (fun _ s ->
      match Alpha_context.Ticket_hash.of_b58check_opt s with
      | Some tkh -> return tkh
      | None -> failwith "cannot parse %s to get a valid ticket_hash" s)

let get_tx_address_balance_command () =
  let open Lwt_result_syntax in
  command
    ~desc:"returns the balance associated to a given tz4 address and ticket"
    (args1
       (default_arg
          ~long:"block"
          ~placeholder:"block"
          ~doc:"block from which the balance is expected"
          ~default:"head"
          block_id_param))
    (prefixes ["get"; "balance"; "for"]
    @@ param
         ~name:"tz4"
         ~desc:"tz4 address from which the balance is queried"
         l2_destination_parameter
    @@ prefixes ["of"]
    @@ param
         ~name:"ticket-hash"
         ~desc:"ticket from which the balance is expected"
         ticket_hash_parameter
    @@ stop)
    (fun block tz4 ticket (cctxt : #Configuration.tx_client_context) ->
      let* value = RPC.balance cctxt block ticket tz4 in
      let*! () = cctxt#message "@[%a@]" Tx_rollup_l2_qty.pp value in
      return_unit)

let get_tx_inbox () =
  let open Lwt_result_syntax in
  command
    ~desc:"returns the inbox for a given block identifier"
    no_options
    (prefixes ["get"; "inbox"; "for"]
    @@ param
         ~name:"block"
         ~desc:"block from which the inbox is requested"
         block_id_param
    @@ stop)
    (fun () block (cctxt : #Configuration.tx_client_context) ->
      let* inbox = RPC.inbox cctxt block in
      let json = Data_encoding.(Json.construct (option Inbox.encoding)) inbox in
      let*! () = cctxt#message "@[%s@]" (Data_encoding.Json.to_string json) in
      return_unit)

let get_tx_block () =
  let open Lwt_result_syntax in
  command
    ~desc:"returns the tx rollup block for a given block identifier"
    no_options
    (prefixes ["get"; "block"]
    @@ param ~name:"block" ~desc:"block requested" block_id_param
    @@ stop)
    (fun () block (cctxt : #Configuration.tx_client_context) ->
      let* block = RPC.block cctxt block in
      let json =
        Data_encoding.(Json.construct (option RPC.Encodings.block)) block
      in
      let*! () = cctxt#message "@[%s@]" (Data_encoding.Json.to_string json) in
      return_unit)

let sign_transaction sks txs =
  let buf =
    Data_encoding.Binary.to_bytes_exn
      Tx_rollup_l2_batch.V1.transaction_encoding
      txs
  in
  List.map (fun sk -> Bls.sign sk buf) sks

let craft_transfers ~counter ~signer transfers =
  let contents =
    List.map
      (fun (qty, destination, ticket_hash) ->
        Tx_rollup_l2_batch.V1.(
          Transfer
            {
              destination = Indexable.from_value destination;
              ticket_hash = Indexable.from_value ticket_hash;
              qty;
            }))
      transfers
  in
  let signer = Indexable.from_value signer in
  Tx_rollup_l2_batch.V1.{signer; counter; contents}

let craft_tx ~counter ~signer ~destination ~ticket_hash ~qty =
  craft_transfers ~counter ~signer [(qty, destination, ticket_hash)]

let craft_withdraw ~counter ~signer ~destination ~ticket_hash ~qty =
  let content =
    Tx_rollup_l2_batch.V1.(Withdraw {destination; ticket_hash; qty})
  in
  let signer = Indexable.from_value signer in
  Tx_rollup_l2_batch.V1.{signer; counter; contents = [content]}

let aggregate_signature_exn signatures =
  match Bls.aggregate_signature_opt signatures with
  | Some res -> res
  | None -> invalid_arg "aggregate_signature_exn"

let batch signatures contents =
  let open Tx_rollup_l2_batch.V1 in
  let aggregated_signature = aggregate_signature_exn signatures in
  {aggregated_signature; contents}

let craft_batch
    (transactions : ('signer, 'content) Tx_rollup_l2_batch.V1.transaction list)
    sks =
  let signatures =
    List.map2
      ~when_different_lengths:()
      (fun txs sk -> sign_transaction sk txs)
      transactions
      sks
    |> (function
         | Ok r -> r
         | Error () ->
             (* assumed valid thanks to preconditions *)
             assert false)
    |> List.concat
  in
  batch signatures transactions

let conv_qty =
  parameter (fun _ qty ->
      match Tx_rollup_l2_qty.of_string qty with
      | Some qty -> return qty
      | None -> failwith "The given qty is invalid")

let conv_counter = parameter (fun _ counter -> return (Int64.of_string counter))

let signer_to_address : Tx_rollup_l2_batch.signer -> Tx_rollup_l2_address.t =
  function
  | Bls_pk pk -> Tx_rollup_l2_address.of_bls_pk pk
  | L2_addr addr -> addr

let signer_next_counter cctxt signer counter =
  let open Lwt_result_syntax in
  match counter with
  | Some counter -> return counter
  | None ->
      let+ counter = RPC.counter cctxt `Head (signer_to_address signer) in
      Int64.succ counter

let craft_tx_transfers () =
  let open Lwt_result_syntax in
  command
    ~desc:"WIP: craft a transaction with transfers"
    (args1
       (arg
          ~long:"counter"
          ~placeholder:"counter"
          ~doc:"counter value of the destination"
          conv_counter))
    (prefixes ["craft"; "tx"; "transfers"; "from"]
    @@ param
         ~name:"source"
         ~desc:"A BLS public key or a BLS public key hash"
         signer_parameter
    @@ prefix "using"
    @@ param
         ~name:"transfers.json"
         ~desc:
           "List of transfers from the signer in JSON format (from a file or \
            directly inlined). The input JSON must be an array of objects of \
            the form '[ {\"destination\": dst, \"qty\" : val, \"ticket\" : \
            ticket_hash} ]'"
         json_file_or_text_parameter
    @@ stop)
    (fun counter
         signer
         transfers_json
         (cctxt : #Configuration.tx_client_context) ->
      let transfers_encoding =
        let open Data_encoding in
        let transfer_encoding =
          obj3
            (req "qty" string)
            (req "destination" string)
            (req "ticket" string)
        in
        list transfer_encoding
      in
      match Data_encoding.Json.destruct transfers_encoding transfers_json with
      | [] -> failwith "Empty transfer list"
      | transfers ->
          let transfers =
            List.map
              (fun (qty, destination, ticket) ->
                ( Int64.of_string qty |> Tx_rollup_l2_qty.of_int64_exn,
                  Tx_rollup_l2_address.of_b58check_exn destination,
                  Alpha_context.Ticket_hash.of_b58check_exn ticket ))
              transfers
          in
          let* counter = signer_next_counter cctxt signer counter in
          let op = craft_transfers ~counter ~signer transfers in
          let json =
            Data_encoding.Json.construct
              (Data_encoding.list Tx_rollup_l2_batch.V1.transaction_encoding)
              [[op]]
          in
          cctxt#message "@[%a@]" Data_encoding.Json.pp json >>= fun () ->
          return_unit)

let craft_tx_transaction () =
  let open Lwt_result_syntax in
  command
    ~desc:"WIP: craft a transaction"
    (args1
       (arg
          ~long:"counter"
          ~placeholder:"counter"
          ~doc:"counter value of the destination"
          conv_counter))
    (prefixes ["craft"; "tx"; "transferring"]
    @@ param ~name:"qty" ~desc:"qty to transfer" conv_qty
    @@ prefixes ["from"]
    @@ param
         ~name:"source"
         ~desc:"A BLS public key or a BLS public key hash"
         signer_parameter
    @@ prefixes ["to"]
    @@ param
         ~name:"dest"
         ~desc:"tz4 destination address"
         l2_destination_parameter
    @@ prefixes ["for"]
    @@ param ~name:"ticket" ~desc:"ticket to transfer" ticket_hash_parameter
    @@ stop)
    (fun counter
         qty
         signer
         destination
         ticket_hash
         (cctxt : #Configuration.tx_client_context) ->
      let* counter = signer_next_counter cctxt signer counter in
      let op = craft_tx ~counter ~signer ~destination ~ticket_hash ~qty in
      let json =
        Data_encoding.Json.construct
          (Data_encoding.list Tx_rollup_l2_batch.V1.transaction_encoding)
          [[op]]
      in
      let*! () = cctxt#message "@[%a@]" Data_encoding.Json.pp json in
      return_unit)

let craft_tx_withdrawal () =
  let open Lwt_result_syntax in
  command
    ~desc:"WIP: craft a withdrawal from L2 to L1"
    (args1
       (arg
          ~long:"counter"
          ~placeholder:"counter"
          ~doc:"counter value of the destination"
          conv_counter))
    (prefixes ["craft"; "tx"; "withdrawing"]
    @@ param ~name:"qty" ~desc:"qty to withdraw" conv_qty
    @@ prefixes ["from"]
    @@ param
         ~name:"source"
         ~desc:"A BLS public key or a BLS public key hash"
         signer_parameter
    @@ prefixes ["to"]
    @@ param
         ~name:"dest"
         ~desc:"L1 destination address"
         l1_destination_parameter
    @@ prefixes ["for"]
    @@ param ~name:"ticket" ~desc:"ticket to withdraw" ticket_hash_parameter
    @@ stop)
    (fun counter
         qty
         signer
         destination
         ticket_hash
         (cctxt : #Configuration.tx_client_context) ->
      let* counter = signer_next_counter cctxt signer counter in
      let op = craft_withdraw ~counter ~signer ~destination ~ticket_hash ~qty in
      let json =
        Data_encoding.Json.construct
          (Data_encoding.list Tx_rollup_l2_batch.V1.transaction_encoding)
          [[op]]
      in
      let*! () = cctxt#message "@[%a@]" Data_encoding.Json.pp json in
      return_unit)

let batch_of_json ~txs:tx_json ~sks:sks_json =
  let transactions =
    Data_encoding.(
      Json.destruct (list Tx_rollup_l2_batch.V1.transaction_encoding) tx_json)
  in
  let sks =
    Data_encoding.(Json.destruct (list (list Bls.Secret_key.encoding)) sks_json)
  in
  craft_batch transactions sks

let craft_tx_batch () =
  let open Lwt_result_syntax in
  command
    ~desc:"craft a transactional rollup batch"
    no_options
    (prefixes ["craft"; "batch"; "with"]
    @@ string ~name:"json" ~desc:"JSON containing a list of operations"
    @@ prefixes ["for"]
    @@ string
         ~name:"secret-keys"
         ~desc:"[UNSAFE] JSON list of secret keys to sign"
    @@ stop)
    (fun () json_str sks_str (cctxt : #Configuration.tx_client_context) ->
      let* txs =
        match Data_encoding.Json.from_string json_str with
        | Ok json -> return json
        | Error _ -> failwith "Cannot decode the given json"
      in
      let* sks =
        match Data_encoding.Json.from_string sks_str with
        | Ok json -> return json
        | Error _ -> failwith "Cannot decode the given json"
      in
      let batch = batch_of_json ~txs ~sks in
      let json =
        Data_encoding.Json.construct
          Tx_rollup_l2_batch.encoding
          (Tx_rollup_l2_batch.V1 batch)
      in
      let*! () = cctxt#message "@[%a@]" Data_encoding.Json.pp json in
      return_unit)

let get_batcher_queue () =
  let open Lwt_result_syntax in
  command
    ~desc:"returns the batcher's queue of pending operations"
    no_options
    (prefixes ["get"; "batcher"; "queue"] @@ stop)
    (fun () (cctxt : #Configuration.tx_client_context) ->
      let* queue = RPC.get_queue cctxt in
      let json =
        Data_encoding.(Json.construct (list L2_transaction.encoding) queue)
      in
      let*! () = cctxt#message "@[%s@]" (Data_encoding.Json.to_string json) in
      return_unit)

let valid_transaction_hash =
  parameter (fun _ s ->
      match L2_transaction.Hash.of_b58check_opt s with
      | Some addr -> return addr
      | None -> failwith "The L2 transaction hash is invalid")

let get_batcher_transaction () =
  let open Lwt_result_syntax in
  command
    ~desc:"returns a batcher transaction for a given hash"
    no_options
    (prefixes ["get"; "batcher"; "transaction"]
    @@ param
         ~name:"hash"
         ~desc:"requested transaction hash"
         valid_transaction_hash
    @@ stop)
    (fun () hash (cctxt : #Configuration.tx_client_context) ->
      let* tx = RPC.get_transaction cctxt hash in
      let json =
        Data_encoding.(Json.construct (option L2_transaction.encoding)) tx
      in
      let*! () = cctxt#message "@[%s@]" (Data_encoding.Json.to_string json) in
      return_unit)

let inject_batcher_transaction () =
  let open Lwt_result_syntax in
  command
    ~desc:"injects the given transaction into the batcher's transaction queue"
    no_options
    (prefixes ["inject"; "batcher"; "transaction"]
    @@ string
         ~name:"signed_tx_json"
         ~desc:"injects a signed transaction into the batcher"
    @@ stop)
    (fun () signed_tx_json (cctxt : #Configuration.tx_client_context) ->
      let* json =
        match Data_encoding.Json.from_string signed_tx_json with
        | Ok json -> return json
        | Error _ -> failwith "cannot decode signed transactions"
      in
      let signed_tx =
        Data_encoding.Json.destruct L2_transaction.encoding json
      in
      let* txh = RPC.inject_transaction cctxt signed_tx in
      let json =
        Data_encoding.(Json.construct L2_transaction.Hash.encoding txh)
      in
      let*! () = cctxt#message "@[%s@]" (Data_encoding.Json.to_string json) in
      return_unit)

let transfer () =
  let open Lwt_result_syntax in
  command
    ~desc:"submit a layer-2 transfer to a rollup node’s batcher"
    (args2
       (arg
          ~long:"secret-key"
          ~placeholder:"sk"
          ~doc:"A BLS secret key"
          secret_key_parameter)
       (arg
          ~long:"counter"
          ~short:'c'
          ~placeholder:"counter"
          ~doc:"The counter associated to the signer address"
          conv_counter))
    (prefix "transfer"
    @@ param ~name:"qty" ~desc:"qty to transfer" conv_qty
    @@ prefix "of"
    @@ param ~name:"ticket" ~desc:"A ticket hash" ticket_hash_parameter
    @@ prefix "from"
    @@ param
         ~name:"source"
         ~desc:"A BLS public key or a BLS public key hash"
         signer_parameter
    @@ prefix "to"
    @@ param
         ~name:"destination"
         ~desc:"A BLS public key"
         l2_destination_parameter
    @@ stop)
    (fun (sk, counter) qty ticket_hash signer destination cctxt ->
      let open Tx_rollup_l2_batch.V1 in
      let* sk =
        match sk with
        | Some sk -> return sk
        | None -> failwith "Missing secret key argument"
      in
      let* counter = signer_next_counter cctxt signer counter in
      (* TODO/TORU: https://gitlab.com/tezos/tezos/-/issues/2903
         Use an RPC to know whether or not it can be safely replaced by
         an index. *)
      let signer = Indexable.from_value signer in
      (* TODO/TORU: https://gitlab.com/tezos/tezos/-/issues/2903
         Use an RPC to know whether or not it can be safely replaced by
         an index. *)
      let destination = Indexable.from_value destination in
      (* TODO/TORU: https://gitlab.com/tezos/tezos/-/issues/2903
         Use an RPC to know whether or not it can be safely replaced by
         an index. *)
      let ticket_hash = Indexable.from_value ticket_hash in
      let contents = [Transfer {destination; ticket_hash; qty}] in
      let operation = Tx_rollup_l2_batch.V1.{counter; signer; contents} in
      let transaction = [operation] in
      let* signature =
        match
          Bls.aggregate_signature_opt @@ sign_transaction [sk] transaction
        with
        | Some signature -> return signature
        | None -> failwith "Could not aggregate signatures together"
      in
      let* hash = RPC.inject_transaction cctxt {transaction; signature} in
      let*! () =
        cctxt#message "Transaction hash: %a" L2_transaction.Hash.pp hash
      in
      return_unit)

let withdraw () =
  let open Lwt_result_syntax in
  command
    ~desc:"submit a layer-2 withdraw to a rollup node’s batcher"
    (args2
       (arg
          ~long:"secret-key"
          ~placeholder:"sk"
          ~doc:"A BLS secret key"
          secret_key_parameter)
       (arg
          ~long:"counter"
          ~short:'c'
          ~placeholder:"counter"
          ~doc:"The counter associated to the signer address"
          conv_counter))
    (prefix "withdraw"
    @@ param ~name:"qty" ~desc:"qty to transfer" conv_qty
    @@ prefix "of"
    @@ param ~name:"ticket" ~desc:"A ticket hash" ticket_hash_parameter
    @@ prefix "from"
    @@ param
         ~name:"source"
         ~desc:"A BLS public key or a BLS public key hash"
         signer_parameter
    @@ prefix "to"
    @@ param
         ~name:"destination"
         ~desc:"A L1 public key hash"
         l1_destination_parameter
    @@ stop)
    (fun (sk, counter) qty ticket_hash signer destination cctxt ->
      let open Tx_rollup_l2_batch.V1 in
      let* sk =
        match sk with
        | Some sk -> return sk
        | None -> failwith "Missing secret key argument"
      in
      let* counter = signer_next_counter cctxt signer counter in
      (* TODO/TORU: https://gitlab.com/tezos/tezos/-/issues/2903
         Use an RPC to know whether or not it can be safely replaced by
         an index. *)
      let signer = Indexable.from_value signer in
      let contents = [Withdraw {destination; ticket_hash; qty}] in
      let operation = Tx_rollup_l2_batch.V1.{counter; signer; contents} in
      let transaction = [operation] in
      let* signature =
        match
          Bls.aggregate_signature_opt @@ sign_transaction [sk] transaction
        with
        | Some signature -> return signature
        | None -> failwith "Could not aggregate signatures together"
      in
      let* hash = RPC.inject_transaction cctxt {transaction; signature} in
      let*! () =
        cctxt#message "Transaction hash: %a" L2_transaction.Hash.pp hash
      in
      return_unit)

let all () =
  [
    get_tx_address_balance_command ();
    get_tx_inbox ();
    get_tx_block ();
    craft_tx_transaction ();
    craft_tx_transfers ();
    craft_tx_withdrawal ();
    craft_tx_batch ();
    get_batcher_queue ();
    get_batcher_transaction ();
    inject_batcher_transaction ();
    transfer ();
    withdraw ();
  ]
