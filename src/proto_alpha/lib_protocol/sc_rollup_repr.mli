(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** The basic components of an optimistic rollup for smart-contracts. *)

(**

   An optimistic rollup for smart-contracts is made of two main
   components:

   - a proof generating virtual machine (PVM), which provides the
   essential semantics for the rollup operations to be validated by
   the layer 1 in case of dispute about a commitment ;

   - a database which maintains the cemented operations of the rollup
   as well as the potentially-disputed operations.

*)

(** A smart-contract rollup has an address starting with "scr1". *)
module Address : sig
  include S.HASH

  (** [from_nonce nonce] produces an address completely determined by
     an operation hash and an origination counter. *)
  val from_nonce : Origination_nonce.t -> t tzresult

  (** [encoded_size] is the number of bytes needed to represent an address. *)
  val encoded_size : int
end

module Internal_for_tests : sig
  val originated_sc_rollup : Origination_nonce.t -> Address.t
end

module Commitment_hash : S.HASH

module State_hash : S.HASH

(** Number of messages consumed by a single commitment. This represents a claim
    about the shape of the Inbox, which can be disputed as part of a commitment
    dispute.

    See also {!Sc_rollup_repr.Commitments}. *)
module Number_of_messages : Bounded.Int32.S

(** Number of ticks computed by a single commitment. This represents a claim
    about the state of the PVM, which can be disputed as part of a commitment
    dispute.

    See also {!Sc_rollup_repr.Commitments}. *)
module Number_of_ticks : Bounded.Int32.S

(** A commitment represents a claim about the state of the Inbox and PVM at
    some Inbox level.

    More formally, a commitment is a claim that:

    {ul
      {li assuming the PVM and Inbox are in a state implied by [predecessor]}
      {li the PVM consumes [number_of_messages] messages tagged with
      [inbox_level] from the Inbox}
      {li the PVM advances to the state [compressed_state] over
      [number_of_ticks] ticks }
    }

    Commitments are disjoint. The next correct commitment is a function of the
    previous machine state and Inbox.

    [number_of_messages] and [inbox_level] can be proven/disproven by Merkle
    proofs on the Inbox state.

    [compressed_state] and [number_of_ticks] can be proven/disproven by PVM
    execution, or equivalently, by an interactive proof game between
    conflicting parties, such that a correct executor always wins the game.
 *)
module Commitment : sig
  type t = {
    compressed_state : State_hash.t;
    inbox_level : Raw_level_repr.t;
    predecessor : Commitment_hash.t;
    number_of_messages : Number_of_messages.t;
    number_of_ticks : Number_of_ticks.t;
  }

  val pp : Format.formatter -> t -> unit

  val encoding : t Data_encoding.t

  val hash : t -> Commitment_hash.t
end

(** A smart contract rollup is identified by its address. *)
type t = Address.t

(** A [Staker] is an implicit account, identified by its public key hash. *)
module Staker :
  S.SIGNATURE_PUBLIC_KEY_HASH with type t = Signature.Public_key_hash.t

val encoding : t Data_encoding.t

val rpc_arg : t RPC_arg.t

val pp : Format.formatter -> t -> unit

(** The data model uses an index of these addresses. *)
module Index : Storage_description.INDEX with type t = Address.t

module Commitment_hash_index :
  Storage_description.INDEX with type t = Commitment_hash.t

(** A smart contract rollup has a kind, which assigns meaning to
   rollup operations. *)
module Kind : sig
  (**

     The list of available rollup kinds.

     This list must only be appended for backward compatibility.
  *)
  type t = Example_arith

  val encoding : t Data_encoding.t

  val equal : t -> t -> bool

  val pp : Format.formatter -> t -> unit
end

module Proof : sig
  type t = Computation_step | Input_step | Blocked_step

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit

  val start_state : t -> State_hash.t

  val stop_state : t -> State_hash.t option

  val valid : t -> bool
end

(** The refutation game types are defined here, as well as the logic for
    how to create a new game from a pair of commits in the commit tree.
    See the [Sc_rollup_game] module for more detailed documentation of
    the game itself---that module is where the update function for
    playing moves is defined. *)
module Game : sig
  (** The two stakers index the game in the storage, as an ordered pair
      of public key hashes. We use [Alice] and [Bob] to represent the
      first and second player respectively. *)
  type player = Alice | Bob

  (**
     A game state is characterized by:

      - [turn], the player that must provide the next move.

      - [dissection], a list of states with tick counts. The current
        player will specify, in the next move, a tick count that
        indicates the last of these states that she agrees with.

      Invariants:
      -----------
      - [dissection] must contain at least 3 values
      - only the last value in [dissection] may be [None]
  *)
  type t = {
    turn : player;
    dissection : (State_hash.t option * Sc_rollup_tick_repr.t) list;
  }

  (** Return the other player *)
  val opponent : player -> player

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit

  module Index : sig
    type t = Staker.t * Staker.t

    val to_path : t -> string list -> string list

    val of_path : string list -> t option

    val path_length : int

    val rpc_arg : t RPC_arg.t

    val encoding : t Data_encoding.t

    val compare : t -> t -> int

    (** The 'normal form' for indices is when the two stakers are
        ordered (we just use [Staker.compare]). *)
    val normalize : t -> t

    (** Given an index in normal form, resolve a given [player] ([Alice]
        or [Bob]) to the actual staker they represent. *)
    val staker : Staker.t * Staker.t -> player -> Staker.t
  end

  (** To begin a game, first the conflict point in the commit tree is
      found, and then this function is applied.
      
      [initial parent child refuter defender] will construct an initial
      game where [refuter] is next to play. The game has [start_state]
      determined by [parent], the commit that both stakers agree on.
      
      The game has [stop_state] set to [None], a single tick after the
      [child] commit, to represent the claim that [child] commit has a
      blocked state. [current_dissection] gives [refuter] a choice: she
      can refute the commit itself by providing a new dissection between
      the two committed states, or she can refute the claim that the
      [child] commit is a blocked state by immediately providing a proof
      of a single tick increment from that state to its successor. *)
  val initial : Commitment.t -> Commitment.t -> Staker.t -> Staker.t -> t

  (** A [step] in the game is either a new dissection (if there are
      intermediate ticks remaining to put in it) or a proof. *)
  type step =
    | Dissection of (State_hash.t option * Sc_rollup_tick_repr.t) list
    | Proof of Proof.t

  (** A [refutation] is a move in the game. [choice] is the final tick
      in the current dissection at which the two players agree. *)
  type refutation = {choice : Sc_rollup_tick_repr.t; step : step}

  val pp_refutation : Format.formatter -> refutation -> unit

  val refutation_encoding : refutation Data_encoding.t

  (** A game ends for one of three reasons: the conflict has been
      resolved via a proof, a player has been timed out, or a player has
      forfeited because of attempting to make an invalid move. *)
  type reason = Conflict_resolved | Invalid_move | Timeout

  val pp_reason : Format.formatter -> reason -> unit

  val reason_encoding : reason Data_encoding.t

  (** A type that represents the current game status in a way that is
      useful to the outside world (using actual [Staker.t] values
      instead of the internal [player] type).

      Used in operation result types. *)
  type status = Ongoing | Ended of (reason * Staker.t)

  val pp_status : Format.formatter -> status -> unit

  val status_encoding : status Data_encoding.t

  (** A game ends with a single [loser] and the [reason] for the game
      ending. This type is 'internal' to the game logic, it uses
      [Alice] or [Bob] to refer to the players without knowing which
      stakers they are. *)
  type outcome = {loser : player; reason : reason}

  val pp_outcome : Format.formatter -> outcome -> unit

  val outcome_encoding : outcome Data_encoding.t

  (** Applies the move [refutation] to the game. Checks the move is
      valid and returns an [Invalid_move] outcome if not.
      
      In the case of the game continuing, this swaps the current
      player and updates the [dissection]. In the case of a [Proof]
      being provided this returns a [Conflict_resolved] outcome. *)
  val play : t -> refutation -> (outcome, t) Either.t
end
