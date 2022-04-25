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

(** 

The refutation game types are defined here, as well as the logic for how
to create a new game from a pair of commits in the commit tree.

XXX: write more here

*)

open Sc_rollup_repr

module Proof : sig
  type t =
    | Computation_step of {
        valid : bool;
        start : State_hash.t;
        stop : State_hash.t;
      }
    | Input_step of {valid : bool; start : State_hash.t; stop : State_hash.t}
    | Blocked_step of {valid : bool; start : State_hash.t}

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit

  val start : t -> State_hash.t

  val stop : t -> State_hash.t option

  val valid : t -> bool
end

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
  inbox_snapshot : Sc_rollup_inbox_repr.t;
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
val initial :
  inbox:Sc_rollup_inbox_repr.t ->
  parent:Commitment.t ->
  child:Commitment.t ->
  refuter:Staker.t ->
  defender:Staker.t ->
  t

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
    useful to the outside world (using actual [.Staker.t] values
    instead of the internal [player] type).

    The [Staker.t] in the [Ended] case is the loser of the game: the
    staker who will have their stake slashed.

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
