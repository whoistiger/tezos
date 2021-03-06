(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type round = int32

type t = round

module Map = Map.Make (Int32)

include (Compare.Int32 : Compare.S with type t := t)

let zero = 0l

let succ n =
  if Compare.Int32.equal n Int32.max_int then
    invalid_arg "round_repr.succ: cannot apply succ to maximum round value"
  else Int32.succ n

let pp fmt i = Format.fprintf fmt "%ld" i

type error += Negative_round of int

type error += Round_overflow of int

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"negative_round"
    ~title:"Negative round"
    ~description:"Round cannot be built out of negative integers."
    ~pp:(fun ppf i ->
      Format.fprintf
        ppf
        "Negative round cannot be built out of negative integers (%Ld)"
        i)
    (obj1 (req "Negative_round" int64))
    (function Negative_round i -> Some (Int64.of_int i) | _ -> None)
    (fun i -> Negative_round (Int64.to_int i)) ;
  register_error_kind
    `Permanent
    ~id:"round_overflow"
    ~title:"Round overflow"
    ~description:
      "Round cannot be built out of integer greater than maximum int32 value."
    ~pp:(fun ppf i ->
      Format.fprintf
        ppf
        "Round cannot be built out of integer greater than maximum int32 value \
         (%Ld)"
        i)
    (obj1 (req "Negative_round" int64))
    (function Round_overflow i -> Some (Int64.of_int i) | _ -> None)
    (fun i -> Round_overflow (Int64.to_int i))

let of_int32 i =
  if i >= 0l then Ok i else error (Negative_round (Int32.to_int i))
  [@@inline]

let pred r =
  let p = Int32.pred r in
  of_int32 p

let of_int i =
  if Compare.Int.(i < 0) then error (Negative_round i)
  else
    (* i is positive *)
    let i32 = Int32.of_int i in
    if Compare.Int.(Int32.to_int i32 = i) then Ok i32
    else error (Round_overflow i)

let to_int i32 =
  let i = Int32.to_int i32 in
  if Int32.(equal (of_int i) i32) then ok i else error (Round_overflow i)

let to_int32 t = t [@@inline]

let to_slot round ~committee_size =
  to_int round >|? fun r ->
  let slot = r mod committee_size in
  Slot_repr.of_int_exn slot

let encoding =
  Data_encoding.conv_with_guard
    (fun i -> i)
    (fun i ->
      match of_int32 i with
      | Ok _ as res -> res
      | Error _ -> Error "Round_repr.encoding: negative round")
    Data_encoding.int32

module Durations = struct
  type t = {
    first_round_duration : Period_repr.t;
    delay_increment_per_round : Period_repr.t;
  }

  type error +=
    | Non_increasing_rounds of {increment : Period_repr.t}
    | Round_durations_must_be_at_least_one_second of {round : Period_repr.t}

  let () =
    register_error_kind
      `Permanent
      ~id:"durations.non_increasing_rounds"
      ~title:"Non increasing round"
      ~description:"The provided rounds are not increasing."
      ~pp:(fun ppf increment ->
        Format.fprintf
          ppf
          "The provided rounds are not increasing (increment: %a)"
          Period_repr.pp
          increment)
      Data_encoding.(obj1 (req "increment" Period_repr.encoding))
      (function
        | Non_increasing_rounds {increment} -> Some increment | _ -> None)
      (fun increment -> Non_increasing_rounds {increment})

  let pp fmt t =
    Format.fprintf
      fmt
      "%a,@ +%a"
      Period_repr.pp
      t.first_round_duration
      Period_repr.pp
      t.delay_increment_per_round

  let create ~first_round_duration ~delay_increment_per_round =
    error_when
      Compare.Int64.(Period_repr.to_seconds first_round_duration < 1L)
      (Round_durations_must_be_at_least_one_second
         {round = first_round_duration})
    >>? fun () ->
    error_when
      Compare.Int64.(Period_repr.to_seconds delay_increment_per_round < 1L)
      (Non_increasing_rounds {increment = delay_increment_per_round})
    >>? fun () -> ok {first_round_duration; delay_increment_per_round}

  let create_opt ~first_round_duration ~delay_increment_per_round =
    match create ~first_round_duration ~delay_increment_per_round with
    | Ok v -> Some v
    | Error _ -> None

  let encoding =
    let open Data_encoding in
    conv_with_guard
      (fun {first_round_duration; delay_increment_per_round} ->
        (first_round_duration, delay_increment_per_round))
      (fun (first_round_duration, delay_increment_per_round) ->
        match create_opt ~first_round_duration ~delay_increment_per_round with
        | None ->
            Error
              "Either round durations are non-increasing or minimal block \
               delay < 1"
        | Some rounds -> Ok rounds)
      (obj2
         (req "first_round_duration" Period_repr.encoding)
         (req "delay_increment_per_round" Period_repr.encoding))

  let round_duration {first_round_duration; delay_increment_per_round} round =
    if Compare.Int32.(round < 0l) then
      invalid_arg "round must be a non-negative integer"
    else
      let first_round_duration_s = Period_repr.to_seconds first_round_duration
      and delay_increment_per_round_s =
        Period_repr.to_seconds delay_increment_per_round
      in
      Period_repr.of_seconds_exn
        Int64.(
          add
            first_round_duration_s
            (mul (of_int32 round) delay_increment_per_round_s))
end

type error += Round_too_high of int32

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"round_too_high"
    ~title:"round too high"
    ~description:"block round too high."
    ~pp:(fun ppf round ->
      Format.fprintf ppf "Block round is too high: %ld" round)
    (obj1 (req "level_offset_too_high" int32))
    (function Round_too_high round -> Some round | _ -> None)
    (fun round -> Round_too_high round)

(* The duration of round n follows the arithmetic sequence:

     round_duration(0)   = first_round_duration
     round_duration(r+1) = round_duration(r) + delay_increment_per_round

   Hence, this sequence can be explicited into:

     round_duration(r) = first_round_duration + r * delay_increment_per_round

   The level offset of round r is the sum of the durations of the rounds up
   until round r - 1. In other words, when r > 0

     level_offset_of_round(0)   = 0
     level_offset_of_round(r+1) = level_offset_of_round(r) + round_duration(r)

Hence

     level_offset_of_round(r) = ??_{k=0}^{r-1} (round_duration(k))

   After unfolding the series, the same function can be finally explicited into

     level_offset_of_round(0) = 0
     level_offset_of_round(r) = r * first_round_duration
                                + 1/2 * r * (r - 1) * delay_increment_per_round
*)
let level_offset_of_round round_durations ~round =
  if Compare.Int32.(round = zero) then ok Int64.zero
  else
    let sum_durations =
      let Durations.{first_round_duration; delay_increment_per_round} =
        round_durations
      in
      let roundz = Int64.of_int32 round in
      let m = Z.of_int64 Int64.(div (mul roundz (pred roundz)) (of_int 2)) in
      Z.(
        add
          (mul
             m
             (Z.of_int64 @@ Period_repr.to_seconds delay_increment_per_round))
          (mul
             (Z.of_int32 round)
             (Z.of_int64 @@ Period_repr.to_seconds first_round_duration)))
    in
    if Compare.Z.(sum_durations > Z.of_int64 Int64.max_int) then
      error (Round_too_high round)
    else ok (Z.to_int64 sum_durations)

type error += Level_offset_too_high of Period_repr.t

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"level_offset_too_high"
    ~title:"level offset too high"
    ~description:"The block's level offset is too high."
    ~pp:(fun ppf offset ->
      Format.fprintf
        ppf
        "The block's level offset is too high: %a"
        Period_repr.pp
        offset)
    (obj1 (req "level_offset_too_high" Period_repr.encoding))
    (function Level_offset_too_high offset -> Some offset | _ -> None)
    (fun offset -> Level_offset_too_high offset)

type round_and_offset = {round : int32; offset : Period_repr.t}

(** Complexity: O(log max_int). *)
let round_and_offset round_durations ~level_offset =
  let level_offset_in_seconds = Period_repr.to_seconds level_offset in
  (* We have the invariant [round <= level_offset] so there is no need to search
     beyond [level_offset]. We set [right_bound] to [level_offset + 1] to avoid
     triggering the error level_offset too high when the round equals
     [level_offset]. *)
  let right_bound =
    if Compare.Int64.(level_offset_in_seconds < Int64.of_int32 Int32.max_int)
    then Int32.of_int (Int64.to_int level_offset_in_seconds + 1)
    else Int32.max_int
  in
  let rec bin_search min_r max_r =
    if Compare.Int32.(min_r >= right_bound) then
      error (Level_offset_too_high level_offset)
    else
      let round = Int32.(add min_r (div (sub max_r min_r) 2l)) in
      level_offset_of_round round_durations ~round:(Int32.succ round)
      >>? fun next_level_offset ->
      if
        Compare.Int64.(Period_repr.to_seconds level_offset >= next_level_offset)
      then bin_search (Int32.succ round) max_r
      else
        level_offset_of_round round_durations ~round
        >>? fun current_level_offset ->
        if
          Compare.Int64.(
            Period_repr.to_seconds level_offset < current_level_offset)
        then bin_search min_r round
        else
          ok
            {
              round;
              offset =
                Period_repr.of_seconds_exn
                  (Int64.sub
                     (Period_repr.to_seconds level_offset)
                     current_level_offset);
            }
  in
  bin_search 0l right_bound

(** Complexity: O(|round_durations|). *)
let timestamp_of_round round_durations ~predecessor_timestamp ~predecessor_round
    ~round =
  let pred_round_duration =
    Durations.round_duration round_durations predecessor_round
  in
  (* First, the function computes when the current level l is supposed
     to start. This is given by adding to the timestamp of the round
     of predecessor level l-1 [predecessor_timestamp], the duration of
     its last round [predecessor_round]. *)
  Time_repr.(predecessor_timestamp +? pred_round_duration)
  >>? fun start_of_current_level ->
  (* Finally, we sum the durations of the rounds at the current level l until
     reaching current [round]. *)
  level_offset_of_round round_durations ~round >>? fun level_offset ->
  let level_offset = Period_repr.of_seconds_exn level_offset in
  Time_repr.(start_of_current_level +? level_offset)

(** Unlike [timestamp_of_round], this function gets the starting time
    of a given round, given the timestamp and the round of a proposal
    at the same level.

    We compute the starting time of [considered_round] from a given
    [round_durations] description, some [current_round], and its
    starting time [current_timestamp].

    Complexity: O(|round_durations|). *)
let timestamp_of_another_round_same_level round_durations ~current_timestamp
    ~current_round ~considered_round =
  level_offset_of_round round_durations ~round:considered_round
  >>? fun target_offset ->
  level_offset_of_round round_durations ~round:current_round
  >>? fun current_offset ->
  ok
  @@ Time_repr.of_seconds
       Int64.(
         add
           (sub (Time_repr.to_seconds current_timestamp) current_offset)
           target_offset)

type error +=
  | Round_of_past_timestamp of {
      provided_timestamp : Time.t;
      predecessor_timestamp : Time.t;
      predecessor_round : t;
    }

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"round_of_past_timestamp"
    ~title:"Round_of_timestamp for past timestamp"
    ~description:"Provided timestamp is before the expected level start."
    ~pp:(fun ppf (provided_ts, predecessor_ts, round) ->
      Format.fprintf
        ppf
        "Provided timestamp (%a) is before the expected level start (computed \
         based on predecessor_ts %a at round %a)."
        Time.pp_hum
        provided_ts
        Time.pp_hum
        predecessor_ts
        pp
        round)
    (obj3
       (req "provided_timestamp" Time.encoding)
       (req "predecessor_timestamp" Time.encoding)
       (req "predecessor_round" encoding))
    (function
      | Round_of_past_timestamp
          {provided_timestamp; predecessor_timestamp; predecessor_round} ->
          Some (provided_timestamp, predecessor_timestamp, predecessor_round)
      | _ -> None)
    (fun (provided_timestamp, predecessor_timestamp, predecessor_round) ->
      Round_of_past_timestamp
        {provided_timestamp; predecessor_timestamp; predecessor_round})

let round_of_timestamp round_durations ~predecessor_timestamp ~predecessor_round
    ~timestamp =
  let round_duration =
    Durations.round_duration round_durations predecessor_round
  in
  Time_repr.(predecessor_timestamp +? round_duration)
  >>? fun start_of_current_level ->
  Period_repr.of_seconds (Time_repr.diff timestamp start_of_current_level)
  |> Error_monad.record_trace
       (Round_of_past_timestamp
          {
            predecessor_timestamp;
            provided_timestamp = timestamp;
            predecessor_round;
          })
  >>? fun diff ->
  round_and_offset round_durations ~level_offset:diff
  >>? fun round_and_offset -> ok round_and_offset.round

let level_offset_of_round round_durations ~round =
  level_offset_of_round round_durations ~round >>? fun offset ->
  ok (Period_repr.of_seconds_exn offset)

module Internals_for_test = struct
  type round_and_offset_raw = {round : round; offset : Period_repr.t}

  let round_and_offset round_durations ~level_offset =
    round_and_offset round_durations ~level_offset >|? fun v ->
    {round = v.round; offset = v.offset}
end
