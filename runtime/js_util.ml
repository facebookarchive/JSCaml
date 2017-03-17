(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let string_to_float s =
  let lit = String.trim s in

  (* Convert a single hex digit to an integer, returning -1 if invalid *)
  let convert_digit ch =
    if '0' <= ch && ch <= '9' then Char.code ch - Char.code '0'
    else if 'a' <= ch && ch <= 'f' then Char.code ch - Char.code 'a' + 10
    else if 'A' <= ch && ch <= 'F' then Char.code ch - Char.code 'A' + 10
    else -1 in

  (* Parses a number given a base, returning nan if any digit is invalid *)
  let rec parse_base base acc s = function
    | -1 -> acc
    | i ->
      let d = convert_digit (String.get s i) in
      if d < 0 || base <= d then
        nan
      else
        let next_acc = (acc *. (float_of_int base) +. (float_of_int d)) in
        parse_base base next_acc s (i - 1)
      in

  (* Parse binary, octal, decimal, hexadecimal based on prefix. *)
  let len = String.length lit in
  match len with
  | 0 -> 0.
  | 1 when String.get lit 0 <> '0' ->
    (try float_of_string lit with Failure _ -> nan)
  | 1 -> parse_base 8 0. (String.sub lit 1 (len - 1)) (len - 2)
  | _ ->
    (match String.sub lit 0 2 with
    | "0b" | "0B" -> parse_base 2 0. (String.sub lit 2 (len - 2)) (len - 3)
    | "0x" | "0X" -> parse_base 16 0. (String.sub lit 2 (len - 2)) (len - 3)
    | _ -> try float_of_string lit with Failure _ -> nan)


let float_to_string = function
  (* +- 0 should be printed as 0. *)
  | 0. -> "0"
  (* Special values *)
  | n when n <> n -> "NaN"
  | n when n = infinity -> "Infinity"
  | n when n = neg_infinity -> "-Infinity"
  (* TODO: implement according to spec *)
  | n ->
    let s = string_of_float n in
    let len = String.length s in
    if String.get s (len - 1) = '.'
      then String.sub s 0 (len - 1)
      else s

let float_to_string_with_radix x radix =
  (* Check radix for easy case *)
  if radix = 10 then float_to_string x
  (* Special cases *)
  else if x = 0. then "0"
  else if x = infinity then "Infinity"
  else if x = neg_infinity then "-Infinity"
  else if x <> x then "NaN"
  (* Now we know it's a normal number *)
  else
    let num_to_digit = "0123456789abcdefghijklmnopqrstuvwxyz" in
    let radix_float = float_of_int radix in
    (* Follow the toPrecision guide of requiring only 21 significant digits *)
    let num_precision_digits = 21 in
    (* Compute integer portion. Adds characters to the end of buf *)
    let rec int_loop buf integer =
      if integer = 0 then ()
      else
        let m = integer mod radix in
        Buffer.add_char buf num_to_digit.[m];
        int_loop buf (integer / radix)
    and
    (* The algorithm is, multiply by the radix, the new digit is there,
    recurse on the fractional part,
    stop when the remaining fractional part is less than some precision.
    Adds characters to the end of buf and returns the last nonzero index of
    the buffer for chopping trailing zeros *)
    frac_loop buf fraction index last_nonzero_index =
      if index > num_precision_digits then
        last_nonzero_index
      else
        let digit_and_fraction = fraction *. radix_float in
        let digit = int_of_float digit_and_fraction in
        let fraction = digit_and_fraction -. (float_of_int digit) in
        Buffer.add_char buf num_to_digit.[digit];
        frac_loop buf fraction (index + 1)
          (if digit = 0 then last_nonzero_index else index)
    and
    (* reverse a buffer in place *)
    reverse_buffer b =
      reverse_buffer_from_position b 0
    and
    reverse_buffer_from_position b = function
      | i when i = Bytes.length b / 2 -> b
      | i ->
        let last = Bytes.length b - 1 in
        let front = Bytes.get b i in
        let back = Bytes.get b (last - i) in
        Bytes.set b i back;
        Bytes.set b (last - i) front;
        reverse_buffer_from_position b (i + 1)
    in
    let is_negative = x < 0.0 in
    let neg_sign = if is_negative then "-" else "" in
    let y = if is_negative then -.x else x in
    let integral = int_of_float y in
    let fractional = y -. (float_of_int integral) in

    (* 16 is the recommended default from the OCaml docs *)
    let int_buf = Buffer.create 16 in
    int_loop int_buf integral;
    (* Add negative sign at end in anticipation of being reversed *)
    Buffer.add_string int_buf neg_sign;
    (* Reverse the buffer because int_loop adds chars in reverse *)
    let int_buf_bytes = reverse_buffer (Buffer.to_bytes int_buf) in
    let buf = Buffer.create 16 in
    Buffer.add_bytes buf int_buf_bytes;
    let int_buf_length = Buffer.length buf in

    if fractional <> 0. then
      let zero_size =
        if Buffer.length buf = 0 then (
          Buffer.add_char buf '0';
          1
        )
        else 0
      in
      Buffer.add_char buf '.';
      let last_nonzero_index = (frac_loop buf fractional 0 0) in
      (* Remove the trailing zeros.
      The fractional portion begins at int_buf_length + zero_size + 1 (for '.').
      The trailing zeros are at last_nonzero_index away from the start of the
      fractional portion.
      There is a final + 1 because Buffer.sub takes in a "off-the-end" index,
      so we want last_nonzero_index + 1 *)
      Buffer.sub buf 0 (int_buf_length + zero_size + last_nonzero_index + 2)
    else
      Buffer.contents buf

let to_uint32 x =
  (* TODO: this might not work, investigate some more Math tests. *)
  Int32.to_int (Int32.of_int (int_of_float x))
