(*
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

external char_code: char -> int = "%identity"
external char_chr: int -> char = "%identity"

let escape_name name =
  let rec find_escape_pos i n str =
    if i = n then -1, ' '
    else
      let ch = String.unsafe_get str i in
      if ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') ||
          ('0' <= ch && ch <= '9') ||  ch = '_' then
        find_escape_pos (i+1) n str
      else
        i, ch in
  let char_a = char_code 'a' in
  let append_escape ch buffer =
    let b = char_code ch in
    Buffer.add_char buffer (char_chr (char_a + b / 16));
    Buffer.add_char buffer (char_chr (char_a + b mod 16)) in
  let rec find_append_escape_and_repeat buffer i n str =
    match find_escape_pos i n str with
    | (-1), _ ->
      if i = 0 then Buffer.add_string buffer "_";
      Buffer.add_substring buffer str i (n-i);
    | j, ch ->
      if i = 0 then Buffer.add_string buffer "__";
      Buffer.add_substring buffer str i (j-i);
      append_escape ch buffer;
      find_append_escape_and_repeat buffer (j+1) n str in
  let n = (String.length name) in
  let buffer = Buffer.create n in
  find_append_escape_and_repeat buffer 0 n name;
  Buffer.contents buffer
