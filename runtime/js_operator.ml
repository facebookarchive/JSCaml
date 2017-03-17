(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Js_core
open Js_types
open Js_util


(* Equality operators *)
let strict_equals lhs rhs =
  if js_Type lhs <> js_Type rhs then
    false
  else match lhs, rhs with
  | Number lhs, Number rhs -> lhs = rhs
  | lhs, rhs ->
    js_SameValueNonNumber lhs rhs

let strict_not_equals lhs rhs =
  not (strict_equals lhs rhs)

let rec equals lhs rhs =
  if js_Type lhs = js_Type rhs then
    strict_equals lhs rhs
  else match lhs, rhs with
  | Null, Undefined -> true
  | Undefined, Null -> true
  | Number lhs, String rhs -> lhs = string_to_float rhs
  | String lhs, Number rhs -> string_to_float lhs = rhs
  | Boolean true, _  -> equals (Number 1.0) rhs
  | Boolean false, _ -> equals (Number 0.0) rhs
  | _, Boolean true  -> equals lhs (Number 1.0)
  | _, Boolean false -> equals lhs (Number 0.0)
  | String _, Object _
  | Symbol _, Object _
  | Number _, Object _ -> equals lhs (js_ToPrimitive rhs)
  | Object _, String _
  | Object _, Symbol _
  | Object _, Number _ -> equals (js_ToPrimitive lhs) rhs
  | _, _ -> false

let not_equals lhs rhs =
  not (equals lhs rhs)


(* Comparison operators *)
let abstract_comparison fn fs x y =
  match js_ToPrimitive x, js_ToPrimitive y with
  | String x, String y -> fs x y
  | px, py ->
    match js_ToNumber px, js_ToNumber py with
    | x, _ when x = nan -> false
    | _, y when y = nan -> false
    | x, y -> fn x y

let less_than =
  abstract_comparison (<) (fun x y -> String.compare x y < 0)

let less_than_or_equal =
  abstract_comparison (<=) (fun x y -> String.compare x y <= 0)

let greater_than =
  abstract_comparison (>) (fun x y -> String.compare x y > 0)

let greater_than_or_equal =
  abstract_comparison (>=) (fun x y -> String.compare x y >= 0)

(* Instanceof operator *)
let instanceof_operator = js_InstanceofOperator


(* Typeof operator *)
let typeof = function
  | Missing -> "undefined"
  | Undefined -> "undefined"
  | Boolean _ -> "boolean"
  | Number _ -> "number"
  | String _ -> "string"
  | Symbol _ -> "symbol"
  | Null -> "object"
  | Object o when o#has_call -> "function"
  | Object _  -> "object"


(* Arithmetic operators *)
let plus lhs rhs =
  match js_ToPrimitive lhs, js_ToPrimitive rhs with
  | String lhs, rhs -> String (lhs ^ js_ToString rhs)
  | lhs, String rhs -> String (js_ToString lhs ^ rhs)
  | lhs, rhs -> Number (js_ToNumber lhs +. js_ToNumber rhs)
