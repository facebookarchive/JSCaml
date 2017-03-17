(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Js_types
open Js_core
open Js_rtti
open Js_array

let cnv_num_to_str = Js_util.float_to_string

let cnv_num_to_bool = function
  | 0. -> false
  | n when n <> n -> false
  | _ -> true

let cnv_str_to_num = Js_util.string_to_float

let cnv_str_to_bool = function
  | "" -> false
  | _ -> true

let cnv_str_to_symbol = Js_syms.js_SymbolFor

let cnv_bool_to_num = function
  | true -> 1.0
  | false -> 0.0

let cnv_bool_to_str = function
  | true -> "true"
  | false -> "false"

let cnv_mixed_to_num = js_ToNumber

let cnv_mixed_to_str = js_ToString

let cnv_mixed_to_bool = js_ToBoolean

let cnv_mixed_to_object = js_ToObject

let is_array_of_elem_type o t =
  if o#is_type tag_Array then
    let a : js_array_instance = Obj.magic o in
    a#elem_type = t
  else false

let cnv_array_to_mixed arr =
  if arr#elem_type = Tagged_t then
    Object (arr :> js_object_instance)
  else
    Object ((new arrayWrapper arr) :> js_object_instance)

let cnv_mixed_to_bool_array = function
  | Object o when is_array_of_elem_type o Boolean_t ->
    ((Obj.magic o)#unwrap : booleanArrayInstance)
  | _ -> raise (js_TypeException ())

let cnv_mixed_to_number_array = function
  | Object o when is_array_of_elem_type o Number_t ->
    ((Obj.magic o)#unwrap : numberArrayInstance)
  | _ -> raise (js_TypeException ())

let cnv_mixed_to_string_array obj =
  match obj with
  | Object o when is_array_of_elem_type o String_t ->
    ((Obj.magic o)#unwrap : stringArrayInstance)
  | _ -> raise (js_TypeException ())

let cnv_mixed_to_symbol_array = function
  | Object o when is_array_of_elem_type o Symbol_t ->
    ((Obj.magic o)#unwrap : symbolArrayInstance)
  | _ -> raise (js_TypeException ())

let cnv_mixed_to_object_array = function
  | Object o when is_array_of_elem_type o Object_t ->
    (Obj.magic o)#unwrap;
  | _ -> raise (js_TypeException ())

let cnv_mixed_to_tag_array = function
  | Null
  | Missing
  | Undefined ->
    new tagArrayInstance Array.prototype [||]
  | Object o when is_array_of_elem_type o Tagged_t ->
    Obj.magic o
  | value ->
    let arr = new dynamicWrapper
      (js_ToObject value)
      Tagged_t
      (fun x -> x)
      (fun x -> x)
    in (arr :> tagArrayInstance)
