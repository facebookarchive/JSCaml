(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

include Js_types
include Js_boolean
include Js_builtin
include Js_core
include Js_cnv
include Js_prop
include Js_rtti
include Js_syms
include Js_base
include Js_iterator

include Js_array
include Js_date
include Js_error
include Js_function
include Js_global
include Js_number
include Js_object
include Js_reflect
include Js_math
include Js_json
include Js_operator
include Js_regexp
include Js_string
include Js_symbol
include Js_template

(* Run time helper routines *)
let assigned_to_with_prop _name _num_withs _value =
  false

let check_with_scopes _name _num_withs value =
  value

let lookup_function _name =
  Function.empty

let lookup_identifier key =
  if Global.instance#hasProperty key then
    Global.instance#prop_get key
  else
    raise (js_ReferenceException ())

let set_identifier _key _value : unit =
  raise (js_ReferenceException ())

let lookup_property key obj =
  match key, obj with
  | Symbol _, Object o -> o#prop_get key
  | key, String _  -> String.prototype#prop_get key
  | key, Number _  -> Number.prototype#prop_get key
  | key, Boolean _ -> Boolean.prototype#prop_get key
  | _ -> Undefined

(* TODO: implement this according to standard *)
let lookup_property_expression idx obj =
  match obj, idx with
  | Object o, Number n when o#is_type tag_Array ->
    ((Obj.magic o) : js_array_instance)#get_element (int_of_float n)
  | Object o, _ ->
    o#prop_get (js_ToPropertyKey idx)
  | _, _ ->
    Undefined

(* TODO: implement this according to standard *)
let set_property_expression obj idx value =
  match obj, idx with
  | Object o, _ ->
    o#prop_set (js_ToPropertyKey idx) value;
  | _, _ ->
    failwith "unimplemented: set_property_expression"

let has_property obj key = match key, obj with
  | Symbol _, Object obj -> obj#hasProperty key
  | _ -> failwith "unimplemented: has_property"

let object_without_properties _names _obj =
  Object (Object.empty)

let lookup_element _index _arr =
  Undefined

let array_slice_from _index _arr =
  Undefined

let push_with_object _obj =
  ()

let pop_with_object () =
  ()

let return_if_object_else_default result (this : objectInstance) =
  match result with
  | Object _ -> result
  | _ -> Object this

let args_rest args i len =
  if i < len then
    Arr.sub args i (len - i)
  else
    [||]

let dynamic_call = js_Call

let dynamic_construct callee args =
  let o = js_ToObject callee in
  if not (o#has_construct) then
    raise (js_TypeException ());
  o#dyn_construct args

let delete obj sym = match obj with
  | Null
  | Undefined
  | Missing ->
    raise (js_TypeException ())
  | Boolean _
  | Number _
  | String _
  | Symbol _ ->
    true
  | Object o -> o#delete sym

let to_property_key value = js_SymbolFor (js_ToString value)

let to_primitive_number = js_ToNumber

let to_primitive_string = js_ToString

let to_primitive_boolean = js_ToBoolean

let to_primitive = js_ToPrimitive
