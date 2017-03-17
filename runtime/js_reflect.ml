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
open Js_object
open Js_builtin
open Js_rtti
open Js_prop
open Js_syms


class reflectInstance = object
  inherit [objectPrototype] builtinInstance Object.prototype tag_Reflect
    as super

  val get = define_function_3 ~length:2.0 "get"
    ret_dyn arg_dyn arg_dyn arg_dyn
    (fun _ target key receiver -> match target with
    | Object target ->
      (match receiver with
      | Missing
      | Undefined ->
        target#get (!jsc_ToPropertyKey key) target
      | Object receiver ->
        target#get (!jsc_ToPropertyKey key) receiver
      | _ ->
        Undefined
      )
    | _ -> raise (!jsc_TypeException ())
    )
  method get_get = get

  val set = define_function_4 ~length:3.0 "set"
    ret_boolean arg_dyn arg_dyn arg_dyn arg_dyn
    (fun _ target key v receiver -> match target with
    | Object target ->
      (match receiver with
      | Missing
      | Undefined ->
        target#set (!jsc_ToPropertyKey key) v target
      | Object receiver ->
        target#set (!jsc_ToPropertyKey key) v receiver
      | _ ->
        false
      )
    | _ -> raise (!jsc_TypeException ())
    )
  method get_set = set

  method! setup_props = setup_props (super#setup_props) [
    (sym_get, new builtinObjectProperty get);
    (sym_set, new builtinObjectProperty set);
  ]
end

module Reflect = struct
  let instance = new reflectInstance
end
