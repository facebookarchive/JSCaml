(* (** *)
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Js_types
open Js_builtin
open Js_object
open Js_function
open Js_array
open Js_rtti
open Js_syms
open Js_prop

(* FFI to invoke jscre *)
type js_reg_exp

external js_reg_exp_compile : string -> string -> js_reg_exp =
  "js_reg_exp_compile"
external js_reg_exp_exec : js_reg_exp -> string -> int -> int array =
  "js_reg_exp_exec"

(* Signature of RegExp types. *)
type js_reg_exp_instance = <
  get_global : bool;
  get_sticky : bool;
  get_lastIndex : float;
  set_lastIndex : float -> unit;
  get_reg_exp : js_reg_exp;
  get_pattern : string;
  get_flags : string;
>

(* Helper function to parse the flags string *)
let get_flag flags flag =
  try ignore(String.index flags flag); true
  with Not_found -> false

(* RegExp.prototype.exec *)
let exec = define_function_1 "exec"
  ret_dyn arg_string
  (fun this subject -> match this with
  | Object o when o#is_type tag_RegExp ->
    (* Fetch relevant properties from the object. *)
    let reg_exp : js_reg_exp_instance = Obj.magic o in
    let global = reg_exp#get_global in
    let sticky = reg_exp#get_sticky in
    let lastIndex = int_of_float (reg_exp#get_lastIndex) in

    (* Invoke the FFI matcher, convert results and update lastIndex. *)
    let idx = if global && sticky then 0 else max 0 lastIndex in
    if idx >= String.length subject then begin
      reg_exp#set_lastIndex 0.0;
      Null;
    end else begin
      let offsets = js_reg_exp_exec reg_exp#get_reg_exp subject idx in
      (match Arr.length offsets with
      | 0 ->
        if sticky then reg_exp#set_lastIndex 0.0;
        Null
      | n ->
        assert (n mod 2 = 0);
        let result = Arr.init (n / 2) (fun i ->
          let off_start = Arr.unsafe_get offsets (i * 2 + 0) in
          let off_end = Arr.unsafe_get offsets (i * 2 + 1) in
          if off_start >= 0 && off_end >= 0 then
            String (String.sub subject off_start (off_end - off_start))
          else
            Undefined
        ) in
        let off_index = Arr.unsafe_get offsets 0 in
        if sticky && off_index <> idx then begin
          (* If sticky bit is set, match must start at lastIndex. *)
          reg_exp#set_lastIndex 0.0;
          Null
        end else begin
          if sticky || global then begin
            let endIndex = Arr.unsafe_get offsets 1 in
            reg_exp#set_lastIndex (float_of_int endIndex);
          end;
          let arr = new tagArrayInstance Array.prototype result in
          arr#prop_create sym_input (String subject);
          arr#prop_create sym_index (Number (float_of_int off_index));
          Object (arr :> js_object_instance)
        end
      )
    end
  | _ ->
    raise (!jsc_TypeException ())
  )

(* RegExp.prototype.test *)
let test = define_function_1 "test"
  ret_boolean arg_string
  (fun this str -> match this with
  | Object o when o#is_type tag_RegExp ->
    (match exec#call this str with
    | Null -> false
    | _ -> true
    )
  | Object o ->
    (match o#prop_get sym_exec with
    | Object f when f#has_call ->
      !jsc_ToBoolean (f#dyn_call this [|String str|])
    | _ -> raise (!jsc_TypeException ())
    )
  | _ -> raise (!jsc_TypeException ())
  )

(* RegExp.prototype.toString *)
let toString = define_function_0 "toString" ret_string
  (fun this -> match this with
  | Object o when o#is_type tag_RegExp ->
    let reg_exp : js_reg_exp_instance = Obj.magic o in
    "/" ^ reg_exp#get_pattern ^ "/" ^ reg_exp#get_flags
  | Object o ->
    let pattern = !jsc_ToString (o#prop_get sym_pattern) in
    let flags = !jsc_ToString (o#prop_get sym_flags) in
    "/" ^ pattern ^ "/" ^ flags
  | _ ->
    raise (!jsc_TypeException ())
  )

class regExpPrototype = object
  inherit objectPrototypeBase as super

  method get_constructor : regExpConstructor =  Obj.magic !regExp_constructor
  method! is_type t = t = tag_RegExpPrototype || super#is_type t

  method get_exec = exec
  method get_test = test
  method get_toString = toString

  method! setup_props = setup_props (super#setup_props) [
    (sym_constructor, object
      inherit property ~enumerable:false ~configurable:true ~writable:true
      method! has_value = true
      method! get_value = Object (!regExp_constructor :> js_object_instance)
    end);
    (sym_exec,     new builtinObjectProperty exec);
    (sym_test,     new builtinObjectProperty test);
    (sym_toString, new builtinObjectProperty toString);
  ]
end

and regExpInstance ~prototype ~pattern ~flags = object
  inherit [regExpPrototype] builtinInstance prototype tag_RegExp as super

  (* Reference to the underlying RegExp object. *)
  val reg_exp = js_reg_exp_compile pattern flags
  method get_reg_exp = reg_exp

  (* Properties extracted from the flags string. *)
  val global     = get_flag flags 'g'
  val ignoreCase = get_flag flags 'i'
  val multiline  = get_flag flags 'm'
  val unicode    = get_flag flags 'u'
  val sticky     = get_flag flags 'y'
  method get_global = global
  method get_ignoreCase = ignoreCase
  method get_multiline = multiline
  method get_unicode = unicode
  method get_sticky = sticky

  (* Original flags & source. *)
  val pattern : string = pattern
  val flags : string = flags
  method get_pattern = pattern
  method get_flags = flags

  (* Index at which the next match is started. *)
  val mutable last_index = 0
  method set_lastIndex index = last_index <- int_of_float index
  method get_lastIndex = float_of_int (last_index)

  method! setup_props = setup_props (super#setup_props) [
    (sym_lastIndex, object inherit builtinProperty
      method! get_value = Number (float_of_int last_index)
      method! set_value v = last_index <- int_of_float (!jsc_ToNumber v)
    end);
  ];
end

and regExpConstructor ~prototype = object
  inherit [regExpPrototype, functionPrototype] builtinConstructor
    2.0 "RegExp" prototype

  method construct pattern flags = match pattern with
    | String pattern ->
      (match flags with
      | Missing
      | Undefined ->
        new regExpInstance prototype pattern ""
      | _ ->
        new regExpInstance prototype pattern (!jsc_ToString flags)
      )
    | _ ->
      failwith "unimplemented: RegExp(...)"
end

module RegExp = struct
  let prototype = new regExpPrototype
  let constructor = new regExpConstructor prototype
  let empty = new regExpInstance prototype "^$" ""
end

let _ =
  regExp_prototype := (RegExp.prototype :> js_object_instance);
  regExp_constructor := (RegExp.constructor :> js_function_instance);
