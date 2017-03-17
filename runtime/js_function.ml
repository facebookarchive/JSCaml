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
open Js_prop
open Js_rtti
open Js_syms
open Js_base
open Js_builtin

class functionPrototypeApply = object(self)
  inherit builtinFunction 2.0 "apply" tag_FunctionPrototypeApply

  method call func this arg_list =
    match arg_list with
    | Object o when o#is_type tag_Array ->
      let a : js_array_instance = Obj.magic o in
      let length = a#length in
      let call_args = Array.make a#length Undefined in
      for i = 0 to length - 1 do
        Array.unsafe_set call_args i (a#get_element i);
      done;
      !jsc_Call func this call_args
    | _ ->
      raise (!jsc_TypeException ())

  method! dyn_call this args =
    self#call this (dyn_arg 0 args) (dyn_arg 1 args)

  method construct (_ : js_value) (_ : js_value) : js_object_instance =
    raise (!jsc_TypeException ())

end

class functionPrototypeCall = object(self)
  inherit builtinFunction 1.0 "call" tag_FunctionPrototypeCall

  method call func this args =
    !jsc_Call func this args

  method! dyn_call func args =
    self#call func (dyn_arg 0 args) (dyn_args 1 args)

  method construct (_ : js_value) (_ : js_value) : js_object_instance =
    raise (!jsc_TypeException ())

end

class functionPrototypeBind = object(self)
  inherit builtinFunction 1.0 "bind" tag_FunctionPrototypeBind

  method call_helper target this bound_args =
    let bound_args_n = Array.length bound_args in
    (* Prototype of the bound function. *)
    let proto = ref target#getPrototypeOf in
    (* Attributes of the bound function. *)
    let targetLength = match target#prop_get sym_length with
      | Number n -> n
      | _ -> 0. in
    let targetName = match target#prop_get sym_name with
      | String s -> s
      | _ -> "" in
    let length = max 0. (targetLength -. (float_of_int bound_args_n)) in
    let name = "bound " ^ targetName in
    (* Bound function object - it is both a Function and a BoundFunction. *)
    Object (object
      inherit baseFunction length name as super

      method getPrototypeOf = !proto
      method setPrototypeOf new_proto = proto := Some new_proto; true

      method! is_type tag = tag = tag_BoundFunction || super#is_type tag

      method get_BoundTargetFunction = target

      method! dyn_call _ extra_args =
        target#dyn_call this (Array.append bound_args extra_args)

      method! has_construct = target#has_construct
      method! dyn_construct extra_args =
        target#dyn_construct (Array.append bound_args extra_args)

    end :> js_object_instance)

  method call target this arg_list =
    match arg_list with
    | Object o when o#is_type tag_Array ->
      let a : js_array_instance = Obj.magic o in
      let length = a#length in
      let call_args = Array.make a#length Undefined in
      for i = 0 to length - 1 do
        Array.unsafe_set call_args i (a#get_element i);
      done;
      self#call_helper target this call_args
    | _ ->
      raise (!jsc_TypeException ())

  method! dyn_call target args = match target with
    | Object o when o#has_call ->
      (* Bound this argument. *)
      let this = dyn_arg 0 args in
      (* Bound arguments. *)
      let n = Array.length args in
      let bound_args = if n > 0 then Array.sub args 1 (n - 1) else [||] in
      self#call_helper o this bound_args
    | _ ->
      raise (!jsc_TypeException ());

  method construct (_ : js_value) (_ : js_value) : js_object_instance =
    raise (!jsc_TypeException ())

end

class functionPrototype = object
  inherit objectPrototypeBase as super

  method get_constructor = !function_constructor

  val m_apply = new functionPrototypeApply
  method get_apply = m_apply

  val m_call = new functionPrototypeCall
  method get_call = m_call

  val m_bind = new functionPrototypeBind
  method get_bind = m_bind

  val m_hasInstance = define_function_1 "[Symbol.hasInstance]"
    ret_boolean arg_dyn
    (fun this v -> !jsc_OrdinaryHasInstance this v)
  method get_hasInstance = m_hasInstance

  method get_name = ""
  method set_name (_ : string) : unit = raise (!jsc_TypeException ())
  method get_length = 0.0
  method set_length (_ : float) : unit = raise (!jsc_TypeException ())

  method! has_call = true
  method! has_construct = true
  method! is_type tag = tag = tag_Function || super#is_type tag
  method! dyn_call _this _args = Undefined

  method call (_this : js_value) = Undefined

  method! setup_props = setup_props (super#setup_props) [
    (sym_constructor, object
      inherit property ~enumerable:false ~configurable:true ~writable:true
      method! has_value = true
      method! get_value = Object (!function_constructor :> js_object_instance)
    end);
    (sym_toString,    new builtinObjectProperty baseToString);
    (sym_apply,       new builtinObjectProperty m_apply);
    (sym_call,        new builtinObjectProperty m_call);
    (sym_bind,        new builtinObjectProperty m_bind);
    (sym_hasInstance, new constObjectProperty m_hasInstance);
    (sym_name,        new constStringProperty "");
    (sym_length,      new constNumberProperty 0.0);
  ];
end

class functionInstance ~prototype ~length ~name = object
  inherit [functionPrototype] builtinInstance prototype tag_Function as super

  val length: float = length
  method get_length = length
  method set_length (_: float) : unit = raise (!jsc_TypeException ())

  val name: string = name
  method get_name = name
  method set_name (_ : string) : unit = raise (!jsc_TypeException ())

  method! has_call = true
  method! has_construct = true

  method! setup_props = setup_props (super#setup_props) [
    (sym_length, new constNumberProperty length );
    (sym_name,   new constStringProperty name   )
  ]
end

class functionConstructor ~prototype = object
  inherit [functionPrototype, functionPrototype] builtinConstructor
    1.0 "Function" prototype

  method call (_ : js_value) (_ : js_value array) : functionInstance =
    raise (!jsc_TypeException ())

  method construct
      (_ : functionInstance) (_ : js_value array) : functionInstance =
    raise (!jsc_TypeException ())
end

module Function = struct
  let prototype = new functionPrototype
  let constructor = new functionConstructor prototype
  let empty = new functionInstance prototype 0. ""
end

let _ =
  function_prototype := (Function.prototype :> js_object_instance);
  function_constructor := (Function.constructor :> js_function_instance);
