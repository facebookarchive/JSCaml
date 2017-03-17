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
open Js_base
open Js_rtti


(* Base class for builtin functions. *)
class builtinFunction ~length ~name ~tag = object
  inherit baseFunction length name as super

  method getPrototypeOf = Some !function_prototype
  method setPrototypeOf _p = raise (!jsc_TypeException  ())

  method! has_construct = false
  method! is_type t = t = tag || super#is_type t
end

(* Base class for builtin instances. *)
class ['p] builtinInstance ~prototype ~tag = object
  inherit baseObject as super

  val mutable typed_prototype : 'p = prototype
  method get_typed_prototype = typed_prototype
  method set_typed_prototype p = typed_prototype <- p
  method getPrototypeOf = Some (typed_prototype :> js_object_instance)
  method setPrototypeOf _p = raise (!jsc_TypeException  ())
  method! is_type t = t = tag || super#is_type t
end

(* Base class for builtin constructors. *)
class ['p, 'q] builtinConstructor ~length ~name ~prototype = object
  inherit ['p] baseConstructor length name prototype

  method get_typed_prototype : 'q = Obj.magic prototype
  method getPrototypeOf = Some !function_prototype
  method setPrototypeOf _p = raise (!jsc_TypeException  ())

end

(* Returns an argument or Missing if it does not exist. *)
let dyn_arg i args =
  if i >= Array.length args then Missing else Array.unsafe_get args i

let dyn_args i args =
  let len = Array.length args in
  if i < len then
    Array.sub args i (len - i)
  else
    [||]

(* Function with 0 or rest arguments. *)
let define_function_0
  ?(length=0.0)
  name
  (ret : 'r -> js_value)
  (f : js_value -> 'r) =
object(self)
  inherit builtinFunction length name tag_Function
  method call = f
  method construct : js_object_instance =
    raise (!jsc_TypeException ())
  method! dyn_call this _args =
    ret (self#call this)
end
let define_function_0_rest
  ?(length=0.0)
  name
  (ret : 'r -> js_value)
  (rest : js_value array -> 't array)
  (f : js_value -> 't array -> 'r) =
object(self)
  inherit builtinFunction length name tag_Function
  method call = f
  method construct (_ : 't array) : js_object_instance =
    raise (!jsc_TypeException ())
  method! dyn_call this args =
    let r = rest (dyn_args 0 args) in
    ret (self#call this r)
end

(* Function with 1 or rest arguments. *)
let define_function_1
  ?(length=1.0)
  name
  (ret : 'r -> js_value)
  (arg0 : js_value -> 'a0)
  (f : js_value -> 'a0 -> 'r) =
object(self)
  inherit builtinFunction length name tag_Function
  method call = f
  method construct (_ : 'a0) : js_object_instance =
    raise (!jsc_TypeException ())
  method! dyn_call this args =
    let a0 = arg0 (dyn_arg 0 args) in
    ret (self#call this a0)
end
let define_function_1_rest
  ?(length=1.0)
  name
  (ret : 'r -> js_value)
  (arg0 : js_value -> 'a0)
  (rest : js_value array -> 't array)
  (f : js_value -> 'a0 -> 't array -> 'r) =
object(self)
  inherit builtinFunction length name tag_Function
  method call = f
  method construct (_ : 'a0) (_ : 't array) : js_object_instance =
    raise (!jsc_TypeException ())
  method! dyn_call this args =
    let a0 = arg0 (dyn_arg 0 args) in
    let r = rest (dyn_args 1 args) in
    ret (self#call this a0 r)
end

(* Function with 2 or rest arguments. *)
let define_function_2
  ?(length=2.0)
  name
  (ret : 'r -> js_value)
  (arg0 : js_value -> 'a0)
  (arg1 : js_value -> 'a1)
  (f : js_value -> 'a0 -> 'a1 -> 'r) =
object(self)
  inherit builtinFunction length name tag_Function
  method call = f
  method construct (_ : 'a0) (_ : 'a1) : js_object_instance =
    raise (!jsc_TypeException ())
  method! dyn_call this args =
    let a0 = arg0 (dyn_arg 0 args) in
    let a1 = arg1 (dyn_arg 1 args) in
    ret (self#call this a0 a1)
end
let define_function_2_rest
  ?(length=2.0)
  name
  (ret : 'r -> js_value)
  (arg0 : js_value -> 'a0)
  (arg1 : js_value -> 'a1)
  (rest : js_value array -> 't array)
  (f : js_value -> 'a0 -> 'a1 -> 't array -> 'r) =
object(self)
  inherit builtinFunction length name tag_Function
  method call = f
  method construct (_ : 'a0) (_ : 'a1) (_ : 't array) : js_object_instance =
    raise (!jsc_TypeException ())
  method! dyn_call this args =
    let a0 = arg0 (dyn_arg 0 args) in
    let a1 = arg1 (dyn_arg 1 args) in
    let r = rest (dyn_args 2 args) in
    ret (self#call this a0 a1 r)
end

(* Function with 3 arguments *)
let define_function_3
  ?(length=3.0)
  name
  (ret : 'r -> js_value)
  (arg0 : js_value -> 'a0)
  (arg1 : js_value -> 'a1)
  (arg2 : js_value -> 'a2)
  (f : js_value -> 'a0 -> 'a1 -> 'a2 -> 'r) =
object(self)
  inherit builtinFunction length name tag_Function
  method call = f
  method construct (_ : 'a0) (_ : 'a1) (_ : 'a2): js_object_instance =
    raise (!jsc_TypeException ())
  method! dyn_call this args =
    let a0 = arg0 (dyn_arg 0 args) in
    let a1 = arg1 (dyn_arg 1 args) in
    let a2 = arg2 (dyn_arg 2 args) in
    ret (self#call this a0 a1 a2)
end

(* Function with 4 arguments *)
let define_function_4
  ?(length=4.0)
  name
  (ret : 'r -> js_value)
  (arg0 : js_value -> 'a0)
  (arg1 : js_value -> 'a1)
  (arg2 : js_value -> 'a2)
  (arg3 : js_value -> 'a3)
  (f : js_value -> 'a0 -> 'a1 -> 'a2 -> 'a3 -> 'r) =
object(self)
  inherit builtinFunction length name tag_Function
  method call = f
  method construct (_ : 'a0) (_ : 'a1) (_ : 'a2) (_ : 'a3): js_object_instance =
    raise (!jsc_TypeException ())
  method! dyn_call this args =
    let a0 = arg0 (dyn_arg 0 args) in
    let a1 = arg1 (dyn_arg 1 args) in
    let a2 = arg2 (dyn_arg 2 args) in
    let a3 = arg3 (dyn_arg 3 args) in
    ret (self#call this a0 a1 a2 a3)
end

(* Helper functions to wrap dynamic call arguments *)
let ret_object x = Object (x :> js_object_instance);;
let ret_number x = Number x;;
let ret_string x = String x;;
let ret_boolean x = Boolean x;;
let ret_string_arr _ = failwith "unimplemented: string array boxing"
let ret_dyn x = x;;

let arg_dyn x = x;;
let arg_string x = !jsc_ToString x;;
let arg_number x = !jsc_ToNumber x;;
let arg_rest_dyn x = x;;

let id x = x
