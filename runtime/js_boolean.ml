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
open Js_function
open Js_prop
open Js_rtti
open Js_syms
open Js_builtin

class booleanPrototype = object
  inherit objectPrototypeBase as super

  val toString = define_function_0 "toString" ret_string
    (fun this -> match this with
    | Boolean true -> "true"
    | Boolean false -> "false"
    | Object o ->
      (match (dynamic_cast o tag_Boolean)#get_booleanData with
      | true -> "true"
      | false -> "false"
      )
    | _ -> raise (!jsc_TypeException ())
    )
  method get_toString = toString

  val! valueOf = define_function_0 "valueOf" ret_dyn
    (fun this -> match this with
    | Boolean _ -> this
    | Object o -> Boolean (dynamic_cast o tag_Boolean)#get_booleanData
    | _ -> raise (!jsc_TypeException ())
    )
  method! get_valueOf = valueOf

  method get_constructor : booleanConstructor =  Obj.magic !boolean_constructor
  method! is_type t = t = tag_BooleanPrototype || super#is_type t

  method! setup_props = setup_props (super#setup_props) [
    (sym_constructor, object
      inherit property ~enumerable:false ~configurable:true ~writable:true
      method! has_value = true
      method! get_value = Object (!boolean_constructor :> js_object_instance)
    end);
    (sym_toString,    new builtinObjectProperty toString);
    (sym_valueOf,     new builtinObjectProperty valueOf);
  ];
end

and booleanInstance ~prototype = object
  inherit [booleanPrototype] builtinInstance prototype tag_Boolean

  val mutable booleanData : bool = false
  method get_booleanData = booleanData
  method set_booleanData v = booleanData <- v

  method call (_ : js_value) (_ : js_value) : bool =
    raise (!jsc_TypeException ())
end

and booleanConstructor ~prototype = object
  inherit [booleanPrototype, functionPrototype] builtinConstructor
    1.0 "Boolean" prototype

  method call (_this : js_value) value = !jsc_ToBoolean value

  method construct value =
    let inst = new booleanInstance !instance_prototype in
    inst#set_booleanData (!jsc_ToBoolean value);
    inst
end

module Boolean = struct
  let prototype = new booleanPrototype
  let constructor = new booleanConstructor prototype
  let empty = new booleanInstance prototype
end

let _ =
  boolean_prototype := (Boolean.prototype :> js_object_instance);
  boolean_constructor := (Boolean.constructor :> js_function_instance);
