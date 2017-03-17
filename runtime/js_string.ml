(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Js_object
open Js_function
open Js_types
open Js_prop
open Js_rtti
open Js_syms
open Js_builtin

class stringPrototype = object
   inherit objectPrototypeBase as super

    method get_constructor : stringConstructor =  Obj.magic !string_constructor
    method! is_type t = t = tag_StringPrototype || super#is_type t

  val toString = define_function_0 "toString" ret_string
    (fun this -> match this with
    | String s -> s
    | Object o -> (dynamic_cast o tag_String)#get_stringData
    | _ -> raise (!jsc_TypeException ())
    )
  method get_toString = toString

  val! valueOf = define_function_0 "valueOf" ret_dyn
    (fun this -> match this with
    | String _ -> this
    | Object o -> String (dynamic_cast o tag_String)#get_stringData
    | _ -> raise (!jsc_TypeException ())
    )
  method! get_valueOf = valueOf

  val indexOf = define_function_2 ~length:1.0 "indexOf"
    ret_number arg_string arg_dyn
    (fun this search position -> match this with
      | Null
      | Undefined ->
        raise (!jsc_TypeException ())
      | _ ->
        let s = !jsc_ToString this in
        let p = !jsc_ToNumber position in
        let l = String.length search in
        let n = String.length s in
        let rec contains i =
          if i + l > n then
            -1.0
          else if String.sub s i l = search then
            float_of_int i
          else
            contains (i + 1)
        in contains (max 0 (min n (int_of_float p)))
    )
  method get_indexOf = indexOf

  method! setup_props = setup_props (super#setup_props) [
    (sym_constructor, object
      inherit property ~enumerable:false ~configurable:true ~writable:true
      method! has_value = true
      method! get_value = Object (!string_constructor :> js_object_instance)
    end);
    (sym_toString, new builtinObjectProperty toString);
    (sym_indexOf,  new builtinObjectProperty indexOf);
    (sym_valueOf,  new builtinObjectProperty valueOf);
  ];
end

and stringInstance ~prototype = object
  inherit [stringPrototype] builtinInstance prototype tag_String

  val mutable stringData : string = ""
  method get_stringData = stringData
  method set_stringData v = stringData <- v

  (* Needed because Flow does not distinguish instances from constructors *)
  method call (_ : js_value) (_ : js_value) : string =
    raise (!jsc_TypeException ())

end

and stringConstructor ~prototype = object
  inherit [stringPrototype, functionPrototype] builtinConstructor
    1.0 "String" prototype

  method call (_this : js_value) = function
    | Missing -> ""
    | Symbol(_, s) -> "Symbol(" ^ s ^ ")"
    | value -> !jsc_ToString value

  method construct value =
    let inst = new stringInstance !instance_prototype in
    inst#set_stringData (match value with
      | Missing -> ""
      | value -> !jsc_ToString value
      );
    inst
end

module String = struct
  let prototype = new stringPrototype
  let constructor = new stringConstructor prototype
  let empty = new stringInstance prototype
end

let _ =
  string_prototype := (String.prototype :> js_object_instance);
  string_constructor := (String.constructor :> js_function_instance);
