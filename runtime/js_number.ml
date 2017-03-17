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
open Js_prop
open Js_object
open Js_function
open Js_util
open Js_rtti
open Js_syms
open Js_builtin

class numberPrototype = object
  inherit objectPrototypeBase as super

  val toString = define_function_1 "toString" ret_string arg_dyn
    (fun this radix ->
      let float_val = match this with
        | Number n -> n
        | Object o -> (dynamic_cast o tag_Number)#get_numberData
        | _ -> raise (!jsc_TypeException ()) in
      match radix with
        | Undefined | Missing -> float_to_string_with_radix float_val 10
        | Number r when (int_of_float r) < 2 || (int_of_float r) > 36 ->
          raise (!jsc_RangeException ())
        | Number r -> float_to_string_with_radix float_val (int_of_float r)
        | _ -> raise (!jsc_TypeException ())
    )
  method get_toString = toString

  val! valueOf = define_function_0 "valueOf" ret_dyn
    (fun this -> match this with
    | Number _ -> this
    | Object o -> Number (dynamic_cast o tag_Number)#get_numberData
    | _ -> raise (!jsc_TypeException ())
    )
  method! get_valueOf = valueOf

  method get_constructor : numberConstructor =  Obj.magic !number_constructor
  method! is_type t = t = tag_NumberPrototype || super#is_type t

  method! setup_props = setup_props (super#setup_props) [
    (sym_constructor, object
      inherit property ~enumerable:false ~configurable:true ~writable:true
      method! has_value = true
      method! get_value = Object (!number_constructor :> js_object_instance)
    end);
    (sym_toString,    new builtinObjectProperty toString);
    (sym_valueOf,     new builtinObjectProperty valueOf);
  ];
end

and numberInstance ~prototype = object
  inherit [numberPrototype] builtinInstance prototype tag_Number

  val mutable numberData : float = nan
  method get_numberData = numberData
  method set_numberData v = numberData <- v

  (* Needed because Flow does not distinguish instances from constructors *)
  method call (_ : js_value) (_ : js_value) : float =
    raise (!jsc_TypeException ())
end

and numberConstructor prototype = object
  inherit [numberPrototype, functionPrototype] builtinConstructor
    1.0 "Number" prototype as super

  method call (_this : js_value) = function
    | Missing -> 0.
    | value -> !jsc_ToNumber value

  method construct value =
    let inst = new numberInstance !instance_prototype in
    inst#set_numberData (match value with
      | Missing -> 0.
      | _ -> (!jsc_ToNumber value);
      );
    inst

  method get_EPSILON           = epsilon_float
  method get_MAX_SAFE_INTEGER  = (+9007199254740991.)
  method get_MAX_VALUE         = max_float
  method get_MIN_SAFE_INTEGER  = (-9007199254740991.)
  method get_MIN_VALUE         = min_float
  method get_NaN               = nan
  method get_NEGATIVE_INFINITY = neg_infinity
  method get_POSITIVE_INFINITY = infinity

  val isNaN = define_function_1 "isNaN" ret_boolean arg_dyn
    (fun (_ : js_value) obj -> match obj with
    | Number n when n <> n -> true
    | _ -> false
    )
  method get_isNaN = isNaN

  method! setup_props = setup_props (super#setup_props) [
    (sym_isNaN,             new builtinObjectProperty isNaN               );
    (sym_EPSILON,           new builtinNumberProperty epsilon_float       );
    (sym_MAX_SAFE_INTEGER,  new builtinNumberProperty (+9007199254740991.));
    (sym_MAX_VALUE,         new builtinNumberProperty max_float           );
    (sym_MIN_SAFE_INTEGER,  new builtinNumberProperty (-9007199254740991.));
    (sym_MIN_VALUE,         new builtinNumberProperty min_float           );
    (sym_NaN,               new builtinNumberProperty nan                 );
    (sym_NEGATIVE_INFINITY, new builtinNumberProperty neg_infinity        );
    (sym_POSITIVE_INFINITY, new builtinNumberProperty infinity            )
  ]
end

module Number = struct
  let prototype = new numberPrototype
  let constructor = new numberConstructor prototype
  let empty = new numberInstance prototype
end

let _ =
  number_prototype := (Number.prototype :> js_object_instance);
  number_constructor := (Number.constructor :> js_function_instance);
