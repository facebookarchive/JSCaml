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
open Js_prop
open Js_array
open Js_types
open Js_math
open Js_builtin
open Js_rtti
open Js_syms
open Js_boolean
open Js_number
open Js_string
open Js_regexp
open Js_error
open Js_date
open Js_function
open Js_console
open Js_json
open Js_reflect

class globalPrototype = object
  inherit objectPrototypeBase as super

  method get_constructor : globalConstructor =  Obj.magic !global_constructor
  method! is_type t = t = tag_GlobalPrototype || super#is_type t
end

and globalInstance ~prototype = object(self)
  inherit [globalPrototype] builtinInstance prototype tag_Global as super

  val parseInt = define_function_2 "parseInt"
    ret_number arg_dyn arg_dyn
    (fun _ _ _ -> (* TODO: not implemented *) 0.0)
  method get_parseInt = parseInt

  val parseFloat = define_function_1 "parseFloat"
    ret_number arg_dyn
    (fun _ _ -> (* TODO: not implemented *) 0.0)
  method get_parseFloat = parseFloat

  val isNaN = define_function_1 "isNaN"
    ret_boolean arg_dyn
    (fun _ obj -> let n = !jsc_ToNumber obj in n <> n)
  method get_isNaN = isNaN

  val isFinite = define_function_1 "isFinite"
    ret_boolean arg_dyn
    (fun _ obj -> match !jsc_ToNumber obj with
    | n when n <> n -> false
    | n when n = infinity -> false
    | n when n = neg_infinity -> false
    | _ -> true
    )
  method get_isFinite = isFinite

  val decodeURI = define_function_1 "decodeURI"
    ret_string arg_string
    (fun _ _ -> (* TODO: not implemented *) "not implemented")
  method get_decodeURI = decodeURI

  val decodeURIComponent = define_function_1 "decodeURIComponent"
    ret_string arg_string
    (fun _ _ -> (* TODO: not implemented *) "not implemented")
  method get_decodeURIComponent = decodeURIComponent

  val encodeURI = define_function_1 "encodeURI"
    ret_string arg_string
    (fun _ _ -> (* TODO: not implemented *) "not implemented")
  method get_encodeURI = encodeURI

  val encodeURIComponent = define_function_1 "encodeURIComponent"
    ret_string arg_string
    (fun _ _ -> (* TODO: not implemented *) "not implemented")
  method get_encodeURIComponent = encodeURIComponent

  method get_Math = Math.instance
  method get_JSON = JSON.instance
  method get_console = Console.instance

  method get_Object = Object.constructor
  method get_Boolean = Boolean.constructor
  method get_String = String.constructor
  method get_Number = Number.constructor
  method get_Error = Error.constructor
  method get_TypeError = TypeError.constructor
  method get_RegExp = RegExp.constructor
  method get_Date = Date.constructor
  method get_Array = Object (Array.constructor :> js_object_instance)

  method get_NaN = nan
  method get_undefined = Undefined
  method get_Infinity = infinity
  method get_global = Object (self :> js_object_instance)

  method! setup_props = setup_props (super#setup_props) [
    (sym_isNaN,             new builtinObjectProperty isNaN);
    (sym_isFinite,          new builtinObjectProperty isFinite);
    (sym_Math,              new builtinObjectProperty Math.instance);
    (sym_console,           new builtinObjectProperty Console.instance);
    (sym_Reflect,           new builtinObjectProperty Reflect.instance);
    (sym_JSON,              new builtinObjectProperty JSON.instance);
    (sym_Reflect,           new builtinObjectProperty Reflect.instance);
    (sym_Object,            new builtinObjectProperty Object.constructor);
    (sym_Array,             new builtinObjectProperty Array.constructor);
    (sym_Object,            new builtinObjectProperty Object.constructor);
    (sym_Boolean,           new builtinObjectProperty Boolean.constructor);
    (sym_Date,              new builtinObjectProperty Date.constructor);
    (sym_Error,             new builtinObjectProperty Error.constructor);
    (sym_Function,          new builtinObjectProperty Function.constructor);
    (sym_Number,            new builtinObjectProperty Number.constructor);
    (sym_RegExp,            new builtinObjectProperty RegExp.constructor);
    (sym_String,            new builtinObjectProperty String.constructor);
    (sym_global,            new builtinObjectProperty self);
    (sym_NaN,               new constNumberProperty nan);
    (sym_undefined,         new constTaggedProperty Undefined);
    (sym_Infinity,          new constNumberProperty infinity);
  ];
end


and globalConstructor ~prototype = object
  inherit [globalPrototype, functionPrototype] builtinConstructor
    0.0 "Global" prototype

  method call (_this : js_value) (_ : js_value) =
    new globalInstance !instance_prototype

  method construct (_ : js_value) =
    new globalInstance !instance_prototype
end

module Global = struct
  let prototype = new globalPrototype
  let instance = new globalInstance prototype
  let constructor = new globalConstructor prototype
end

let _ =
  global_prototype := (Global.prototype :> js_object_instance);
  global_constructor := (Global.constructor :> js_function_instance)
