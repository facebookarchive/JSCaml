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
open Js_prop
open Js_types
open Js_rtti
open Js_syms
open Js_builtin

class symbolPrototype = object
  inherit objectPrototypeBase as super

  method get_constructor : symbolConstructor =  Obj.magic !symbol_constructor
  method! is_type t = t = tag_SymbolPrototype || super#is_type t

  method! setup_props = setup_props (super#setup_props) [
    (sym_constructor, object
      inherit property ~enumerable:false ~configurable:true ~writable:true
      method! has_value = true
      method! get_value = Object (!symbol_constructor :> js_object_instance)
    end);
  ]
end

and symbolInstance ~prototype = object
  inherit [symbolPrototype] builtinInstance prototype tag_Symbol

  val mutable key : string = ""
  method set_key new_key = key <- new_key
  val mutable id : int = 0
  method set_id new_id = id <- new_id
end

and symbolConstructor ~prototype = object
  inherit [symbolPrototype, functionPrototype] builtinConstructor
    1.0 "Symbol" prototype

  method call (_this : js_value) value =
    let str = match value with
      | Missing   -> ""
      | Undefined -> ""
      | value     -> !jsc_ToString value
    in Symbol (-1, str)

  method construct (_ : js_value) : symbolInstance =
    raise (!jsc_TypeException ())

  val is_for = define_function_1 "for" ret_dyn arg_string
    (fun (_ : js_value) key -> js_SymbolFor key)
  method get_for = is_for

  method get_toPrimitive        = sym_toPrimitive
  method get_hasInstance        = sym_hasInstance
  method get_isConcatSpreadable = sym_isConcatSpreadable
  method get_iterator           = sym_iterator
  method get_match              = sym_match
  method get_replace            = sym_replace
  method get_search             = sym_search
  method get_species            = sym_species
  method get_split              = sym_split
  method get_toStringTag        = sym_toStringTag
  method get_unscopables        = sym_unscopables
end

module Symbol = struct
  let prototype = new symbolPrototype
  let constructor = new symbolConstructor prototype
  let empty = new symbolInstance prototype
end
