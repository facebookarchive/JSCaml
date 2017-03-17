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
open Js_base

class errorPrototype = object
  inherit objectPrototypeBase as super

  val toString = define_function_0 "toString" ret_string
    (fun this ->
      let to_string o =
        let name = (match o#prop_get sym_name with
          | Undefined | Missing -> "Error"
          | v -> !jsc_ToString v
          ) in
        let msg = (match o#prop_get sym_message with
          | Undefined | Missing -> ""
          | v -> !jsc_ToString v
          ) in
        match name, msg with
        | "", _ -> msg
        | _, "" -> name
        | _, _ -> name ^ ": " ^ msg
      in match this with
        | Object o -> to_string o
        | _ -> raise (!jsc_TypeException ())
    )
  method get_toString = toString

  method get_name = "Error"
  method get_message = ""

  method get_constructor : errorConstructor =  Obj.magic !error_constructor
  method! is_type t = t = tag_ErrorPrototype || super#is_type t

  method! setup_props = setup_props (super#setup_props) [
    (sym_constructor, object
      inherit property ~enumerable:false ~configurable:true ~writable:true
      method! has_value = true
      method! get_value = Object (!error_constructor :> js_object_instance)
    end);
    (sym_toString, new builtinObjectProperty toString);
    (sym_name,     new builtinStringProperty "Error");
    (sym_message,  new builtinStringProperty "");
  ];
end

and errorInstance ~prototype = object(self)
  inherit [errorPrototype] builtinInstance prototype tag_Error

  method set_name name = self#prop_set sym_name (String name)
  method get_name = !jsc_ToString (self#prop_get sym_name)

  method set_message message = self#prop_set sym_message (String message)
  method get_message = !jsc_ToString (self#prop_get sym_message)
end

and errorConstructor ~prototype = object(self)
  inherit [errorPrototype, functionPrototype] builtinConstructor
    1.0 "Error" prototype

  method call (_this : js_value) = function
    | Undefined | Missing ->
      new errorInstance !instance_prototype
    | value ->
      let inst = new errorInstance !instance_prototype in
      let val_ref = ref (String (!jsc_ToString value)) in
      ignore (inst#defineOwnProperty sym_message (object
        inherit property ~enumerable:false ~configurable:true ~writable:true
        method! has_value = true
        method! get_value = !val_ref
        method! set_value v = val_ref := v
      end));
      inst

  method construct =
    self#call Undefined

  method! dyn_construct args =
    Object ((self#construct (dyn_arg 0 args)) :> js_object_instance)
end

module Error = struct
  let prototype = new errorPrototype
  let constructor = new errorConstructor prototype
  let empty = new errorInstance prototype
end

let _ =
  error_prototype := (Error.prototype :> js_object_instance);
  error_constructor := (Error.constructor :> js_function_instance);

class nativeErrorPrototype ~name ~constructor ~tag = object
  inherit objectPrototypeBase as super

  val toString = define_function_0 "toString" ret_string
    (fun this ->
      let to_string o =
        let name = (match o#prop_get sym_name with
          | Undefined | Missing -> name
          | v -> !jsc_ToString v
          ) in
        let msg = (match o#prop_get sym_message with
          | Undefined | Missing -> ""
          | v -> !jsc_ToString v
          ) in
        match name, msg with
        | "", _ -> msg
        | _, "" -> name
        | _, _ -> name ^ ":: " ^ msg
      in match this with
        | Object o -> to_string o
        | _ -> raise (!jsc_TypeException ())
    )
  method get_toString = toString

  method get_name = name
  method get_message = ""

  method get_constructor : nativeErrorConstructor =  Obj.magic !constructor
  method! getPrototypeOf = Some !error_prototype
  method! is_type t = t = tag || super#is_type t

  method! setup_props = setup_props (super#setup_props) [
    (sym_constructor, object
      inherit property ~enumerable:false ~configurable:true ~writable:true
      method! has_value = true
      method! get_value = Object (!constructor :> js_object_instance)
    end);
    (sym_toString, new builtinObjectProperty toString);
    (sym_name,     new builtinStringProperty name);
    (sym_message,  new builtinStringProperty "");
  ];

end

and nativeErrorInstance ~prototype ~tag = object(self)
  inherit [nativeErrorPrototype] builtinInstance prototype tag_Error as super

  method set_message message = self#prop_set sym_message (String message)
  method get_message = !jsc_ToString (self#prop_get sym_message)

  method! is_type t = t = tag || super#is_type t
end

and nativeErrorConstructor ~prototype ~name ~tag = object(self)
  inherit [nativeErrorPrototype] baseConstructor 1.0 name prototype

  method get_typed_prototype : nativeErrorPrototype = prototype
  method getPrototypeOf = Some (!error_constructor :> js_object_instance)
  method setPrototypeOf _p = raise (!jsc_TypeException  ())

  method call (_this : js_value) = function
    | Undefined | Missing ->
      new nativeErrorInstance prototype tag
    | value ->
      let inst = new nativeErrorInstance prototype tag in
      let val_ref = ref (String (!jsc_ToString value)) in
      ignore (inst#defineOwnProperty sym_message (object
        inherit property ~enumerable:false ~configurable:true ~writable:true
        method! has_value = true
        method! get_value = !val_ref
        method! set_value v = val_ref := v
      end));
      inst

  method construct =
    self#call Undefined

  method! dyn_construct args =
    Object ((self#construct (dyn_arg 0 args)) :> js_object_instance)

end

module EvalError = struct
  let prototype = new nativeErrorPrototype
    "EvalError" evalError_constructor tag_EvalErrorPrototype
  let constructor =
    new nativeErrorConstructor prototype "EvalError" tag_EvalError
  let empty = new nativeErrorInstance prototype tag_EvalError
end

module RangeError = struct
  let prototype = new nativeErrorPrototype
    "RangeError" rangeError_constructor tag_RangeErrorPrototype
  let constructor =
    new nativeErrorConstructor prototype "RangeError" tag_RangeError
  let empty = new nativeErrorInstance prototype tag_RangeError
end

module ReferenceError = struct
  let prototype = new nativeErrorPrototype
    "ReferenceError" referenceError_constructor tag_ReferenceErrorPrototype
  let constructor =
    new nativeErrorConstructor prototype "ReferenceError" tag_ReferenceError
  let empty = new nativeErrorInstance prototype tag_ReferenceError
end

module SyntaxError = struct
  let prototype = new nativeErrorPrototype
    "SyntaxError" syntaxError_constructor tag_SyntaxErrorPrototype
  let constructor =
    new nativeErrorConstructor prototype "SyntaxError" tag_SyntaxError
  let empty = new nativeErrorInstance prototype tag_SyntaxError
end

module TypeError = struct
  let prototype = new nativeErrorPrototype
    "TypeError" typeError_constructor tag_TypeErrorPrototype
  let constructor =
    new nativeErrorConstructor prototype "TypeError" tag_TypeError
  let empty = new nativeErrorInstance prototype tag_TypeError
end

module URIError = struct
  let prototype = new nativeErrorPrototype
    "URIError" uriError_constructor tag_URIErrorPrototype
  let constructor =
    new nativeErrorConstructor prototype "URIError" tag_URIError
  let empty = new nativeErrorInstance prototype tag_URIError
end

let _ =
  evalError_prototype := (EvalError.prototype :> js_object_instance);
  evalError_constructor := (EvalError.constructor :> js_function_instance);
  referenceError_prototype := (ReferenceError.prototype :> js_object_instance);
  referenceError_constructor :=
    (ReferenceError.constructor :> js_function_instance);
  rangeError_prototype := (RangeError.prototype :> js_object_instance);
  rangeError_constructor := (RangeError.constructor :> js_function_instance);
  syntaxError_prototype := (SyntaxError.prototype :> js_object_instance);
  syntaxError_constructor := (SyntaxError.constructor :> js_function_instance);
  typeError_prototype := (TypeError.prototype :> js_object_instance);
  typeError_constructor := (TypeError.constructor :> js_function_instance);
  uriError_prototype := (URIError.prototype :> js_object_instance);
  uriError_constructor := (URIError.constructor :> js_function_instance);
