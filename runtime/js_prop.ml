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
open Js_syms

class property ~enumerable ~configurable ~writable = object
  val mutable enumerable : bool = enumerable
  val mutable configurable : bool = configurable
  val mutable writable : bool = writable

  method has_getter = false
  method get_getter : js_value = raise (!jsc_TypeException ())

  method has_setter = false
  method get_setter : js_value = raise (!jsc_TypeException ())

  method has_value = false
  method get_value : js_value = raise (!jsc_TypeException ())
  method set_value : js_value -> unit = raise (!jsc_TypeException ())

  method is_writable = writable
  method is_configurable = configurable
  method is_enumerable = enumerable

  method set_writable v = writable <- v
  method set_configurable v = configurable <- v
  method set_enumerable v = enumerable <- v
end

(* Read-only properties. *)
class constProperty = object
  inherit property ~enumerable: false ~configurable: false ~writable: false
  method !has_value = true
  method !set_value _ = ()
end
class constBooleanProperty ~v = object
  inherit constProperty
  method !get_value = Boolean v
end
class constNumberProperty ~v = object
  inherit constProperty
  method !get_value = Number v
end
class constStringProperty ~v = object
  inherit constProperty
  method !get_value = String v
end
class constTaggedProperty ~v = object
  inherit constProperty
  method !get_value = v
end
class constObjectProperty ~v = object
  inherit constProperty
  method !get_value = Object (v :> js_object_instance)
end

(* Property for a builtin value. *)
class builtinProperty = object
  inherit property ~enumerable: false ~configurable: false ~writable: true
  method !has_value = true
  (* TODO: allow mutation of properties as long as types match. *)
  method !set_value _ = ()
end
class builtinBooleanProperty ~v = object
  inherit builtinProperty
  method !get_value = Boolean v
end
class builtinNumberProperty ~v = object
  inherit builtinProperty
  method !get_value = Number v
end
class builtinStringProperty ~v = object
  inherit builtinProperty
  method !get_value = String v
end
class builtinTaggedProperty ~v = object
  inherit builtinProperty
  method !get_value = v
end
class builtinObjectProperty ~v = object
  inherit builtinProperty
  method !get_value = Object (v :> js_object_instance)
end

(* Getter for a builtin value. *)
class builtinGetter = object
  inherit property ~enumerable: false ~configurable: false ~writable: false
  method! has_getter = true
end

(* Object reference property *)
class objectProperty ~v = object
  inherit property
    ~enumerable: false
    ~configurable: false
    ~writable: false

  method !has_value = true
  method !get_value = Object (!v :> js_object_instance)
  method !set_value _ = ()
end

(* Sets up the initial properties of an object. *)
let setup_props t ps =
  ps |> List.iter (fun (k, v) ->
    LinkedHashtbl.add t (symbol_id k) (v : js_prop)
  );
  t
