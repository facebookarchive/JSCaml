(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module LinkedHashtbl = struct
  type ('a, 'b) entry =
    { key: 'a;
      value: 'b;
      mutable prev: (('a, 'b) entry) option;
      mutable next: (('a, 'b) entry) option;
    }
  type ('a, 'b) t =
    { tbl: ('a, ('a, 'b) entry) Hashtbl.t;
      mutable first: (('a, 'b) entry) option;
      mutable last: (('a, 'b) entry) option;
    }

  let create n = { tbl = Hashtbl.create n; last = None; first = None }
  let find {tbl; _} key = (Hashtbl.find tbl key).value
  let mem {tbl; _} key = Hashtbl.mem tbl key

  let fold f {last; _} acc =
    let rec do_fold acc = function
      | None -> acc
      | Some elem ->
        do_fold (f elem.key elem.value acc) elem.prev
    in do_fold acc last

  let remove t key =
    try
      let e = Hashtbl.find t.tbl key in
      (match e.prev with
      | None -> ()
      | Some prev -> prev.next <- e.next
      );
      (match e.next with
      | None -> ()
      | Some next -> next.prev <- e.prev
      );
      (match t.last with
      | Some last when last == e -> t.last <- last.prev
      | _ -> ()
      );
      (match t.first with
      | Some first when first == e -> t.first <- first.next
      | _ -> ()
      );
      Hashtbl.remove t.tbl key
    with Not_found -> ()

  let add t key value =
    remove t key;
    let e = {key = key; value = value; prev = t.last; next = None} in
    Hashtbl.add t.tbl key e;
    (match t.last with
    | None -> ()
    | Some last -> last.next <- Some e
    );
    (match t.first with
    | None -> t.first <- Some e
    | Some _ -> ()
    );
    t.last <- Some e;
end

type to_primitive_hint =
  | StringHint
  | NumberHint

type js_value =
  | Boolean of bool
  | Null
  | Missing
  | Number of float
  | Object of js_object_instance
  | String of string
  | Symbol of int * string
  | Undefined

and js_type =
  | Boolean_t
  | Number_t
  | String_t
  | Symbol_t
  | Object_t
  | Undefined_t
  | Null_t
  | Tagged_t

and js_prop = <
  has_getter : bool;
  get_getter : js_value;
  has_setter : bool;
  get_setter : js_value;
  has_value : bool;
  get_value : js_value;
  set_value : js_value -> unit;
  is_configurable: bool;
  is_enumerable: bool;
  is_writable: bool;
  set_configurable: bool -> unit;
  set_enumerable: bool -> unit;
  set_writable: bool -> unit;
>

and js_prop_map = (int, js_prop) LinkedHashtbl.t

and js_iterator = <
  step : js_value option;
>

and js_array_instance = <
  getPrototypeOf : js_object_instance option;
  setPrototypeOf : js_object_instance -> bool;
  defineOwnProperty : js_value -> js_prop -> bool;
  getOwnProperty : js_value -> js_prop option;
  hasProperty : js_value -> bool;
  delete : js_value -> bool;
  get : js_value -> js_object_instance -> js_value;
  set : js_value -> js_value -> js_object_instance -> bool;

  get_element : int -> js_value;
  set_element : int -> js_value -> unit;
  delete_element : int -> bool;
  has_element : int -> bool;
  elem_type : js_type;
  iterator : js_iterator;
  resize : int -> unit;

  unwrap : js_array_instance;

  prop_get : js_value -> js_value;
  prop_set : js_value -> js_value -> unit;
  prop_create : js_value -> js_value -> unit;

  dyn_call : js_value -> js_value array -> js_value;
  dyn_construct : js_value array -> js_value;
  get_properties : js_prop_map;
  setup_props : js_prop_map;
  is_type : int -> bool;
  has_call : bool;
  has_construct : bool;
  length : int;
>

and 'a js_typed_array = <
  getPrototypeOf : js_object_instance option;
  setPrototypeOf : js_object_instance -> bool;
  defineOwnProperty : js_value -> js_prop -> bool;
  getOwnProperty : js_value -> js_prop option;
  hasProperty : js_value -> bool;
  delete : js_value -> bool;
  get : js_value -> js_object_instance -> js_value;
  set : js_value -> js_value -> js_object_instance -> bool;

  get_typed_element : int -> 'a;
  set_typed_element : int -> 'a -> unit;
  get_element : int -> js_value;
  set_element : int -> js_value -> unit;
  delete_element : int -> bool;
  has_element : int -> bool;
  elem_type : js_type;
  iterator : js_iterator;
  resize : int -> unit;

  unwrap : js_array_instance;

  prop_get : js_value -> js_value;
  prop_set : js_value -> js_value -> unit;
  prop_create : js_value -> js_value -> unit;

  dyn_call : js_value -> js_value array -> js_value;
  dyn_construct : js_value array -> js_value;
  get_properties : js_prop_map;
  setup_props : js_prop_map;
  is_type: int -> bool;
  has_call : bool;
  has_construct : bool;
  length : int;
>

and js_function_instance = <
  getPrototypeOf : js_object_instance option;
  setPrototypeOf : js_object_instance -> bool;
  defineOwnProperty : js_value -> js_prop -> bool;
  getOwnProperty : js_value -> js_prop option;
  hasProperty : js_value -> bool;
  delete : js_value -> bool;
  get : js_value -> js_object_instance -> js_value;
  set : js_value -> js_value -> js_object_instance -> bool;

  get_length : float;
  set_length : float -> unit;
  get_name : string;
  set_name : string -> unit;

  prop_get : js_value -> js_value;
  prop_set : js_value -> js_value -> unit;
  prop_create : js_value -> js_value -> unit;

  dyn_call : js_value -> js_value array -> js_value;
  dyn_construct : js_value array -> js_value;
  get_properties : js_prop_map;
  setup_props : js_prop_map;
  is_type: int -> bool;
  has_call : bool;
  has_construct : bool;
>

and js_function_prototype = <
  getPrototypeOf : js_object_instance option;
  setPrototypeOf : js_object_instance -> bool;
  defineOwnProperty : js_value -> js_prop -> bool;
  getOwnProperty : js_value -> js_prop option;
  hasProperty : js_value -> bool;
  delete : js_value -> bool;
  get : js_value -> js_object_instance -> js_value;
  set : js_value -> js_value -> js_object_instance -> bool;

  get_length : float;
  set_length : float -> unit;
  get_name : string;
  set_name : string -> unit;

  prop_get : js_value -> js_value;
  prop_set : js_value -> js_value -> unit;
  prop_create : js_value -> js_value -> unit;

  dyn_call : js_value -> js_value array -> js_value;
  dyn_construct : js_value array -> js_value;
  get_properties : js_prop_map;
  setup_props : js_prop_map;
  is_type: int -> bool;
  has_call : bool;
  has_construct : bool;
  has_instance : js_value -> bool;

  get_toString : < call : js_value -> string>;
  get_hasOwnProperty : < call : js_value -> js_value -> bool>;
  get_valueOf : < call : js_value -> js_value>;
  get_isPrototypeOf : < call : js_value -> js_value -> bool>;
  get_propertyIsEnumerable : < call : js_value -> js_value -> bool >;

  get_constructor : js_function_instance;
  get_hasInstance : < call : js_value -> js_value -> bool>;
  call : js_value -> js_value;
>

and js_object_instance = <
  getPrototypeOf : js_object_instance option;
  setPrototypeOf : js_object_instance -> bool;
  defineOwnProperty : js_value -> js_prop -> bool;
  getOwnProperty : js_value -> js_prop option;
  hasProperty : js_value -> bool;
  delete : js_value -> bool;
  get : js_value -> js_object_instance -> js_value;
  set : js_value -> js_value -> js_object_instance -> bool;

  prop_get : js_value -> js_value;
  prop_set : js_value -> js_value -> unit;
  prop_create : js_value -> js_value -> unit;

  dyn_call : js_value -> js_value array -> js_value;
  dyn_construct : js_value array -> js_value;
  get_properties : js_prop_map;
  setup_props : js_prop_map;
  is_type: int -> bool;
  has_call : bool;
  has_construct : bool;
>

type js_string_array = string js_typed_array

(* Magic value to deal with circular references. *)
let nil () = ref (Obj.magic ())

(* Instances set by their modules. *)
let object_prototype           : js_object_instance ref = nil ()
let number_prototype           : js_object_instance ref = nil ()
let string_prototype           : js_object_instance ref = nil ()
let boolean_prototype          : js_object_instance ref = nil ()
let function_prototype         : js_object_instance ref = nil ()
let regExp_prototype           : js_object_instance ref = nil ()
let error_prototype            : js_object_instance ref = nil ()
let evalError_prototype        : js_object_instance ref = nil ()
let rangeError_prototype       : js_object_instance ref = nil ()
let referenceError_prototype   : js_object_instance ref = nil ()
let syntaxError_prototype      : js_object_instance ref = nil ()
let typeError_prototype        : js_object_instance ref = nil ()
let uriError_prototype         : js_object_instance ref = nil ()
let global_prototype           : js_object_instance ref = nil ()
let array_prototype            : js_object_instance ref = nil ()
let date_prototype             : js_object_instance ref = nil ()
let symbol_prototype           : js_object_instance ref = nil ()
let object_constructor         : js_function_instance ref = nil ()
let number_constructor         : js_function_instance ref = nil ()
let string_constructor         : js_function_instance ref = nil ()
let boolean_constructor        : js_function_instance ref = nil ()
let function_constructor       : js_function_instance ref = nil ()
let error_constructor          : js_function_instance ref = nil ()
let evalError_constructor      : js_function_instance ref = nil ()
let rangeError_constructor     : js_function_instance ref = nil ()
let referenceError_constructor : js_function_instance ref = nil ()
let syntaxError_constructor    : js_function_instance ref = nil ()
let typeError_constructor      : js_function_instance ref = nil ()
let uriError_constructor       : js_function_instance ref = nil ()
let regExp_constructor         : js_function_instance ref = nil ()
let global_constructor         : js_function_instance ref = nil ()
let array_constructor          : js_function_instance ref = nil ()
let date_constructor           : js_function_instance ref = nil ()
let symbol_constructor         : js_function_instance ref = nil ()

(* Quite hacky forward declarations. *)
let jsc_TypeException : (unit -> exn) ref = nil ()
let jsc_RangeException : (unit -> exn) ref = nil ()
let jsc_ReferenceException : (unit -> exn) ref = nil ()
let jsc_SyntaxException : (unit -> exn) ref = nil ()
let jsc_Type : (js_value -> js_type) ref = nil ()
let jsc_GetMethod : (js_value -> js_value -> js_object_instance option) ref =
  nil ()
let jsc_IsCallable : (js_value -> bool) ref = nil ()
let jsc_ToPrimitive : (js_value -> js_value) ref = nil ()
let jsc_OrdinaryToPrimitive :
  (js_object_instance -> to_primitive_hint -> js_value) ref = nil ()
let jsc_ToPrimitive_Number : (js_value -> js_value) ref = nil ()
let jsc_ToPrimitive_String : (js_value -> js_value) ref = nil ()
let jsc_ToObject : (js_value -> js_object_instance) ref = nil ()
let jsc_ToBoolean : (js_value -> bool) ref = nil ()
let jsc_ToNumber : (js_value -> float) ref = nil ()
let jsc_ToInteger : (js_value -> float) ref = nil ()
let jsc_ToLength : (js_value -> float) ref = nil ()
let jsc_ToString : (js_value -> string) ref = nil ()
let jsc_SameValueNonNumber : (js_value -> js_value -> bool) ref = nil ()
let jsc_SameValue : (js_value -> js_value -> bool) ref = nil ()
let jsc_Call : (js_value -> js_value -> js_value array -> js_value) ref = nil ()
let jsc_OrdinaryHasInstance : (js_value -> js_value -> bool) ref = nil ()
let jsc_InstanceofOperator : (js_value -> js_value -> bool) ref = nil ()
let jsc_Get : (js_value -> js_value -> js_value) ref = nil ()
let jsc_ToPropertyDescriptor : (js_value -> js_prop) ref = nil ()
let jsc_FromPropertyDescriptor : (js_prop -> js_value) ref = nil ()
let jsc_GetOwnPropertyKeysString : (js_object_instance -> js_string_array) ref =
  nil ()
let jsc_DefinePropertyOrThrow : (js_value -> js_value -> js_prop -> unit) ref =
  nil ()
let jsc_ToPropertyKey : (js_value -> js_value) ref = nil ()
let jsc_EnumerableOwnNames : (js_object_instance -> js_string_array) ref =
  nil ()

(* Translation of JS exceptions *)
type nested_exception =
  | Nested_break
  | Nested_continue
  | Nested_labeled_break_or_continue of (unit -> unit)
  | Nested_finally of nested_exception
  | Nested_none
  | Nested_return
  | Nested_thrownValue of js_value

exception Break
exception Finally of nested_exception
exception Continue
exception Return
exception ThrownValue of js_value
