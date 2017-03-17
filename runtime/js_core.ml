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
open Js_error
open Js_util
open Js_object
open Js_prop
open Js_string
open Js_number
open Js_boolean
open Js_symbol
open Js_syms
open Js_rtti
open Js_array

(* Some objects are typed runtime wrappers which must be unwrapped to compare *)
let unwrap obj =
  if obj#is_type tag_DynamicWrapper then
    ((Obj.magic obj) : 'e dynamicWrapper)#get_obj
  else if obj#is_type tag_ArrayWrapper then
    ((((Obj.magic obj) : 'e arrayWrapper)#unwrap) :> js_object_instance)
  else
    obj

(* Helper methods to instantiate exceptions with error messages. *)
let native_error proto tag msg =
  let e = new nativeErrorInstance proto tag in
  e#set_message msg;
  Object (e :> js_object_instance)
let js_TypeError = native_error TypeError.prototype tag_TypeError
let js_RangeError = native_error RangeError.prototype tag_RangeError
let js_ReferenceError = native_error ReferenceError.prototype tag_ReferenceError
let js_SyntaxError = native_error SyntaxError.prototype tag_SyntaxError

(* The exceptions must be exposed to the C FFI. *)
let _ =
  Callback.register_exception "thrown_value" (ThrownValue Missing);
  Callback.register "js_type_error" js_TypeError;
  Callback.register "js_range_error" js_RangeError;
  Callback.register "js_reference_error" js_ReferenceError;
  Callback.register "js_syntax_error" js_SyntaxError;;

let js_TypeException () = ThrownValue (js_TypeError "")
let js_RangeException () = ThrownValue (js_RangeError "")
let js_ReferenceException () = ThrownValue (js_ReferenceError "")
let js_SyntaxException () = ThrownValue (js_SyntaxError "")

let get_object = function
  | Object o -> o
  | _ -> raise (js_TypeException ())

let js_BoundTargetFunction = function
  | Object o when o#is_type tag_BoundFunction ->
    Some (Object ((Obj.magic o)#get_BoundTargetFunction))
  | _ ->
    None

let js_Type = function
  | Boolean _ -> Boolean_t
  | Number _ -> Number_t
  | Object _ -> Object_t
  | String _ -> String_t
  | Symbol _ -> Symbol_t
  | Undefined -> Undefined_t
  | Missing -> Undefined_t
  | Null -> Null_t

let js_Get obj sym =
  match obj with
  | Object o -> o#prop_get sym
  | _ -> Undefined

let js_GetMethod obj sym =
  match js_Get obj sym with
  | Missing
  | Undefined
  | Null ->
    None
  | Object f when f#has_call ->
    Some f
  | _ ->
    raise (js_TypeException ())

let js_ToObject = function
  | Null
  | Missing
  | Undefined ->
    raise (!jsc_TypeException ())
  | Object o -> (o :> js_object_instance)
  | Boolean b ->
    let o = new booleanInstance Boolean.prototype in
    o#set_booleanData b;
    (o :> js_object_instance)
  | Number n ->
    let o = new numberInstance Number.prototype in
    o#set_numberData n;
    (o :> js_object_instance)
  | String s ->
    let o = new stringInstance String.prototype in
    o#set_stringData s;
    (o :> js_object_instance)
  | Symbol(id, key) ->
    let o = new symbolInstance Symbol.prototype in
    o#set_key key;
    o#set_id id;
    (o :> js_object_instance)

let js_IsCallable = function
  | Object o -> o#has_call
  | _ -> false

let js_Call f this args =
  (js_ToObject f)#dyn_call this args

let try_call sym o =
  match o#prop_get sym with
  | Object f when f#has_call ->
    (match f#dyn_call (Object o) [||] with
    | Object _ -> None
    | prim_val -> Some prim_val
    )
  | _ ->
    None

let rec try_call_any fs v = match fs with
  | [] -> raise (!jsc_TypeException ())
  | f :: fs ->
    match try_call f v with
    | None -> try_call_any fs v
    | Some v -> v

let js_OrdinaryToPrimitive o = function
  | StringHint ->
    (match try_call_any [sym_toString; sym_valueOf] o with
    | Object _ -> raise (!jsc_TypeException ())
    | prim -> prim
    )
  | NumberHint ->
    (match try_call_any [sym_valueOf; sym_toString] o with
    | Object _ -> raise (!jsc_TypeException ())
    | prim -> prim
    )

let to_primitive hint o fs =
  (match o#prop_get sym_toPrimitive with
  | Undefined
  | Missing ->
    (match try_call_any fs o with
    | Object _ -> raise (!jsc_TypeException ())
    | prim -> prim
    )
  | Object f when f#has_call ->
    (match f#dyn_call (Object o) [| String hint |] with
    | Object _ -> raise (!jsc_TypeException ())
    | prim -> prim
    )
  | _ ->
    raise (!jsc_TypeException ())
  )

let js_ToPrimitive = function
  | Object o ->
    to_primitive "default" o [sym_valueOf; sym_toString]
  | value ->
    value

let js_ToPrimitive_Number = function
  | Object o ->
    to_primitive "number" o [sym_valueOf; sym_toString]
  | value ->
    value

let js_ToPrimitive_String = function
  | Object o ->
    to_primitive "string" o [sym_toString; sym_valueOf]
  | value ->
    value

let js_ToBoolean = function
  | Boolean b -> b
  (* +0, -0 and nan convert to false *)
  | Number n when n = 0. || n <> n -> false
  | Number _ -> true
  (* String must be non-empty *)
  | String "" -> false
  | String _ -> true
  (* null/undefined convert to false *)
  | Null -> false
  | Missing -> false
  | Undefined -> false
  (* Objects covert to true *)
  | Symbol _ | Object _ -> true

let rec js_ToNumber = function
  | Boolean b -> if b then 1. else 0.
  | Null -> 0.
  | Missing -> nan
  | Number f -> f
  | String s -> string_to_float s
  | Symbol _ -> raise (!jsc_TypeException ())
  | Undefined -> nan
  | v -> js_ToNumber (js_ToPrimitive_Number v)

let js_ToInteger v =
  match js_ToNumber v with
  | n when n = 0. -> n
  | n when n <> n -> n
  | n when n = infinity -> n
  | n when n = neg_infinity -> n
  | n -> floor (abs_float (n))

let js_ToLength v =
  let max_length = float_of_int ((1 lsl 53) - 1) in
  match js_ToInteger v with
  | n when n < 0. -> 0.
  | n when n = infinity || n > max_length -> max_length
  | n -> n

let rec js_ToString = function
  | Null -> "null"
  | Undefined -> "undefined"
  | Missing -> "undefined"
  | Boolean true -> "true"
  | Boolean false -> "false"
  | String str -> str
  | Symbol _ -> raise (!jsc_TypeException ())
  | Number n -> float_to_string n
  | v -> js_ToString (js_ToPrimitive_String v)

let js_SameValueNonNumber x y = match x, y with
  | Undefined, Undefined -> true
  | Undefined, Missing -> true
  | Missing, Missing -> true
  | Missing, Undefined -> true
  | Null, Null -> true
  | String x, String y -> x = y
  | Boolean x, Boolean y -> x = y
  | Symbol _, Symbol _ -> x == y
  | Object x, Object y -> unwrap x == unwrap y
  | _, _ -> false

let js_SameValue x y =
  if js_Type x <> js_Type y then
    false
  else match x, y with
  | Number x, Number y ->
    (match x, y with
    | x, y when x <> x -> y <> y
    | 0., 0. -> Int64.bits_of_float x = Int64.bits_of_float y
    | x, y -> x = y
    )
  | x, y -> js_SameValueNonNumber x y

let rec js_OrdinaryHasInstance c o =
  if not (js_IsCallable c) then
    false
  else
    match js_BoundTargetFunction c with
    | None ->
      (match o with
      | Object o ->
        (match js_Get c sym_prototype with
        | Object p ->
          let rec check o p =
            match o#getPrototypeOf with
            | None -> false
            | Some op when unwrap p == unwrap op -> true
            | Some op -> check op p
          in check o p
        | _ -> raise (!jsc_TypeException ())
        )
      | _ -> false
      )
    | Some bc ->
      js_InstanceofOperator o bc

and js_InstanceofOperator o c =
  if js_Type c <> Object_t then
    raise (!jsc_TypeException ());
  match js_GetMethod c sym_hasInstance with
  | Some instOfHandler ->
    js_ToBoolean (js_Call (Object instOfHandler) c [|o|])
  | None ->
    if not (js_IsCallable c) then
      raise (!jsc_TypeException ());
    js_OrdinaryHasInstance c o

let get_function obj sym =
  if obj#hasProperty sym then
    match obj#prop_get sym with
    | Undefined -> Undefined
    | Object f when f#has_call -> Object f
    | _ -> raise (js_TypeException ())
  else
    Undefined

let js_ToPropertyDescriptor obj =
  let o = js_ToObject obj in
  let enumerable = js_ToBoolean (o#prop_get sym_enumerable) in
  let configurable = js_ToBoolean (o#prop_get sym_configurable) in
  let writable = js_ToBoolean (o#prop_get sym_writable) in
  if o#hasProperty sym_get || o#hasProperty sym_set then begin
    if o#hasProperty sym_writable || o#hasProperty sym_value then
      raise (js_TypeException ());
    let getter = get_function o sym_get in
    let setter = get_function o sym_set in
    object
      inherit property
        ~enumerable: enumerable
        ~configurable: configurable
        ~writable: writable

      method! has_getter = true
      method! get_getter = getter

      method! has_setter = true
      method! get_setter = setter
    end
  end else begin
    let value = ref (o#prop_get sym_value) in
    object
      inherit property
        ~enumerable: enumerable
        ~configurable: configurable
        ~writable: writable

      method! has_value = true
      method! get_value = !value
      method! set_value v = value := v
    end
  end

let js_FromPropertyDescriptor pd =
  let obj = new objectInstance Object.prototype in
  if pd#has_value then begin
    obj#prop_create sym_value pd#get_value;
    obj#prop_create sym_writable (Boolean pd#is_writable);
    obj#prop_create sym_enumerable (Boolean pd#is_enumerable);
    obj#prop_create sym_configurable (Boolean pd#is_configurable);
  end else begin
    obj#prop_create sym_get pd#get_getter;
    obj#prop_create sym_set pd#get_setter;
    obj#prop_create sym_enumerable (Boolean pd#is_enumerable);
    obj#prop_create sym_configurable (Boolean pd#is_configurable);
  end;
  Object (obj :> js_object_instance)

let js_GetOwnPropertyKeysString obj =
  ((new Js_array.stringArrayInstance Array.prototype
    (Arr.of_list (LinkedHashtbl.fold (fun k _ keys ->
      js_KeyFor k :: keys
    ) obj#get_properties []))
  ) :> js_string_array)

let js_DefinePropertyOrThrow obj key desc =
  if not ((get_object obj)#defineOwnProperty key desc) then
    raise (js_TypeException ())
  else
    ()

let js_ToPropertyKey key = match key with
  | Symbol _ -> key
  | _ -> js_SymbolFor (js_ToString key)

let js_EnumerableOwnNames obj =
  let object_part =
    (Arr.of_list (LinkedHashtbl.fold (fun k p keys ->
      if p#is_enumerable then js_KeyFor k :: keys else keys
    ) obj#get_properties [])) in
  ((new stringArrayInstance Array.prototype (
    if obj#is_type tag_Array then begin
      let array_part = Arr.init (Obj.magic obj)#length string_of_int in
      Arr.append array_part object_part
    end else begin
      object_part
    end
  )) :> js_string_array)

let _ =
  jsc_TypeException := js_TypeException;
  jsc_RangeException := js_RangeException;
  jsc_ReferenceException := js_ReferenceException;
  jsc_SyntaxException := js_SyntaxException;
  jsc_Type := js_Type;
  jsc_GetMethod := js_GetMethod;
  jsc_IsCallable := js_IsCallable;
  jsc_ToObject := js_ToObject;
  jsc_ToPrimitive := js_ToPrimitive;
  jsc_OrdinaryToPrimitive := js_OrdinaryToPrimitive;
  jsc_ToPrimitive_Number := js_ToPrimitive_Number;
  jsc_ToPrimitive_String := js_ToPrimitive_String;
  jsc_ToBoolean := js_ToBoolean;
  jsc_ToInteger := js_ToInteger;
  jsc_ToLength := js_ToLength;
  jsc_ToNumber := js_ToNumber;
  jsc_ToString := js_ToString;
  jsc_SameValueNonNumber := js_SameValueNonNumber;
  jsc_SameValue := js_SameValue;
  jsc_Call := js_Call;
  jsc_OrdinaryHasInstance := js_OrdinaryHasInstance;
  jsc_InstanceofOperator := js_InstanceofOperator;
  jsc_Get := js_Get;
  jsc_ToPropertyDescriptor := js_ToPropertyDescriptor;
  jsc_FromPropertyDescriptor := js_FromPropertyDescriptor;
  jsc_GetOwnPropertyKeysString := js_GetOwnPropertyKeysString;
  jsc_DefinePropertyOrThrow := js_DefinePropertyOrThrow;
  jsc_ToPropertyKey := js_ToPropertyKey;
  jsc_EnumerableOwnNames := js_EnumerableOwnNames;;
