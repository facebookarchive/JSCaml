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

val js_TypeException : unit -> exn

val js_RangeException : unit -> exn

val js_ReferenceException : unit -> exn

val js_SyntaxException : unit -> exn

val js_Type : js_value -> js_type

val js_GetMethod : js_value -> js_value -> js_object_instance option

val js_IsCallable : js_value -> bool

val js_OrdinaryToPrimitive : js_object_instance -> to_primitive_hint -> js_value

val js_ToPrimitive : js_value -> js_value

val js_ToPrimitive_Number : js_value -> js_value

val js_ToPrimitive_String : js_value -> js_value

val js_ToObject : js_value -> js_object_instance

val js_ToNumber : js_value -> float

val js_ToInteger : js_value -> float

val js_ToLength : js_value -> float

val js_ToBoolean : js_value -> bool

val js_ToString : js_value -> string

val js_SameValueNonNumber : js_value -> js_value -> bool

val js_SameValue : js_value -> js_value -> bool

val js_Call : js_value -> js_value -> js_value array -> js_value

val js_OrdinaryHasInstance : js_value -> js_value -> bool

val js_InstanceofOperator : js_value -> js_value -> bool

val js_Get : js_value -> js_value -> js_value

val js_ToPropertyDescriptor : js_value -> js_prop

val js_FromPropertyDescriptor : js_prop -> js_value

val js_GetOwnPropertyKeysString : js_object_instance -> js_string_array

val js_DefinePropertyOrThrow : js_value -> js_value -> js_prop -> unit

val js_ToPropertyKey : js_value -> js_value

val js_EnumerableOwnNames : js_object_instance -> js_string_array
