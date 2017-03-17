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

(* Extracts the ID of a symbol. *)
let symbol_id = function
| Symbol(id, _) -> id
| v -> failwith ("symbol_id: not a symbol: " ^ (!jsc_ToString v))

(* Extracts the Key of a symbol. *)
let symbol_key = function
| Symbol(_, key) -> key
| v -> failwith ("symbol_id: not a symbol: " ^ (!jsc_ToString v))

(* Number of symbol IDs reserved to the runtime. *)
let reserved_syms = 1000;

(* Map associating symbol names to symbols. *)
module InternMap = Map.Make(String)
let intern_map = ref (InternMap.empty);;
let key_map = Hashtbl.create 0

(* Creates an interned symbol for a given name. *)
let js_SymbolFor name =
  try
    InternMap.find name !intern_map
  with Not_found ->
    let id = reserved_syms + InternMap.cardinal !intern_map in
    let sym = Symbol(id, name) in
    Hashtbl.add key_map id name;
    intern_map := InternMap.add name sym !intern_map;
    sym

(* Special symbols used by the runtime, hidden from Symbol.for. *)
(* These symbols are accessible through the Symbol constructor. *)
let sym_hasInstance        = Symbol(0, "Symbol.hasInstance");;
let sym_isConcatSpreadable = Symbol(1, "Symbol.isConcatSpreadable");;
let sym_iterator           = Symbol(2, "Symbol.iterator");;
let sym_match              = Symbol(3, "Symbol.match");;
let sym_replace            = Symbol(4, "Symbol.replace");;
let sym_search             = Symbol(5, "Symbol.search");;
let sym_species            = Symbol(6, "Symbol.species");;
let sym_split              = Symbol(7, "Symbol.split");;
let sym_toStringTag        = Symbol(8, "Symbol.toStringTag");;
let sym_unscopables        = Symbol(9, "Symbol.unscopables");;

(* Returns the symbol's name for an ID. *)
let js_KeyFor = function
  | id when id = symbol_id sym_hasInstance        -> "Symbol.hasInstance"
  | id when id = symbol_id sym_isConcatSpreadable -> "Symbol.isConcatSpreadable"
  | id when id = symbol_id sym_iterator           -> "Symbol.iterator"
  | id when id = symbol_id sym_match              -> "Symbol.match"
  | id when id = symbol_id sym_replace            -> "Symbol.replace"
  | id when id = symbol_id sym_search             -> "Symbol.search"
  | id when id = symbol_id sym_species            -> "Symbol.species"
  | id when id = symbol_id sym_split              -> "Symbol.split"
  | id when id = symbol_id sym_toStringTag        -> "Symbol.toStringTag"
  | id when id = symbol_id sym_unscopables        -> "Symbol.unscopables"
  | id -> Hashtbl.find key_map id

(* Common symbols used by the runtime, also returned by Symbol.for. *)
let sym_prototype                 = js_SymbolFor "prototype";;
let sym_constructor               = js_SymbolFor "constructor";;
let sym_toPrimitive               = js_SymbolFor "toPrimitive";;
let sym_toString                  = js_SymbolFor "toString";;
let sym_valueOf                   = js_SymbolFor "valueOf";;
let sym_indexOf                   = js_SymbolFor "indexOf";;
let sym_name                      = js_SymbolFor "name";;
let sym_length                    = js_SymbolFor "length";;
let sym_value                     = js_SymbolFor "value";;
let sym_writable                  = js_SymbolFor "writable";;
let sym_enumerable                = js_SymbolFor "enumerable";;
let sym_configurable              = js_SymbolFor "configurable";;
let sym_message                   = js_SymbolFor "message";;
let sym_join                      = js_SymbolFor "join";;
let sym_isArray                   = js_SymbolFor "isArray";;
let sym_apply                     = js_SymbolFor "apply";;
let sym_call                      = js_SymbolFor "call";;
let sym_bind                      = js_SymbolFor "bind";;
let sym_defineProperty            = js_SymbolFor "defineProperty"
let sym_getPrototypeOf            = js_SymbolFor "getPrototypeOf"
let sym_getOwnPropertyDescriptor  = js_SymbolFor "getOwnPropertyDescriptor"
let sym_getOwnPropertyNames       = js_SymbolFor "getOwnPropertyNames"
let sym_is                        = js_SymbolFor "is"
let sym_keys                      = js_SymbolFor "keys"
let sym_isExtensible              = js_SymbolFor "isExtensible"
let sym_isSealed                  = js_SymbolFor "isSealed"
let sym_isFrozen                  = js_SymbolFor "isFrozen"
let sym_preventExtensions         = js_SymbolFor "preventExtensions"
let sym_hasOwnProperty            = js_SymbolFor "hasOwnProperty";;
let sym_isPrototypeOf             = js_SymbolFor "isPrototypeOf";;
let sym_propertyIsEnumerable      = js_SymbolFor "propertyIsEnumerable";;
let sym_isNaN                     = js_SymbolFor "isNaN";;
let sym_isFinite                  = js_SymbolFor "isFinite";;
let sym_EPSILON                   = js_SymbolFor "EPSILON";;
let sym_MAX_SAFE_INTEGER          = js_SymbolFor "MAX_SAFE_INTEGER";;
let sym_MAX_VALUE                 = js_SymbolFor "MAX_VALUE";;
let sym_MIN_SAFE_INTEGER          = js_SymbolFor "MIN_SAFE_INTEGER";;
let sym_MIN_VALUE                 = js_SymbolFor "MIN_VALUE";;
let sym_NaN                       = js_SymbolFor "NaN";;
let sym_NEGATIVE_INFINITY         = js_SymbolFor "NEGATIVE_INFINITY";;
let sym_POSITIVE_INFINITY         = js_SymbolFor "POSITIVE_INFINITY";;
let sym_Math                      = js_SymbolFor "Math";;
let sym_Array                     = js_SymbolFor "Array";;
let sym_Object                    = js_SymbolFor "Object";;
let sym_Boolean                   = js_SymbolFor "Boolean";;
let sym_Date                      = js_SymbolFor "Date";;
let sym_Error                     = js_SymbolFor "Error";;
let sym_Function                  = js_SymbolFor "Function";;
let sym_JSON                      = js_SymbolFor "JSON";;
let sym_Number                    = js_SymbolFor "Number";;
let sym_RegExp                    = js_SymbolFor "RegExp";;
let sym_String                    = js_SymbolFor "String";;
let sym_undefined                 = js_SymbolFor "undefined";;
let sym_global                    = js_SymbolFor "global";;
let sym_Infinity                  = js_SymbolFor "Infinity";;
let sym_console                   = js_SymbolFor "console";;
let sym_pop                       = js_SymbolFor "pop";;
let sym_push                      = js_SymbolFor "push";;
let sym_values                    = js_SymbolFor "values";;
let sym_get                       = js_SymbolFor "get";;
let sym_set                       = js_SymbolFor "set";;
let sym_abs                       = js_SymbolFor "abs";;
let sym_acos                      = js_SymbolFor "acos";;
let sym_acosh                     = js_SymbolFor "acosh";;
let sym_asin                      = js_SymbolFor "asinh";;
let sym_asinh                     = js_SymbolFor "asin";;
let sym_atan                      = js_SymbolFor "atan";;
let sym_atan2                     = js_SymbolFor "atan2";;
let sym_atanh                     = js_SymbolFor "atanh";;
let sym_ceil                      = js_SymbolFor "ceil";;
let sym_clz32                     = js_SymbolFor "clz32";;
let sym_cos                       = js_SymbolFor "cos";;
let sym_cosh                      = js_SymbolFor "cosh";;
let sym_exp                       = js_SymbolFor "exp";;
let sym_expm1                     = js_SymbolFor "expm1";;
let sym_floor                     = js_SymbolFor "floor";;
let sym_log                       = js_SymbolFor "log";;
let sym_log1p                     = js_SymbolFor "log1p";;
let sym_log10                     = js_SymbolFor "log10";;
let sym_log2                      = js_SymbolFor "log2";;
let sym_pow                       = js_SymbolFor "pow";;
let sym_random                    = js_SymbolFor "random";;
let sym_round                     = js_SymbolFor "round";;
let sym_sign                      = js_SymbolFor "sign";;
let sym_sin                       = js_SymbolFor "sin";;
let sym_sinh                      = js_SymbolFor "sinh";;
let sym_sqrt                      = js_SymbolFor "sqrt";;
let sym_tan                       = js_SymbolFor "tan";;
let sym_tanh                      = js_SymbolFor "tanh";;
let sym_trunc                     = js_SymbolFor "trunc";;
let sym_E                         = js_SymbolFor "E";;
let sym_LN10                      = js_SymbolFor "LN10";;
let sym_LN2                       = js_SymbolFor "LN2";;
let sym_LOG10E                    = js_SymbolFor "LOG10E";;
let sym_LOG2E                     = js_SymbolFor "LOG2E";;
let sym_PI                        = js_SymbolFor "PI";;
let sym_SQRT1_2                   = js_SymbolFor "SQRT1_2";;
let sym_SQRT2                     = js_SymbolFor "SQRT2";;
let sym_next                      = js_SymbolFor "next";;
let sym_done                      = js_SymbolFor "done";;
let sym_of                        = js_SymbolFor "of";;
let sym_exec                      = js_SymbolFor "exec";;
let sym_test                      = js_SymbolFor "test";;
let sym_input                     = js_SymbolFor "input";;
let sym_index                     = js_SymbolFor "index";;
let sym_lastIndex                 = js_SymbolFor "lastIndex";;
let sym_ignoreCase                = js_SymbolFor "ignoreCase";;
let sym_unicode                   = js_SymbolFor "unicode";;
let sym_sticky                    = js_SymbolFor "sticky";;
let sym_multiline                 = js_SymbolFor "multiline";;
let sym_source                    = js_SymbolFor "source";;
let sym_flags                     = js_SymbolFor "flags";;
let sym_pattern                   = js_SymbolFor "pattern";;
let sym_parse                     = js_SymbolFor "parse";;
let sym_stringify                 = js_SymbolFor "stringify";;
let sym_toJSON                    = js_SymbolFor "toJSON";;
let sym_raw                       = js_SymbolFor "raw";;
let sym_Reflect                   = js_SymbolFor "Reflect";;
let sym_getTime                   = js_SymbolFor "getTime";;
let sym_setTime                   = js_SymbolFor "setTime";;
let sym_getMonth                  = js_SymbolFor "getMonth";;
let sym_getUTCMonth               = js_SymbolFor "getUTCMonth";;
let sym_getUTCFullYear            = js_SymbolFor "getUTCFullYear";;
let sym_getFullYear               = js_SymbolFor "getFullYear";;
let sym_getDay                    = js_SymbolFor "getDay";;
let sym_getUTCDay                 = js_SymbolFor "getUTCDay";;
let sym_getMinutes                = js_SymbolFor "getMinutes";;
let sym_getUTCMinutes             = js_SymbolFor "getUTCMinutes";;
let sym_getSeconds                = js_SymbolFor "getSeconds";;
let sym_getUTCSeconds             = js_SymbolFor "getUTCSeconds";;
let sym_getMilliseconds           = js_SymbolFor "getMilliseconds";;
let sym_getUTCMilliseconds        = js_SymbolFor "getUTCMilliseconds";;
let sym_getTimezoneOffset         = js_SymbolFor "getTimezoneOffset";;
let sym_setMilliseconds           = js_SymbolFor "setMilliseconds";;
let sym_setUTCMilliseconds        = js_SymbolFor "setUTCMilliseconds";;
let sym_setSeconds                = js_SymbolFor "setSeconds";;
let sym_setUTCSeconds             = js_SymbolFor "setUTCSeconds";;
let sym_setHours                  = js_SymbolFor "setHours";;
let sym_setUTCHours               = js_SymbolFor "setUTCHours";;
let sym_setMinutes                = js_SymbolFor "setMinutes";;
let sym_setUTCMinutes             = js_SymbolFor "setUTCMinutes";;
let sym_setDate                   = js_SymbolFor "setDate";;
let sym_setUTCDate                = js_SymbolFor "setUTCDate";;
let sym_setMonth                  = js_SymbolFor "setMonth";;
let sym_setUTCMonth               = js_SymbolFor "setUTCMonth";;
let sym_setFullYear               = js_SymbolFor "setFullYear";;
let sym_setUTCFullYear            = js_SymbolFor "setUTCFullYear";;
let sym_toDateString              = js_SymbolFor "toDateString";;
let sym_toTimeString              = js_SymbolFor "toTimeString";;
let sym_toUTCString               = js_SymbolFor "toUTCString";;
let sym_toISOString               = js_SymbolFor "toISOString";;
