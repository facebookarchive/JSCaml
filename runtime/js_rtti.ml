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


(* Cast an object to a specific class, throwing TypeError on failure *)
let dynamic_cast o tag =
  if o#is_type tag then Obj.magic o else raise (!jsc_TypeException ())

(* Map associating structural type names to tags *)
module TagMap = Map.Make(String)
let tag_map = ref (TagMap.empty);;

(* Creates an tag for a type name *)
let js_TagFor name =
  try
    TagMap.find name !tag_map
  with Not_found ->
    let tag = TagMap.cardinal !tag_map in
    tag_map := TagMap.add name tag !tag_map;
    tag

(* Runtime Type Information for builtins *)
let tag_Object = js_TagFor "Object";;
let tag_ObjectPrototype = js_TagFor "ObjectPrototype";;
let tag_Function = js_TagFor "Function";;
let tag_FunctionPrototype = js_TagFor "FunctionPrototype";;
let tag_FunctionPrototypeApply = js_TagFor "FunctionPrototypeApply";;
let tag_FunctionPrototypeBind = js_TagFor "FunctionPrototypeBind";;
let tag_FunctionPrototypeCall = js_TagFor "FunctionPrototypeCall";;
let tag_BoundFunction = js_TagFor "BoundFunction";;
let tag_Boolean = js_TagFor "Boolean";;
let tag_BooleanPrototype = js_TagFor "BooleanPrototype";;
let tag_Number = js_TagFor "Number";;
let tag_NumberPrototype = js_TagFor "NumberPrototype";;
let tag_String = js_TagFor "String";;
let tag_StringPrototype = js_TagFor "StringPrototype";;
let tag_Symbol = js_TagFor "Symbol";;
let tag_SymbolPrototype = js_TagFor "SymbolPrototype";;
let tag_Array = js_TagFor "Array";;
let tag_DynamicWrapper = js_TagFor "@@DynamicWrapper";;
let tag_ArrayWrapper = js_TagFor "@@ArrayWrapper";;
let tag_ArrayPrototype = js_TagFor "ArrayPrototype";;
let tag_ArrayIterator = js_TagFor "ArrayIterator";;
let tag_ArrayIteratorPrototype = js_TagFor "ArrayIteratorPrototype";;
let tag_ArrayIteratorResult = js_TagFor "ArrayIteratorResult";;
let tag_Arguments = js_TagFor "Arguments";;
let tag_Date = js_TagFor "Date";;
let tag_DatePrototype = js_TagFor "DatePrototype";;
let tag_RegExp = js_TagFor "RegExp";;
let tag_RegExpPrototype = js_TagFor "RegExpPrototype";;
let tag_Error = js_TagFor "Error";;
let tag_ErrorPrototype = js_TagFor "ErrorPrototype";;
let tag_TypeError = js_TagFor "TypeError";;
let tag_TypeErrorPrototype = js_TagFor "TypeErrorPrototype";;
let tag_RangeError = js_TagFor "RangeError";;
let tag_RangeErrorPrototype = js_TagFor "RangeErrorPrototype";;
let tag_Global = js_TagFor "Global";;
let tag_GlobalPrototype = js_TagFor "GlobalPrototype";;
let tag_EvalError = js_TagFor "EvalError";;
let tag_EvalErrorPrototype = js_TagFor "EvalErrorPrototype";;
let tag_ReferenceError = js_TagFor "ReferenceError";;
let tag_ReferenceErrorPrototype = js_TagFor "ReferenceErrorPrototype";;
let tag_SyntaxError = js_TagFor "SyntaxError";;
let tag_SyntaxErrorPrototype = js_TagFor "SyntaxErrorPrototype";;
let tag_URIError = js_TagFor "URIError";;
let tag_URIErrorPrototype = js_TagFor "URIErrorPrototype";;
let tag_console = js_TagFor "console";;
let tag_Reflect = js_TagFor "Reflect";;
