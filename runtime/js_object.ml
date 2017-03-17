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
open Js_rtti
open Js_syms
open Js_base
open Js_builtin

class objectInstance ~prototype = object
  inherit baseObject

  val mutable prototype = (prototype :> js_object_instance)

  method getPrototypeOf = Some prototype
  method setPrototypeOf proto =
    (* todo: check for circular prototype chain. *)
    prototype <- proto;
    true

end

class virtual objectPrototypeBase = object
  inherit baseObject

  val mutable typed_prototype : objectPrototype = Obj.magic !object_prototype
  method get_typed_prototype = typed_prototype
  method set_typed_prototype p = typed_prototype <- p
  method getPrototypeOf = Some (typed_prototype :> js_object_instance)
  method setPrototypeOf _p = raise (!jsc_TypeException  ())

  val baseToString = define_function_0 "toString" ret_string
    (fun this -> match this with
    | Missing
    | Undefined -> "[object Undefined]"
    | Null -> "[object Null]"
    | Boolean _ -> "[object Boolean]"
    | String _ -> "[object String]"
    | Number _ -> "[object Number]"
    | Symbol _ -> "[object Symbol]"
    | Object o ->
      (match o#prop_get sym_toStringTag with
      | String tag -> "[object " ^ tag ^ "]"
      | _ ->
        (match o with
        | o when o#is_type tag_Array -> "[object Array]"
        | o when o#is_type tag_Arguments -> "[object Arguments]"
        | o when o#is_type tag_RegExp -> "[object RegExp]"
        | o when o#is_type tag_Date -> "[object Date]"
        | o when o#is_type tag_Error -> "[object Error]"
        | o when o#has_call -> "[object Function]"
        | _ -> "[object Object]"
        )
      )
    )

  val hasOwnProperty = define_function_1 "hasOwnProperty"
    ret_boolean arg_dyn
    (fun this key ->
      match (!jsc_ToObject this)#getOwnProperty (!jsc_ToPropertyKey key) with
      | None -> false
      | Some _ -> true
    )
  method get_hasOwnProperty = hasOwnProperty

  val valueOf = define_function_0 "valueOf"
    ret_dyn
    (fun this -> Object (!jsc_ToObject this))
  method get_valueOf = valueOf

  val isPrototypeOf = define_function_1 "isPrototypeOf"
    ret_boolean arg_dyn
    (fun this proto ->
      let v = match proto with
      | Object o -> Some o
      | _ -> None in
      let o = !jsc_ToObject this in
      let rec check_proto = function
        | None -> false
        | Some v when v == o -> true
        | Some v -> check_proto (v#getPrototypeOf)
      in check_proto v
    )
  method get_isPrototypeOf = isPrototypeOf

  val propertyIsEnumerable = define_function_1 "propertyIsEnumerable"
    ret_boolean arg_dyn
    (fun this key ->
      let o = !jsc_ToObject this in
      let k = !jsc_ToPropertyKey key in
      match o#getOwnProperty k with
      | None -> false
      | Some p -> p#is_enumerable
    )
  method get_propertyIsEnumerable = propertyIsEnumerable

end

and objectPrototype = object
  inherit objectPrototypeBase as super

  method! getPrototypeOf = None
  method! setPrototypeOf (_proto : js_object_instance) =
    false
  method get_constructor : objectConstructor =
    Obj.magic !object_constructor

  method get_toString = baseToString

  method! is_type tag = tag = tag_ObjectPrototype || super#is_type tag

  method! setup_props = setup_props (super#setup_props) [
    (sym_constructor,          new objectProperty object_constructor);
    (sym_toString,             new builtinObjectProperty baseToString);
    (sym_valueOf,              new builtinObjectProperty valueOf);
    (sym_hasOwnProperty,       new builtinObjectProperty hasOwnProperty);
    (sym_isPrototypeOf,        new builtinObjectProperty isPrototypeOf);
    (sym_propertyIsEnumerable, new builtinObjectProperty propertyIsEnumerable);
  ];
end

and objectConstructor ~prototype = object
  inherit [objectPrototype, js_function_prototype] builtinConstructor
    1.0 "Object" prototype as super

  method construct (_value : js_value) =
    new objectInstance !instance_prototype

  val defineProperty = define_function_3 "defineProperty"
    ret_dyn arg_dyn arg_dyn arg_dyn
    (fun (_ : js_value) obj key desc ->
      if !jsc_Type obj <> Object_t then
        raise (!jsc_TypeException ());
      let k = !jsc_ToPropertyKey key in
      let p = !jsc_ToPropertyDescriptor desc in
      !jsc_DefinePropertyOrThrow obj k p;
      obj
    )
  method get_defineProperty = defineProperty

  val getPrototypeOf = define_function_1 "getPrototypeOf"
    ret_dyn arg_dyn
    (fun (_ : js_value) obj -> match obj with
    | Object o ->
      (match o#getPrototypeOf with
      | None -> Null
      | Some p -> Object (p :> js_object_instance)
      )
    | Number _ -> Object !number_prototype
    | String _ -> Object !string_prototype
    | Boolean _ -> Object !boolean_prototype
    | _ -> raise (!jsc_TypeException ())
    )
  method get_getPrototypeOf = getPrototypeOf

  val getOwnPropertyDescriptor = define_function_2 "getOwnPropertyDescriptor"
    ret_dyn arg_dyn arg_dyn
    (fun (_ : js_value) obj prop ->
      let o = (!jsc_ToObject obj) in
      let key = !jsc_ToPropertyKey prop in
      match (o#getOwnProperty key) with
      | Some desc -> !jsc_FromPropertyDescriptor desc
      | None -> Undefined
    )
  method get_getOwnPropertyDescriptor = getOwnPropertyDescriptor

  val getOwnPropertyNames = define_function_1 "getOwnPropertyNames"
    ret_string_arr arg_dyn
    (fun (_ : js_value) obj ->
      !jsc_GetOwnPropertyKeysString (!jsc_ToObject obj)
    )
  method get_getOwnPropertyNames = getOwnPropertyNames

  val is = define_function_2 "is"
    ret_boolean arg_dyn arg_dyn
    (fun (_ : js_value) value1 value2 ->
      !jsc_SameValue value1 value2
    )
  method get_is = is

  val keys = define_function_1 "keys"
    ret_string_arr arg_dyn
    (fun (_ : js_value) obj ->
      !jsc_EnumerableOwnNames (!jsc_ToObject obj)
    )
  method get_keys = keys

  val isExtensible = define_function_1 "isExtensible"
    ret_boolean arg_dyn
    (fun (_ : js_value) (_obj : js_value) ->
      (* TODO: not yet implemented *)
      true
    )
  method get_isExtensible = isExtensible

  val isSealed = define_function_1 "isSealed"
    ret_boolean arg_dyn
    (fun (_ : js_value) (_obj : js_value) ->
      (* TODO: not yet implemented *)
      false
    )
  method get_isSealed = isSealed

  val isFrozen = define_function_1 "isFrozen"
    ret_boolean arg_dyn
    (fun (_ : js_value) (_obj : js_value) ->
      (* TODO: not yet implemented *)
      false
    )
  method get_isFrozen = isFrozen

  val preventExtensions = define_function_1 "preventExtensions"
    ret_dyn arg_dyn
    (fun _ _  ->
      (* TODO: not yet implemented *)
      Undefined
    )
  method get_preventExtensions = preventExtensions

  val create = define_function_2 ~length:1.0 "create"
    ret_dyn arg_dyn arg_dyn
    (fun _ proto props ->
      let obj = match proto with
        | Null -> object
            inherit baseObject
            method getPrototypeOf = None
            method setPrototypeOf (_ : js_object_instance) = false
          end
        | Object o -> object
            inherit baseObject
            method getPrototypeOf = Some o
            method setPrototypeOf (_ : js_object_instance) = false
          end
        | _ -> raise (!jsc_TypeException ())
      in match props with
      | Missing
      | Undefined ->
        Object obj
      | Object o ->
        let open LinkedHashtbl in
        let rec copy_property = function
          | None -> ()
          | Some p when p.value#is_enumerable ->
            let key = js_SymbolFor (js_KeyFor p.key) in
            if p.value#has_value then begin
              let val_ref = ref p.value#get_value in
              ignore (obj#defineOwnProperty key (object
                inherit property
                  ~enumerable: p.value#is_enumerable
                  ~writable: p.value#is_writable
                  ~configurable: p.value#is_configurable
                method! has_value = true
                method! get_value = !val_ref
                method! set_value v = val_ref := v
              end))
            end else begin
            ignore (obj#defineOwnProperty key (object
              inherit property
                ~enumerable: p.value#is_enumerable
                ~writable: p.value#is_writable
                ~configurable: p.value#is_configurable
              method! has_getter = p.value#has_getter
              method! get_getter = p.value#get_getter
              method! has_setter = p.value#has_setter
              method! get_setter = p.value#get_setter
            end))
            end
          | Some p ->
            copy_property p.next;
        in
        copy_property o#get_properties.first;
        Object obj
      | _ ->
        raise (!jsc_TypeException ())
    )
  method get_create = create

  method! setup_props = setup_props (super#setup_props) [
    (sym_defineProperty,           new builtinObjectProperty defineProperty);
    (sym_getPrototypeOf,           new builtinObjectProperty getPrototypeOf);
    (sym_getOwnPropertyDescriptor, new builtinObjectProperty getOwnPropertyDescriptor);
    (sym_getOwnPropertyNames,      new builtinObjectProperty getOwnPropertyNames);
    (sym_is,                       new builtinObjectProperty is);
    (sym_keys,                     new builtinObjectProperty keys);
    (sym_isExtensible,             new builtinObjectProperty isExtensible);
    (sym_isSealed,                 new builtinObjectProperty isSealed);
    (sym_isFrozen,                 new builtinObjectProperty isFrozen);
    (sym_preventExtensions,        new builtinObjectProperty preventExtensions);
  ];
end

module Object = struct
  let prototype = new objectPrototype
  let constructor = new objectConstructor prototype
  let empty = new objectInstance prototype
end

let _ =
  object_prototype := (Object.prototype :> js_object_instance);
  object_constructor := (Object.constructor :> js_function_instance);
