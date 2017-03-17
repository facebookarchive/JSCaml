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
open Js_prop
open Js_rtti

(* Object instance implementation, base class of the hierarchy *)
class virtual baseObject = object(self)
  (* Mapping from symbols to property descriptors. *)
  val mutable props : js_prop_map option = None

  method getOwnProperty sym : js_prop option =
    try Some (LinkedHashtbl.find (self#get_properties) (symbol_id sym))
    with Not_found -> None

  method defineOwnProperty sym prop =
    match self#getOwnProperty sym with
    | Some ownProp when ownProp#has_value && prop#has_value ->
      (* Reuse the existing descriptor. *)
      ownProp#set_value (prop#get_value);
      ownProp#set_enumerable prop#is_enumerable;
      ownProp#set_writable prop#is_writable;
      ownProp#set_configurable prop#is_configurable;
      true
    | _ ->
      LinkedHashtbl.add (self#get_properties) (symbol_id sym) prop;
      true

  method hasProperty sym =
    if LinkedHashtbl.mem (self#get_properties) (symbol_id sym)
      then true
      else match self#getPrototypeOf with
        | None -> false
        | Some p -> p#hasProperty sym

  method delete sym =
    match self#getOwnProperty sym with
    | None -> true
    | Some prop when prop#is_configurable ->
      LinkedHashtbl.remove (self#get_properties) (symbol_id sym);
      true
    | _ ->
      raise (!jsc_TypeException ())

  method get key receiver =
    match self#getOwnProperty key with
    | None ->
      (match self#getPrototypeOf with
      | Some o -> o#get key receiver
      | None -> Undefined
      )
    | Some prop when prop#has_value ->
      prop#get_value
    | Some prop when prop#has_getter ->
      (match prop#get_getter with
      | Undefined -> Undefined
      | getter -> !jsc_Call getter (Object receiver) [||]
      )
    | _ ->
      Undefined

  method set key value receiver =
    let set_data_property key value =
      match receiver#getOwnProperty key with
      | Some prop when prop#has_value && prop#is_writable ->
        prop#set_value value;
        true
      | Some _ ->
        false
      | None ->
        let val_ref = ref value in
        ignore (receiver#defineOwnProperty key (object
          inherit property ~enumerable:true ~configurable:true ~writable:true
          method! has_value = true
          method! get_value = !val_ref
          method! set_value v = val_ref := v
        end));
        true
    in match self#getOwnProperty key with
    | None ->
      (match self#getPrototypeOf with
      | Some o -> o#set key value receiver
      | None -> set_data_property key value
      )
    | Some ownProp when ownProp#has_value && ownProp#is_writable ->
      set_data_property key value
    | Some ownProp when ownProp#has_setter ->
      (* Invoke the setter on this object *)
      (match ownProp#get_setter with
      | Undefined ->
        false
      | setter ->
        ignore (!jsc_Call setter (Object receiver) [|value|]);
        true
      )
    | Some _ ->
      false

  method prop_set key value =
    ignore (self#set key value (self :> js_object_instance));
  method prop_get key =
    self#get key (self :> js_object_instance)

  method prop_create sym value =
    (* TODO: implement this according to the spec *)
    let val_ref = ref value in
    ignore (self#defineOwnProperty sym (object
      inherit property ~enumerable:true ~configurable:true ~writable:true
      method! has_value = true
      method! get_value = !val_ref
      method! set_value v = val_ref := v
    end));

  method setup_props : js_prop_map =
    LinkedHashtbl.create 1

  method get_properties : js_prop_map =
    match props with
    | None ->
      let ps = self#setup_props in
      props <- Some ps;
      ps
    | Some ps ->
      ps

  method dyn_call (_ : js_value) (_ : js_value array) : js_value =
    raise (!jsc_TypeException ())

  method dyn_construct (_ : js_value array) : js_value =
    raise (!jsc_TypeException ())

  method has_call = false
  method has_construct = false

  method virtual getPrototypeOf : js_object_instance option
  method virtual setPrototypeOf : js_object_instance -> bool
  method is_type tag = tag = tag_Object
end

(* Base class for all functions. *)
class virtual baseFunction ~length ~name = object
  inherit baseObject as super

  val length: float = length
  method get_length = length
  method set_length (_: float) : unit = raise (!jsc_TypeException ())

  val name: string = name
  method get_name = name
  method set_name (_ : string) : unit = raise (!jsc_TypeException ())

  method! is_type tag = tag = tag_Function || super#is_type tag
  method! has_call = true
  method! has_construct = true

  method! setup_props = setup_props (super#setup_props) [
    (sym_length, new constNumberProperty length );
    (sym_name,   new constStringProperty name   );
  ]
end

(* Base class for all constructors. *)
class virtual ['p] baseConstructor ~length ~name ~prototype = object
  inherit baseFunction length name as super

  val instance_prototype : 'p ref = ref prototype
  method get_prototype = !instance_prototype
  method set_prototype proto = instance_prototype := proto

  (* todo: need to get a type tag for the prototype so that
  dynamic updates to the property can be type checked. *)

  method! setup_props = setup_props (super#setup_props) [
    (sym_prototype, new objectProperty instance_prototype);
  ];
end
