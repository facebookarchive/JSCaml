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
open Js_object
open Js_core
open Js_rtti
open Js_syms

module KeySet = Set.Make(String)

open LinkedHashtbl
type node = (int, js_prop) entry

(* Property iterators go through the string representation of enumerable keys *)
type iter_property =
  { (* Current index when iterating through an array *)
    mutable index : int;
    (* Current node of the linked hash map containing the properties *)
    mutable keys : node option;
    (* Current object in the prototype chain *)
    mutable obj : js_object_instance;
    (* True if iteration proceeds through array keys 0...length - 1 *)
    mutable inside_arr : bool;
    (* Set of visited keys for deduplication *)
    mutable visited : KeySet.t;
  }

let get_iter_property = function
  | Null
  | Missing
  | Undefined ->
    { index = 0;
      keys = None;
      obj = Object.empty;
      inside_arr = true;
      visited = KeySet.empty;
    }
  | value ->
    let obj = js_ToObject value in
    { index = 0;
      keys = obj#get_properties.first;
      obj = obj;
      inside_arr = true;
      visited = KeySet.empty;
    }

let rec step_iter_property iter =
  (* Traverses string keys of prototypes. *)
  let rec iter_named_props iter =
    match iter.keys with
    | None ->
      let rec next_proto = function
        | None ->
          (* Iteration should be stopped before this. *)
          None
        | Some obj ->
          if obj#is_type tag_Array && (Obj.magic obj)#length > 0 then begin
            (* Iterate through array keys *)
            iter.index <- 1;
            iter.obj <- obj;
            iter.inside_arr <- true;
            iter.keys <- obj#get_properties.first;
            Some "0"
          end else begin
            (* Move on to the prototype's properties. *)
            match obj#get_properties.first with
            | None -> next_proto obj#getPrototypeOf
            | Some e ->
              (* Start iterating through the linked hash map *)
              iter.obj <- obj;
              iter.inside_arr <- false;
              iter.keys <- Some e;
              iter_named_props iter
          end
      in next_proto iter.obj#getPrototypeOf
    | Some e when e.value#is_enumerable ->
      iter.keys <- e.next;
      Some (js_KeyFor e.key)
    | Some e ->
      iter.keys <- e.next;
      iter_named_props iter in
  (* Traverses integer keys of arrays. *)
  let iter_indices iter =
    if not (iter.obj#is_type tag_Array) then
      iter_named_props iter
    else begin
      let arr : js_array_instance = Obj.magic iter.obj in
      if iter.index >= arr#length then
        iter_named_props iter
      else begin
        let index = iter.index in
        iter.index <- index + 1;
        Some (string_of_int index)
      end
    end in
  (* Fetch an element from the iterator & deduplicate it. *)
  match (if iter.inside_arr then iter_indices else iter_named_props) iter with
    | None -> None
    | Some key when KeySet.mem key iter.visited ->
      step_iter_property iter
    | Some key ->
      iter.visited <- KeySet.add key iter.visited;
      Some key


let get_iter_value = function
  | Null
  | Undefined
  | Missing
  | Number _
  | Boolean _
  | Symbol _ ->
    raise (js_TypeException ())
  | String s -> object
      val mutable i = 0
      method step =
        if i >= String.length s then
          None
        else begin
          i <- i + 1;
          Some (String (String.sub s (i - 1) 1))
        end
    end
  | Object o when o#is_type tag_Array ->
    (Obj.magic o)#iterator
  | Object _ ->
    failwith "not implemented: Object iterator"

let step_iter_value iter =
  iter#step
