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
open Js_array
open Js_syms
open Js_prop

let template_cache : (int, js_value) Hashtbl.t = Hashtbl.create 0;;
let template_ids : (string array, int) Hashtbl.t = Hashtbl.create 0;;


let intern_template raw cooked =
  try Hashtbl.find template_ids raw
  with Not_found ->
    (* Map the raw elements to a unique ID. *)
    let id = Hashtbl.length template_ids in
    Hashtbl.add template_ids raw id;

    (* Create the template object. *)
    let arr_raw = new constStringArrayInstance Array.prototype raw in
    let arr_cooked = new constStringArrayInstance Array.prototype cooked in
    ignore (arr_cooked#defineOwnProperty sym_raw (object
      inherit property ~enumerable:false ~writable:false ~configurable:false
        method! has_value = true
        method! get_value = Object (arr_raw :> js_object_instance)
    end));
    let template = Object (arr_cooked :> js_object_instance) in

    (* Map the ID to the template object. *)
    Hashtbl.add template_cache id template;

    (* Return the unique template ID. *)
    id

let get_template id =
  Hashtbl.find template_cache id
