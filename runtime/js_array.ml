(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module Arr = Array

open Js_object
open Js_function
open Js_types
open Js_prop
open Js_util
open Js_rtti
open Js_syms
open Js_base
open Js_builtin

let max_arr_index = 4294967294 (* 2 ^32 - 2 *)
let undefined_magic = 0x0FFFFFF0 (* Magic constant to mark holes *)
let max_exp_growth = 4194304 (* 4Mb *)
let linear_growth = 1048576 (* 1Mb *)
let min_capacity = 4

(* Chunk of memory with magic getter/setters *)
type 'a mem_chunk

let mmake len : 'a mem_chunk =
  Obj.magic (Obj.new_block 0 len)

let mget mem idx =
  Obj.magic (Obj.field (Obj.repr mem) idx)

let mset mem idx v =
  Obj.set_field (Obj.repr mem) idx (Obj.repr v)


(* Converts an OCaml array to a memory chunk *)
let mem_of_array arr =
  let mem = mmake (Array.length arr) in
  for i = 0 to Array.length arr - 1 do
    mset mem i (Array.unsafe_get arr i);
  done;
  mem

let is_index s =
  let n = to_uint32 (string_to_float s) in
  (float_to_string (float_of_int n)) = s

let to_index_or_key = function
  | Symbol (id, _) ->
    let s = (js_KeyFor id) in
    let n = int_of_float (string_to_float s) in
    if not (is_index s) || n > max_arr_index || n < 0 then
      None
    else
      Some n
  | _ ->
    None

let dyn_args_to_elems args =
  match Arr.length args with
  | 0 ->
    [||]
  | 1 ->
    let n = !jsc_ToNumber (Arr.unsafe_get args 0) in
    Arr.make (int_of_float n) Undefined
  | _ ->
    args

let args_to_elems args =
  match Arr.length args with
  | 0 ->
    [||]
  | 1 ->
    let n = int_of_float (!jsc_ToLength (Arr.unsafe_get args 0)) in
    Array.make n Undefined
  | _ ->
    args

let alternative_constructor o =
  o#has_construct && o <> (!array_constructor :> js_object_instance)


(* ArrayIteratorPrototype *)
class arrayIteratorPrototype = object
  inherit objectPrototypeBase as super

  val next = define_function_0 "next"
    ret_dyn
    (fun this ->
      let o = !jsc_ToObject this in
      if o#is_type tag_ArrayIterator then begin
        Object ((Obj.magic o)#next :> js_object_instance)
      end else begin
        let r = new objectInstance Object.prototype in
        r#prop_create sym_value Undefined;
        r#prop_create sym_done (Boolean true);
        Object r
      end
    )
  method get_next = next

  method get_toString = baseToString

  method! is_type t = t = tag_ArrayIteratorPrototype || super#is_type t

  method! setup_props = setup_props (super#setup_props) [
    (sym_next,        new builtinObjectProperty next);
    (sym_toStringTag, new constStringProperty "Array Iterator");
  ];
end

(* ArrayIteratorResult *)
and arrayIteratorResult ~value ~is_done = object
  inherit objectInstance Object.prototype as super

  val mutable value = value
  method get_value = value
  method set_value v = value <- v

  val mutable is_done = is_done
  method get_done = is_done
  method set_done v = is_done <- v

  method! is_type tag = tag = tag_ArrayIteratorResult || super#is_type tag

  method! setup_props = setup_props (super#setup_props) [
    (sym_done, object
      inherit builtinProperty
      method !get_value = Boolean is_done
      method !set_value v = is_done <- !jsc_ToBoolean v
    end);
    (sym_value, object
      inherit builtinProperty
      method !get_value = value
      method !set_value v = value <- v
    end);
  ];
end

(* ArrayIterator *)
and arrayIteratorInstance ~prototype ~iterated ~index = object
  inherit baseObject as super

  method! is_type tag = tag = tag_ArrayIterator || super#is_type tag
  method getPrototypeOf = Some (prototype :> js_object_instance)
  method setPrototypeOf _p = raise (!jsc_TypeException ())

  val mutable iterated = Some iterated
  val mutable index = index

  method next =
    (* TODO: add support for key, value, key + value iteration *)
    match iterated with
    | Some obj when obj#is_type tag_Array ->
      let a : js_array_instance = Obj.magic obj in
      let length = a#length in
      if index >= length then begin
        iterated <- None;
        new arrayIteratorResult Undefined true
      end else begin
        let i = index in
        index <- index + 1;
        new arrayIteratorResult (a#get_element i) false
      end
    | Some obj ->
      let length = !jsc_ToLength (obj#prop_get sym_length) in
      if float_of_int index >= length then begin
        iterated <- None;
        new arrayIteratorResult Undefined true
      end else begin
        let key = js_SymbolFor (string_of_int index) in
        index <- index + 1;
        new arrayIteratorResult (obj#prop_get key) false
      end
    | None ->
      new arrayIteratorResult Undefined true
end

module ArrayIterator = struct
  let prototype = new arrayIteratorPrototype
end

(* Array.prototype.join *)
let join = define_function_1 "join" ret_string arg_dyn
  (fun this separator -> match this with
  | Null
  | Missing
  | Undefined ->
    raise (!jsc_TypeException ())
  | Boolean _
  | Number _
  | String _
  | Symbol _ ->
    ""
  | Object o when o#is_type tag_Array ->
    let a : js_array_instance  = Obj.magic o in
    let sep = match separator with
      | Missing | Undefined | Null -> ","
      | separator -> !jsc_ToString separator in
    let to_string = function
      | Null | Undefined -> ""
      | v -> !jsc_ToString v in
    (match a#length with
    | 0 -> ""
    | n ->
      let rec join acc = function
        | 0 -> join (acc ^ to_string (a#get_element 0)) 1
        | i when i < n -> join (acc ^ sep ^ to_string (a#get_element i)) (i + 1)
        | _ -> acc
      in join "" 0
    )
  | Object _ ->
    failwith "unimplemented: Array.prototype.join"
  )

(* Array.prototype.toString *)
let toString = define_function_0 "toString" ret_string
  (fun this -> match this with
  | Object o when o#is_type tag_Array ->
    join#call this Missing
  | _ ->
    failwith "unimplemented: Array.prototype.toString"
  )

(* Array.prototype.pop *)
let pop = define_function_0 "pop" ret_dyn
  (fun this -> match !jsc_ToObject this with
  | o when o#is_type tag_Array ->
    let a : js_array_instance = Obj.magic o in
    if a#length = 0 then
      Undefined
    else begin
      let elem = try a#get_element (a#length - 1) with Not_found -> Undefined in
      a#resize (a#length - 1);
      elem
    end
  | _ ->
    failwith "unimplemented: Array.prototype.pop"
  )

(* Array.prototype.push *)
let push = object
  inherit builtinFunction ~length:1.0 ~name:"call" ~tag:tag_Function
  method! dyn_call this args = match !jsc_ToObject this with
    | o when o#is_type tag_Array ->
      let a : js_array_instance = Obj.magic o in
      let new_length = a#length + Array.length args in
      if new_length > (1 lsl 53) - 1 then
        raise (!jsc_TypeException ());
      args |> Array.iter (fun elem ->
        a#set_element a#length elem;
      );
      Number (float_of_int a#length)
    | _ ->
      failwith "unimplemented: Array.prototype.push"
end

(* Array.prototype.values *)
let values = define_function_0 "values" ret_object
  (fun this ->
    new arrayIteratorInstance ArrayIterator.prototype (!jsc_ToObject this) 0
  )

(* Array *)
class virtual ['e] arrayInstance ~prototype ~elems = object(self)
  inherit baseObject as super

  method! is_type tag = tag == tag_Array || super#is_type tag
  method getPrototypeOf = Some (prototype :> js_object_instance)
  method setPrototypeOf _p = raise (!jsc_TypeException ())

  val mutable elements : 'e mem_chunk = mem_of_array elems
  val mutable length : int = Array.length elems
  val mutable capacity : int = Array.length elems

  method length = length

  method private grow_capacity desired =
    if desired <= min_capacity then
      (* Clamp capacity to a minimum. *)
      min_capacity
    else if desired <= max_exp_growth then begin
      (* The array grows exponentially until max_exp_growth is reached. *)
      let rec find_capacity cap =
        if desired > cap then cap * 2 else find_capacity (cap / 2)
      in find_capacity max_exp_growth
    end else begin
      (* Then it grows linearly. *)
      let linear_part = desired - max_exp_growth in
      let linear_chunks = (linear_part + linear_growth) / linear_growth in
      max_exp_growth + linear_chunks * linear_growth
    end

  method resize len =
    let adjust_capacity new_capacity last_elem =
      (* Create a new buffer of the requested size *)
      let new_elems = mmake new_capacity in
      (* Copy existing items. *)
      for j = 0 to last_elem - 1 do
        mset new_elems j (mget elements j);
      done;
      (* Write a magic integer in unassigned slots. This is used to detect
         access to fields which have not been assigned, returning undefined. *)
      for j = last_elem to new_capacity - 1 do
        mset new_elems j (Obj.magic undefined_magic);
      done;
      elements <- new_elems;
      capacity <- new_capacity;
    in
    if len > capacity then
      (* Grow array if new length exceeds capacity. *)
      adjust_capacity (self#grow_capacity len) length
    else if length < capacity / 4 then
      (* Shrink array by 1/2 if 3/4 of its is empty. *)
      adjust_capacity (max (capacity / 2) min_capacity) len
    else begin
      (* Delete elements past the end of the array *)
      for j = len + 1 to length - 1 do
        mset elements j (Obj.magic undefined_magic);
      done;
    end;
    length <- len;

  method get_typed_element i : 'e =
    if i < 0 || max_arr_index < i then
      let receiver = (self :> js_object_instance) in
      self#unbox (super#get (js_SymbolFor (string_of_int i)) receiver)
    else if i >= length then
      raise (!jsc_RangeException ())
    else begin
      let elem = mget elements i in
      (* If the element is an int block and contains the magic token,
         it means that it does not exist. The magic token is required since
         (), true and false are also represented as integers. *)
      if Obj.is_int (Obj.repr elem) && Obj.magic elem == undefined_magic then
        raise (!jsc_RangeException ())
      else
        elem
    end

  method set_typed_element i (v : 'e) =
    if i < 0 || max_arr_index < i then begin
      self#prop_create (js_SymbolFor (string_of_int i)) (self#box v)
    end else begin
      self#resize (max (i + 1) length);
      mset elements i v
    end

  method get_element i =
    try self#box (self#get_typed_element i)
    with _ ->
      let receiver = (self :> js_object_instance) in
      super#get (js_SymbolFor (string_of_int i)) receiver

  method set_element i v =
    self#set_typed_element i (self#unbox v)

  method delete_element i =
    if i < 0 || max_arr_index < i then
      super#delete (js_SymbolFor (string_of_int i))
    else if i >= length then
      true
    else begin
      mset elements i undefined_magic;
      true
    end

  method has_element = function
    | i when i < 0 || length <= i -> false
    | i ->
      let elem = mget elements i in
      not (Obj.is_int (Obj.repr elem)) || Obj.magic elem <> undefined_magic

  method iterator = object
    val mutable i = 0
    method step =
      if i >= self#length then
        None
      else begin
        i <- i + 1;
        Some (self#get_element (i - 1))
      end
  end

  method virtual elem_type : js_type
  method private virtual box : 'e -> js_value
  method private virtual unbox : js_value -> 'e

  method unwrap = (self :> js_array_instance)

  method! hasProperty key =
    match to_index_or_key key with
    | None -> super#hasProperty key
    | Some n when n < length -> true
    | Some _ -> false

  method! getOwnProperty key =
    match to_index_or_key key with
    | Some n when self#has_element n ->
      Some (object
        inherit property ~enumerable:true ~configurable:true ~writable:true
        method! has_value = true
        method! get_value = self#get_element n
        method! set_value = self#set_element n
      end)
    | _ ->
      super#getOwnProperty key

  method! get key receiver =
    match to_index_or_key key with
    | Some n -> self#get_element n
    | None -> super#get key receiver

  method! set key value receiver =
    match to_index_or_key key with
    | Some n ->
      self#set_element n value;
      true
    | None ->
      super#set key value receiver

  method! delete key =
    match to_index_or_key key with
    | Some n ->
      self#delete_element n
    | None ->
      super#delete key

  method! setup_props = setup_props (super#setup_props) [
    (sym_length, object
      inherit property
        ~enumerable:false
        ~configurable:false
        ~writable:true

      method! has_value = true
      method! get_value = Number (float_of_int length)
      method! set_value v =
        (* TODO: This does not quite follow the spec. *)
        let n = !jsc_ToNumber v in
        let len = int_of_float n in
        if float_of_int len = n && 0 < len && len <= max_arr_index + 1 then
          self#resize len
        else
          raise (!jsc_RangeException ())
    end);
  ];
end

(* Array to wrap an object to provide an array-like interface *)
class ['a] dynamicWrapper obj elem_type box unbox = object(self)
  inherit baseObject

  method! is_type tag =
    tag = tag_DynamicWrapper ||
    tag = tag_Array ||
    obj#is_type tag
  method getPrototypeOf = obj#getPrototypeOf
  method setPrototypeOf = obj#setPrototypeOf

  method get_element i =
    obj#prop_get (js_SymbolFor (string_of_int i))

  method set_element i v =
    obj#prop_set (js_SymbolFor (string_of_int i)) v

  method delete_element i : bool =
    obj#delete (js_SymbolFor (string_of_int i))

  method has_element i =
    match obj#getOwnProperty (js_SymbolFor (string_of_int i)) with
    | Some _ -> true
    | None -> false

  method get_typed_element i : 'a =
    unbox (self#get_element i)

  method set_typed_element i (v : 'a) =
    self#set_element i (box v)

  method length =
    int_of_float (!jsc_ToLength (obj#prop_get sym_length))

  method resize len =
    ignore (obj#prop_set sym_length (Number (float_of_int len)))

  method elem_type : js_type =
    elem_type

  method iterator = object
    val mutable i = 0
    method step =
      if i >= self#length then
        None
      else begin
        i <- i + 1;
        Some (self#get_element (i - 1))
      end
  end

  method unwrap = (self :> js_array_instance)
  method get_obj = (obj :> js_object_instance)

  method! getOwnProperty = obj#getOwnProperty
  method! hasProperty = obj#hasProperty
  method! get = obj#get
  method! set = obj#set
  method! get_properties = obj#get_properties
end

class booleanArrayInstance ~prototype elems = object
  inherit [bool] arrayInstance prototype elems
  method elem_type = Boolean_t
  method private box v = Boolean v
  method private unbox = function
    | Boolean v -> v
    | _ -> raise (!jsc_TypeException ())
end

class numberArrayInstance ~prototype elems = object
  inherit [float] arrayInstance prototype elems
  method elem_type = Number_t
  method private box v = Number v
  method private unbox = function
    | Number v -> v
    | _ -> raise (!jsc_TypeException ())
end

class ['e] objectArrayInstance ~prototype elems = object
  inherit ['e] arrayInstance prototype elems
  method elem_type = Object_t
  method private box v = Object (v :> js_object_instance)
  method private unbox = function
    | Object v ->
      (* TODO: runtime check for element types *)
      (Obj.magic v : 'e)
    | _ -> raise (!jsc_TypeException ())
end

class stringArrayInstance ~prototype elems = object
  inherit [string] arrayInstance prototype elems
  method elem_type = String_t
  method private box v = String v
  method private unbox = function
    | String v -> v
    | _ -> raise (!jsc_TypeException ())
end

class symbolArrayInstance ~prototype elems = object
  inherit [int * string] arrayInstance prototype elems
  method elem_type = Symbol_t
  method private box (id, key) = Symbol (id, key)
  method private unbox = function
    | Symbol (id, key) -> (id, key)
    | _ -> raise (!jsc_TypeException ())
end

class tagArrayInstance ~prototype elems = object
  inherit [js_value] arrayInstance prototype elems as super
  method elem_type = Tagged_t
  method private box v = v
  method private unbox v = v

  method! get_typed_element i =
    try super#get_typed_element i
    with _ ->
      (* Typed arrays throw RangeError, but tagged arrays can handle holes *)
      Undefined
end

(* Array.prototype *)
class arrayPrototype = object(self)
  inherit objectPrototypeBase as super

  method get_toString = toString
  method get_join = join
  method get_pop = pop
  method get_push = push
  method get_values = values
  method get_iterator = values
  method get_length = 0.0
  method length = 0
  method elem_type = Tagged_t
  method get_element (_ : int) = Undefined
  method set_element (_ : int) (_ : js_value) = ()
  method delete_element (_ : int) = true
  method has_element (_ : int) = false
  method resize (_ : int) = ()
  method iterator = object
    method step : js_value option = None
  end
  method unwrap = self

  method get_constructor : arrayConstructor =  Obj.magic !array_constructor
  method! is_type t = t = tag_ArrayPrototype || super#is_type t

  method! setup_props = setup_props (super#setup_props) [
    (sym_constructor, object
      inherit property ~enumerable:false ~configurable:true ~writable:true
      method! has_value = true
      method! get_value = Object (!array_constructor :> js_object_instance)
    end);
    (sym_toString,    new builtinObjectProperty toString);
    (sym_join,        new builtinObjectProperty join);
    (sym_pop,         new builtinObjectProperty pop);
    (sym_push,        new builtinObjectProperty push);
    (sym_values,      new builtinObjectProperty values);
    (sym_iterator,    new builtinObjectProperty values);
    (sym_length,      new builtinNumberProperty 0.0);
  ];
end

and arrayConstructor ~prototype = object
  inherit [arrayPrototype, functionPrototype] builtinConstructor
    1.0 "Array" prototype as super

  val m_isArray = define_function_1 "isArray" ret_boolean arg_dyn
    (fun (_ : js_value) x -> match x with
    | Object o -> o#is_type tag_Array
    | _ -> false
    )
  method get_isArray = m_isArray

  val m_of = define_function_0_rest "of"
    ret_object arg_rest_dyn
    (fun this args ->
      match this with
      | Object o when alternative_constructor o ->
        let len = float_of_int (Arr.length args) in
        let obj = !jsc_ToObject (o#dyn_construct [| Number len |]) in
        args |> Array.iteri (fun i elem ->
          obj#prop_create (js_SymbolFor (string_of_int i)) elem;
        );
        ignore (obj#prop_set sym_length (Number len));
        ((new dynamicWrapper obj Tagged_t id id) :> tagArrayInstance)
      | _ ->
        new tagArrayInstance prototype args
    )
  method get_of = m_of

  method call (_this : js_value) args =
    new tagArrayInstance !instance_prototype (args_to_elems args)

  method! dyn_call (_this : js_value) args =
    let elems = dyn_args_to_elems args in
    ret_object (new tagArrayInstance !instance_prototype elems)

  method construct args =
    new tagArrayInstance !instance_prototype (args_to_elems args)

  method! dyn_construct args =
    let elems = dyn_args_to_elems args in
    ret_object (new tagArrayInstance !instance_prototype elems)

  method! setup_props = setup_props (super#setup_props) [
    (sym_isArray,  new builtinObjectProperty m_isArray);
    (sym_of,       new builtinObjectProperty m_of);
  ];
end


module Array = struct
  let prototype = new arrayPrototype
  let constructor = new arrayConstructor prototype

  let add_spread _spread (arr : 'e arrayInstance) =
    arr
end

let _ =
  array_prototype := (Array.prototype :> js_object_instance);
  array_constructor := (Array.constructor :> js_function_instance)

(* Map containing polymorphic elements. *)
module Mask = Map.Make (struct type t = int let compare = compare end)

(**
 * Wrapper that makes mono-morphic arrays appear to be a polymorphic arrays.
 *
 * Things get complicated when writes create a polymorphic array.
 * This is handled by means of a mask and can cause a runtime type error
 * when the array is cast down to a mono-morphic array.
 *)
class ['e] arrayWrapper (elems : 'e arrayInstance) = object
  inherit baseObject as super

  method! is_type tag =
    tag = tag_ArrayWrapper ||
    tag = tag_Array ||
    super#is_type tag
  method getPrototypeOf = elems#getPrototypeOf
  method setPrototypeOf = elems#setPrototypeOf

  val mutable elements : 'e arrayInstance = elems
  val mutable mask : js_value Mask.t = Mask.empty

  method get_element (i : int) : js_value =
    if Mask.mem i mask then
      Mask.find i mask
    else
      elements#get_element i

  method set_element (i : int) (v : js_value) =
    if !jsc_Type v = elements#elem_type then begin
      elements#set_element i v;
      mask <- Mask.remove i mask;
    end else begin
      mask <- Mask.add i v mask
    end

  method unwrap =
    if Mask.is_empty mask then
      elements
    else
      raise (!jsc_TypeException ())

  method elem_type = elements#elem_type
  method length = elements#length
  method iterator = elements#iterator
  method resize = elements#resize
  method delete_element = elements#delete_element;
  method! defineOwnProperty = elements#defineOwnProperty
  method! getOwnProperty = elements#getOwnProperty
  method! hasProperty = elements#hasProperty
  method! get = elements#get
  method! set = elements#set
  method! get_properties = elements#get_properties
end

(* Immutable arrays. *)
class virtual ['e] constArrayInstance ~prototype elems = object(self)
  inherit ['e] arrayInstance prototype elems as super

  method! set_element _ _ = ()
  method! set_typed_element _ _ = ()
  method unbox = raise (!jsc_TypeException ())

  method! getOwnProperty key =
    match to_index_or_key key with
    | Some n when self#has_element n ->
      Some (object
        inherit property ~enumerable:true ~configurable:false ~writable:false
        method! has_value = true
        method! get_value = self#get_element n
        method! set_value _ = ()
      end)
    | _ ->
      super#getOwnProperty key

  method! setup_props = setup_props (super#setup_props) [
    (sym_length, object
      inherit property
        ~enumerable:false
        ~configurable:false
        ~writable:false

      method! has_value = true
      method! get_value = Number (float_of_int length)
      method! set_value _ = ()
    end);
  ];
end

class constBooleanArrayInstance ~prototype elems = object
  inherit [bool] constArrayInstance prototype elems
  method elem_type = Boolean_t
  method private box v = Boolean v
end

class constNumberArrayInstance ~prototype elems = object
  inherit [float] constArrayInstance prototype elems
  method elem_type = Number_t
  method private box v = Number v
end

class ['e] constObjectArrayInstance ~prototype elems = object
  inherit ['e] constArrayInstance prototype elems
  method elem_type = Object_t
  method private box v = Object (v :> js_object_instance)
end

class constStringArrayInstance ~prototype elems = object
  inherit [string] constArrayInstance prototype elems
  method elem_type = String_t
  method private box v = String v
end

class constSymbolArrayInstance ~prototype elems = object
  inherit [int * string] constArrayInstance prototype elems
  method elem_type = Symbol_t
  method private box (id, key) = Symbol (id, key)
end

class constTagArrayInstance ~prototype elems = object
  inherit [js_value] constArrayInstance prototype elems as super
  method elem_type = Tagged_t
  method private box v = v

  method! get_typed_element i =
    try super#get_typed_element i
    with _ ->
      (* Typed arrays raise RangeError, but tagged arrays can handle holes *)
      Undefined
end
