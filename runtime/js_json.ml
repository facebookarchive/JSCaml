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
open Js_array
open Js_rtti
open Js_builtin
open Js_syms
open Js_prop
open Js_util

let rec repeat s = function
  | n when n <= 0 -> ""
  | 1 -> s
  | n -> s ^ repeat s (n - 1)

(* JSON.stringify *)
let stringify value replacer space =
  let to_string = function
    | String s ->
      Some s
    | Number n ->
      Some (float_to_string n)
    | Object o when o#is_type tag_Number ->
      Some (float_to_string ((Obj.magic o)#get_numberData))
    | Object o when o#is_type tag_String ->
      Some ((Obj.magic o)#get_stringData)
    | _ ->
      None
  in
  (* Validate the list of keys or the replacer function. *)
  let replacer_fun, property_list = match replacer with
    | Object o when o#has_call ->
      replacer, None
    | Object o when o#is_type tag_Array ->
      let arr : js_array_instance = Obj.magic o in
      let rec get_elems = function
        | i when i = arr#length -> []
        | i ->
          match to_string (arr#get_element i) with
          | None -> get_elems (i + 1)
          | Some s -> s :: get_elems (i + 1)
      in Undefined, Some (get_elems 0)
    | Object o ->
      let length = int_of_float (!jsc_ToLength (o#prop_get sym_length)) in
      let rec get_elems = function
        | i when i = length -> []
        | i ->
          match to_string (o#prop_get (js_SymbolFor (string_of_int i))) with
          | None -> get_elems (i + 1)
          | Some s -> s :: get_elems (i + 1)
        in Undefined, Some (get_elems 0)
    | _ ->
      Undefined, None
  in
  (* Truncates a string to 10 characters. *)
  let truncate str =
    if String.length str >= 10 then String.sub str 0 10 else str
  in
  (* Find the indentation level. *)
  let gap = match space with
  | Number n ->
    repeat " " (min (int_of_float n) 10)
  | Object o when o#is_type tag_Number ->
    repeat " " (min (int_of_float (Obj.magic o)#get_numberData) 10)
  | String str ->
    truncate str
  | Object o when o#is_type tag_String ->
    truncate (Obj.magic o)#get_stringData
  | _ ->
    ""
  in
  (* Serializes a number. *)
  let json_number = function
    | n when n = infinity -> "null"
    | n when n = neg_infinity -> "null"
    | n when n <> n -> "null"
    | n -> float_to_string n
  in
  (* Serializes a boolean. *)
  let json_boolean = function
    | true -> "true"
    | false -> "false"
  in
  (* Serializes a string. *)
  let json_string str =
    "\"" ^ String.escaped str ^ "\""
  in
  (* Buffer where data is appended. *)
  let buf = Buffer.create 32 in
  let add_string str =
    Buffer.add_string buf str
  in
  let add_gap indent =
    if gap = "" then () else ignore (add_string indent)
  in
  (* Invokes replacer/toJSON *)
  let replace value holder key =
    (* Invoke toJSON, if defined. *)
    let value_toJSON =
      match value with
      | Object o ->
        let toJSON = o#prop_get sym_toJSON in
        if !jsc_IsCallable toJSON then
          !jsc_Call toJSON (Object holder) [|String key; value|]
        else
          value
      | _ -> value
    in
    (* Invoke the replacer method, if defined. *)
    match replacer_fun with
    | Undefined
    | Missing ->
      value_toJSON
    | _ ->
      !jsc_Call replacer (Object holder) [|String key; value|]
  in
  (* Set of visited values, used to detect circular references. *)
  let visited = Hashtbl.create 1 in
  let check_visited obj =
    if Hashtbl.mem visited obj then raise (!jsc_TypeException ());
    Hashtbl.add visited obj ();
  in
  (* Serializes a property of an object/element of an array *)
  let rec json_prop value indent =
    match value with
    | Undefined
    | Missing
    | Symbol _ ->
      false
    | Null -> add_string "null"; true
    | Boolean b -> add_string (json_boolean b); true
    | Number n -> add_string (json_number n); true
    | String s -> add_string (json_string s); true
    | Object o when o#is_type tag_Boolean ->
      add_string (json_boolean (Obj.magic o)#get_booleanData); true
    | Object o when o#is_type tag_Number ->
      add_string (json_number (Obj.magic o)#get_numberData); true
    | Object o when o#is_type tag_String ->
      add_string (json_string (Obj.magic o)#get_stringData); true
    | Object o when o#is_type tag_Array ->
      json_array (Obj.magic o : js_array_instance) indent; true
    | Object o when o#has_call -> false
    | Object o -> json_object o indent; true
  (* Serializes an array object. *)
  and json_array arr indent =
    check_visited (arr :> js_object_instance);
    (* Indent the contents. *)
    let indent' = indent ^ gap in
    let separator = if gap = "" then "," else (",\n" ^ indent') in
    (* Convert a single item to a string *)
    let json_item i =
      let value = arr#get_element i in
      let key = string_of_int i in
      let o = (arr :> js_object_instance) in
      if not (json_prop (replace value o key) indent') then
        add_string "null";
    in
    (* Converts & concatenates all items. *)
    let length = arr#length in
    let rec json_items = function
      | i when i >= length -> ()
      | i when i + 1 == length -> json_item i;
      | i ->
        json_item i;
        ignore (add_string separator);
        json_items (i + 1);
    (* Wrap the items in [] *)
    in match length with
      | 0 ->
        add_string "[]"
      | _ ->
        add_string "[";
        add_gap ("\n" ^ indent');
        json_items 0;
        add_gap ("\n" ^ indent);
        add_string "]";

  (* Serializes an object. *)
  and json_object o indent =
    check_visited o;
    (* Indent the contents. *)
    let indent' = indent ^ gap in
    (* Convert a single item to a string *)
    let json_item is_first key =
      let value = o#prop_get (js_SymbolFor key) in
      match replace value o key with
      | Undefined
      | Missing
      | Symbol _ ->
        is_first
      | Object o when o#has_call ->
        is_first
      | value ->
        if not is_first then add_string ",";
        add_string ("\n" ^ indent');
        (match gap with
        | "" -> add_string ("\"" ^ String.escaped key ^ "\":");
        | _ -> add_string ("\"" ^ String.escaped key ^ "\": ");
        );
        ignore (json_prop value indent');
        false
    in
    (* Traverse either the keys or the property list. *)
    add_string "{";
    let empty = match property_list with
      | None ->
        (* Iterate through the properties in order of creation. *)
        let open LinkedHashtbl in
        let rec json_items is_first = function
          | None -> is_first
          | Some e ->
            json_items (json_item is_first (js_KeyFor e.key)) e.next
        in json_items true o#get_properties.first
      | Some props ->
        (* Iterate through the specified properties. *)
        let rec json_items is_first = function
          | [] -> is_first
          | k :: ks ->
            json_items (json_item is_first k) ks
        in json_items true props
    (* Wrap the items in {} *)
    in
    if not empty then add_gap ("\n" ^ indent);
    add_string "}"
  in
  (* Start serialization with the root node. *)
  let root = ((new objectInstance Object.prototype) :> js_object_instance) in
  ignore (json_prop (replace value root "") "");
  Buffer.contents buf

type json_token =
  | JEOF
  | JLBrace
  | JRBrace
  | JLBracket
  | JRBracket
  | JColon
  | JComma
  | JLit of js_value

(* JSON.parse *)
let parse str _reviver =
  let length = String.length str in
  (* Extracts the next token from the source string. *)
  let rec next_token i =
    (* Helper to test for a keyword. *)
    let test_string keyword =
      let l = String.length keyword in
      let rec test_rec = function
        | j when j == l -> true
        | j when String.get str (i + j) = String.get keyword j ->
          test_rec (j + 1)
        | _ -> false
      in test_rec 0
    in
    (* Helper to test for whitespace *)
    let is_whitespace ch =
      match Char.code ch with
      | 0x09 -> true (* \t *)
      | 0x0A -> true (* LF *)
      | 0x0B -> true (* \v *)
      | 0x0C -> true (* \f *)
      | 0x0D -> true (* CR *)
      | 0x20 -> true (* space *)
      | _ -> false
    in
    (* Helper to test for digits *)
    let is_digit ch =
      '0' <= ch && ch <= '9'
    in
    (* Helper to parse a string. *)
    let rec parse_string buf i =
      match String.get str i with
        | '\\' ->
          Buffer.add_char buf
            (match String.get str (i + 1) with
            | 'b' -> '\b'
            | 't' -> '\t'
            | 'n' -> '\n'
            | 'v' -> Char.chr 0xB
            | 'f' -> Char.chr 0xC
            | 'r' -> '\r'
            | '"' -> '"'
            | '\'' -> '\''
            | '\\' -> '\\'
            | '/' -> '/'
            | _ -> raise (!jsc_SyntaxException ())
            );
          parse_string buf (i + 2)
        | '"' ->
          (i + 1, JLit (String (Buffer.contents buf)))
        | ch ->
          Buffer.add_char buf ch;
          parse_string buf (i + 1)
    in
    (* Helper to parse a number. *)
    let parse_number i =
      (* [0-9]*, as decimal part *)
      let rec parse_digits n i =
        try match String.get str i with
          | ch when is_digit ch ->
            let digit = float_of_int (Char.code ch - Char.code '0') in
            parse_digits (n *. 10. +. digit) (i + 1)
          | _ -> (i, n)
        with Invalid_argument _ -> (i, n)
      in
      (* [0-9]*, as fractional part *)
      let rec parse_frac f e i =
        try match String.get str i with
          | ch when is_digit ch ->
            let digit = float_of_int (Char.code ch - Char.code '0') in
            parse_frac (f +. (e *. digit)) (e *. 0.1) (i + 1)
          | _ -> (i, f)
        with Invalid_argument _ -> (i, f)
      in
      (* [+-]?[0-9]+ *)
      let parse_exp i =
        try match String.get str i with
          | '+' -> parse_digits 0.0 (i + 1)
          | '-' -> let (i, n) = parse_digits 0.0 (i + 1) in (i, ~-. n)
          | ch when is_digit ch -> parse_digits 0.0 i
          | _ -> raise (!jsc_SyntaxException ())
        with Invalid_argument _ ->
          raise (!jsc_SyntaxException ())
      in
      (* [0-9]+(e[+-]?[0-9]+)? *)
      let parse_float i =
        try match String.get str i with
          | ch when is_digit ch ->
            let (i, f) = parse_frac 0.0 0.1 i in
            begin
              try match String.get str i with
              | 'E' | 'e' -> let (i, e) = parse_exp (i + 1) in (i, f, e)
              | _ -> (i, f, 0.0)
              with Invalid_argument _ -> (i, f, 0.0)
            end
          | _ -> raise (!jsc_SyntaxException ())
        with Invalid_argument _ -> raise (!jsc_SyntaxException ())
      in
      (* Find the decimal part. *)
      let (i, n) = match String.get str i with
        | '0' -> (i + 1, 0.0)
        | _ -> parse_digits 0.0 i
      in
      (* Concatenate it with an optional fractional part. *)
      try match String.get str i with
        | '.' ->
          let (i, f, e) = parse_float (i + 1) in
          (i, (n +. f) *. (10. ** e))
        | 'E' | 'e' ->
          let (i, e) = parse_exp (i + 1) in
          (i, n *. (10. ** e))
        | _ ->
          (i, n)
      with Invalid_argument _ -> (i, n)
    in
    (* Examine the first character & extract a token. *)
    try match String.get str i with
      | ch when is_whitespace ch -> next_token (i + 1)
      | '{' -> (i + 1, JLBrace)
      | '}' -> (i + 1, JRBrace)
      | '[' -> (i + 1, JLBracket)
      | ']' -> (i + 1, JRBracket)
      | ':' -> (i + 1, JColon)
      | ',' -> (i + 1, JComma)
      | 'n' when test_string "null" -> (i + 4, JLit Null)
      | 't' when test_string "true" -> (i + 4, JLit (Boolean true))
      | 'f' when test_string "false" -> (i + 5, JLit (Boolean false))
      | '"' -> parse_string (Buffer.create 32) (i + 1)
      | '-' ->
        let (i, n) = parse_number (i + 1) in (i, JLit (Number (~-. n)))
      | ch when is_digit ch ->
        let (i, n) = parse_number i in (i, JLit (Number n))
      | _ -> raise (!jsc_SyntaxException ())
    with Invalid_argument _ -> (length, JEOF)
  in
  (* Bails out if the next token does not match *)
  let expect tk i = match next_token i with
    | (i, tk') when tk = tk' -> i
    | _ -> raise (!jsc_SyntaxException ())
  in
  (* Recursively parses the JSON string. *)
  let rec parse_json i = match next_token i with
    | (i, JLit v) -> (i, v)
    | (i, JLBrace) -> parse_object i
    | (i, JLBracket) -> parse_array i
    | _ -> raise (!jsc_SyntaxException ())
  and parse_object i =
    let obj = new objectInstance Object.prototype in
    let rec parse_elems i last_comma =
      match next_token i with
      | (i, JRBrace) when not last_comma ->
        (i, Object obj)
      | (i, JLit (String k)) ->
        let i = expect JColon i in
        let (i, v) = parse_json i in
        obj#prop_create (js_SymbolFor k) v;
        (match next_token i with
        | (i, JRBrace) -> (i, Object obj)
        | (i, JComma) -> parse_elems i true
        | _ -> raise (!jsc_SyntaxException ())
        )
      | _ -> raise (!jsc_SyntaxException ())
    in parse_elems i false
  and parse_array i =
    let arr = new tagArrayInstance Array.prototype [||] in
    let rec parse_elems i idx last_comma =
      match next_token i with
      | (i, JRBracket) when not last_comma ->
        (i, Object (arr :> js_object_instance))
      | (i, tk) ->
        let i, elem = match tk with
          | JLit v -> (i, v)
          | JLBrace -> parse_object i
          | JLBracket -> parse_array i
          | _ -> raise (!jsc_SyntaxException ())
        in
        arr#set_element idx elem;
        (match next_token i with
        | (i, JRBracket) -> (i, Object (arr :> js_object_instance))
        | (i, JComma) -> parse_elems i (idx + 1) true
        | _ -> raise (!jsc_SyntaxException ())
        )
    in parse_elems i 0 false
  in
  try
    let (i, root) = parse_json 0 in
    match next_token i with
    | _, JEOF -> root
    | _ -> raise (!jsc_SyntaxException ())
  with
    (* If parsing fails, throw a syntax error. *)
    Invalid_argument _ -> raise (!jsc_SyntaxException ())


class jsonInstance ~prototype = object
  inherit [objectPrototype] builtinInstance prototype tag_Object as super

  val m_stringify = define_function_3 "stringify"
    ret_string arg_dyn arg_dyn arg_dyn
    (fun _ value replacer space -> stringify value replacer space)
  method get_stringify = m_stringify

  val m_parse = define_function_2 "parse"
    ret_dyn arg_string arg_dyn
    (fun _ text reviver -> parse text reviver)
  method get_parse = m_parse

  method! setup_props = setup_props (super#setup_props) [
    (sym_toStringTag, new constStringProperty "JSON");
    (sym_stringify,   new constObjectProperty m_stringify);
    (sym_parse,       new constObjectProperty m_parse);
  ];
end

module JSON = struct
  let instance = new jsonInstance Object.prototype
end
