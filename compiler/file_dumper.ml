(*
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module Ty = Type

open File_util
open Utils_js
open Spider_monkey_ast

module LocMap = MyMap.Make (struct
  type t = Loc.t
  let compare = Loc.compare
end)

module TypeMap = MyMap.Make (struct
  type t = Ty.t
  let compare = Ty.reasonless_compare
end)

module TypeSet = Ty.TypeSet

module TmplMap = MyMap.Make (struct
  type t = string array
  let compare a b =
    let len = Array.length a in
    let diff = len - Array.length b in
    if diff <> 0 then
      diff
    else begin
      let rec cmp = function
        | i when i >= len -> 0
        | i ->
          let elem_a = Array.unsafe_get a i in
          let elem_b = Array.unsafe_get b i in
          let diff = String.compare elem_a elem_b in
          if diff <> 0 then diff else cmp (i + 1)
      in cmp 0
    end
end)

type target_stack_entry = {
  (* empty, try-catch or labelled statement that is still being traversed. *)
  target : Statement.t';
  (* how many finallies were encountered since the start of statement *)
  mutable finally_count : int
}

type binding =
  | ConstBinding
  | FuncBinding of Loc.t
  | UnknownBinding
  | VarBinding

type scope_kind =
  | BlockScope
  | FunctionScope
  | GlobalScope
  | WithScope of Type.annotation option

type scope_stack_entry = {
  kind : scope_kind;
  parent_scope : scope_stack_entry option;
  mutable constants : Identifier.t SMap.t;
  mutable functions : Identifier.t SMap.t;
  mutable variables : Identifier.t SMap.t;
}

module ForInOf = struct
  type left =
    | LeftDeclaration of (Loc.t * Statement.VariableDeclaration.t)
    | LeftExpression of Expression.t
end

type js_type =
  | Array_t
  | Boolean_t
  | Function_t
  | Number_t
  | Object_t
  | String_t
  | Symbol_t
  | Tagged_t
  | Void_t

exception LoopBreak

let inference = Reason.locationless_reason (Reason.RCustom "inference")
let proto_reason = Reason.locationless_reason Reason.RPrototype
let boolT = Ty.BoolT (inference, None)
let emptyT = Ty.EmptyT inference
let arrT = Ty.ArrT (inference, Ty.ArrayAT(emptyT, Some []))
let funProtoT = Ty.FunProtoT inference
let funProtoApplyT = Ty.FunProtoApplyT inference
let funProtoBindT = Ty.FunProtoBindT inference
let funProtoCallT = Ty.FunProtoCallT inference
let mixedT = Ty.MixedT (inference, Ty.Mixed_everything)
let objProtoT = Ty.ObjProtoT inference
let objT = Ty.AnyObjT inference
let nullT = Ty.NullT inference
let numT = Ty.NumT (inference, Ty.Literal (0., ""))
let stringT = Ty.StrT (inference, Ty.Literal "")

let is_builtin_cons = function
  | "Array"
  | "String"
  | "Number"
  | "Object"
  | "Boolean"
  | "Error"
  | "Symbol"
  | "Function"
  | "TypeError"
  | "RangeError"
  | "EvalError"
  | "ReferenceError"
  | "SyntaxError"
  | "URIError"
  | "RegExp"
  | "Date" -> true
  | _ -> false

let is_builtin_inst = function
  | "JSON"
  | "Math"
  | "Reflect"
  | "console" -> true
  | _ -> false

let is_builtin_name name =
  is_builtin_cons name || is_builtin_inst name

let builtin_name name =
  if is_builtin_cons name then
    ("Js_runtime." ^ name ^ ".constructor")
  else
    ("Js_runtime." ^ name ^ ".instance")

let is_global_name = function
  | "NaN"
  | "Infinity"
  | "undefined"
  | "parseInt"
  | "parseFloat"
  | "isNaN"
  | "isFinite"
  | "decodeURI"
  | "decodeURIComponent"
  | "encodeURI"
  | "encodeURIComponent"
  | "Math"
  | "console"
  | "JSON" -> true
  | _ -> false

let is_symbol = function
  | "$SymbolToPrimitive"
  | "$SymbolHasInstance"
  | "$SymbolIsConcatSpreadable"
  | "$SymbolIterator"
  | "$SymbolMatch"
  | "$SymbolReplace"
  | "$SymbolSearch"
  | "$SymbolSpecies"
  | "$SymbolSplit"
  | "$SymbolToStringTag"
  | "$SymbolUnscopables" -> true
  | _ -> false

let is_empty_array = function
  | Ty.ArrT (r, Ty.ArrayAT (Ty.EmptyT _, _)) ->
    Reason.desc_of_reason r = Reason.REmptyArrayLit
  | _ -> false

class file_dumper (context : Context.t) = object(s)

(* Dump traverses the AST for a file and produces a textual
   representation of the file expressed as OCaml source code,
   assuming the existence of a JS runtime module.

   The string is then dumped into a file created at the
   given target path. (The caller should ensure that the directory
   specified in the path already exists.)

   The source path is recorded in a comment in the generated file.
*)
  method dump
      (target_path : string)
      (source_path : string)
      (file_ast : program)
      : unit =
    s#dump_file target_path source_path file_ast

  val mutable buffer = Buffer.create 1024
  val mutable current_scope =
    {kind = GlobalScope; parent_scope = None;
      constants = SMap.empty; functions = SMap.empty; variables = SMap.empty}
  val mutable func_level = 0
  val mutable indent_count = 0
  val mutable labels = SSet.empty
  val target_stack = Stack.create()
  val mutable obj_typedef_for_name : Ty.t SMap.t = SMap.empty
  val mutable funcob_typedef_for_name : Ty.t SMap.t = SMap.empty
  val mutable object_type_name_map : string TypeMap.t = TypeMap.empty
  val mutable prototype_name_map_loc : (string * Ty.t) LocMap.t = LocMap.empty
  val mutable prototype_name_map : string TypeMap.t = TypeMap.empty
  val mutable builtin_prototype_name_map : string TypeMap.t = TypeMap.empty
  val mutable updated_prototype_map : Ty.t TypeMap.t = TypeMap.empty
  val mutable prototype_for : Ty.t TypeMap.t = TypeMap.empty
  val mutable extern_funcob_name_map : string TypeMap.t = TypeMap.empty
  val mutable funcob_type_name_map : string TypeMap.t = TypeMap.empty
  val mutable static_type_for : Ty.t TypeMap.t = TypeMap.empty
  val mutable instance_type_for_constructor : Ty.t TypeMap.t = TypeMap.empty
  (* Keeps track of which class defines the method because the FunT for a
  method refers to its this type via AnyT because it assumes the method may
  be extraced and called via an object of another type. Sadly, inside a class,
  Flow happily assumes the this value always well typed. *)
  val mutable instance_type_for_method : Ty.t TypeMap.t = TypeMap.empty
  val mutable constructor_type_for : Ty.t SMap.t = SMap.empty
  (* Keeps track of the type of the constructor method for each instance
  type. This is just a convenience since it is burdensome to grovel through
  the instance type just find its constructor. *)
  val mutable constructor_type_for_instance : Ty.t TypeMap.t = TypeMap.empty
  val mutable constructor_name_for : string SMap.t = SMap.empty
  val mutable type_for_temp : Ty.t LocMap.t = LocMap.empty
  val mutable symbols : SSet.t = SSet.empty
  val mutable tags : SSet.t = SSet.empty
  (* keeps track of the type of AnyT with reason bound this *)
  val mutable current_bound_this_t : Ty.t = mixedT
  (* keeps track of the type of the this keyword *)
  val mutable current_this_t : Ty.t = mixedT
  val mutable current_return_t : Ty.t = mixedT
  val mutable templates : (int * string list) TmplMap.t = TmplMap.empty

  method new_line str =
    Buffer.add_string buffer "\n";
    for i = 1 to indent_count do
      Buffer.add_string buffer "  "
    done;
    Buffer.add_string buffer str;

  method indent str =
    s#new_line str;
    indent_count <- indent_count + 1;

  method unindent str =
    indent_count <- indent_count - 1;
    s#new_line str;

  method add str =
    Buffer.add_string buffer str;

  method add_buffer buf =
    Buffer.add_buffer buffer buf;

  method add_escaped name =
    s#add (escape_name name)

  method get_scope_to_hoist_into () =
    let rec find_hoist_target scope =
      match scope.kind with
      | FunctionScope | GlobalScope -> scope
      | _ ->
        (match scope.parent_scope with
        | None -> failwith "Scope chain does not end in GlobalScope"
        | Some parent -> find_hoist_target parent) in
    find_hoist_target current_scope

  method symbol_name name =
    symbols <- SSet.add name symbols;
    "sym" ^ escape_name name

  method tag_name name =
    match name with
    | "Js_runtime.tag_Object" ->
      "Js_runtime.tag_Object"
    | "Js_runtime.functionPrototype" ->
      "Js_runtime.tag_FunctionPrototype"
    | "Js_runtime.functionPrototypeApply" ->
      "Js_runtime.tag_FunctionPrototypeApply"
    | "Js_runtime.functionPrototypeBind" ->
      "Js_runtime.tag_FunctionPrototypeBind"
    | "Js_runtime.functionPrototypeCall" ->
      "Js_runtime.tag_FunctionPrototypeCall"
    | _ ->
      tags <- SSet.add name tags;
      "tag_" ^ name

  method dump_file target_path source_path file_ast =
    Stack.push { target = Statement.Empty; finally_count = 0 } target_stack;
    s#hoist_toplevel_vars file_ast;
    let variable_buffer = buffer in
    buffer <- Buffer.create 1024;
    s#dump_toplevel_statements file_ast;
    let statement_buffer = buffer in
    buffer <- Buffer.create 10240;
    s#dump_toplevel_functions file_ast;
    let function_buffer = buffer in
    buffer <- Buffer.create 1024;
    s#dump_header source_path;
    let chan = open_out target_path in
    Buffer.output_buffer chan buffer;
    Buffer.output_buffer chan variable_buffer;
    Buffer.output_buffer chan function_buffer;
    Buffer.output_buffer chan statement_buffer;

  method dump_header source_path =
    s#add (spf "(* Generated from %s *)" source_path);
    s#new_line "";
    let prev_buffer = buffer in
    buffer <- Buffer.create 1024;
    s#new_line "let _this = Js_runtime.Global.instance#get_global;;";
    s#new_line "ignore(_this);;";
    s#new_line "class dummy = object end";
    s#new_line "";
    let saved_map = prototype_name_map in
    prototype_name_map <- TypeMap.empty; (* not ready to use names *)
    if not (SMap.is_empty obj_typedef_for_name) then
      s#dump_object_types ();
    if not (SMap.is_empty funcob_typedef_for_name) then
      s#dump_funcob_types ();
    if not (TypeMap.is_empty extern_funcob_name_map) then
      s#dump_extern_funcob_types ();
    if not (LocMap.is_empty prototype_name_map_loc) then
      s#dump_prototypes ();
    prototype_name_map <- saved_map;
    s#dump_hoisted_declarations SSet.empty;
    let decl_buffer = buffer in
    buffer <- prev_buffer;
    if not (SSet.is_empty symbols) then
      s#dump_symbols ();
    if not (SSet.is_empty tags) then
      s#dump_tags ();
    if not (SSet.is_empty labels) then
      s#dump_labels ();
    if not (TmplMap.is_empty templates) then
      s#dump_templates ();
    Buffer.add_buffer buffer decl_buffer;
    s#new_line "";

  method dump_labels () =
    labels |> SSet.iter
      (fun label ->
        s#new_line "exception Break"; s#add_escaped label;
        s#new_line "exception Continue"; s#add_escaped label);
    s#new_line "";

  method dump_templates () =
    templates |> TmplMap.iter
      (fun raw (id, cooked) ->
        s#new_line ("let tmpl_" ^ string_of_int id ^ " = ");
        s#add "Js_runtime.intern_template [|";
        raw |> Array.iter (fun raw ->
          s#add ("\"" ^ String.escaped raw ^ "\";")
        );
        s#add "|] [|";
        cooked |> List.iter (fun cooked ->
          s#add ("\"" ^ String.escaped cooked ^ "\";")
        );
        s#add "|]";
      );
    s#new_line "";

  method dump_symbols () =
    symbols |> SSet.iter
      (fun name ->
        s#new_line "let sym";
        s#add_escaped name;
        s#add " = ";
        s#add ("Js_runtime.js_SymbolFor \"" ^ String.escaped name ^ "\";;");
      );
    s#new_line "";

  method dump_tags () =
    tags |> SSet.iter
      (fun name ->
        s#new_line "let tag_";
        s#add name;
        s#add " = ";
        s#add ("Js_runtime.js_TagFor \"" ^ String.escaped name ^ "\";;");
      );
    s#new_line "";

  method dump_object_types () =
    let written_names = ref SSet.empty in
    obj_typedef_for_name |> SMap.iter
      (fun n t ->
        match n, t with
        | _, Ty.AnyT r when (Reason.desc_of_reason r) = Reason.RDummyThis -> ()
        | _, Ty.FunT _ -> ()
        | "Js_runtime.baseObject", _ -> ()
        | _, Ty.ObjProtoT _ -> ()
        | _, _ when TypeMap.mem t builtin_prototype_name_map -> ()
        | _  ->
          if not (SSet.mem n !written_names) then
            (s#dump_class_definition t;
            s#new_line "";
            written_names := SSet.add n !written_names))

  method dump_prototypes () =
    ignore (LocMap.fold
      (fun _ (n, t) acc ->
        if SSet.mem n acc then
          acc
        else begin
          s#dump_prototype_definition t n;
          SSet.add n acc
        end)
      prototype_name_map_loc
      SSet.empty);
    s#new_line "";

  method dump_funcob_types () =
    SMap.iter
      (fun n t ->
        s#dump_funcob_class_definition t n;
        s#new_line "";)
      funcob_typedef_for_name

  method dump_extern_funcob_types () =
    ignore (TypeMap.fold
      (fun fty name acc ->
        if SSet.mem name acc then acc
        else begin
          s#dump_extern_funcob_class_definition fty name;
          s#new_line "";
          SSet.add name acc
        end)
      extern_funcob_name_map
      SSet.empty)

  method hoist_toplevel_vars (_loc, statements, _comments) =
    statements |> List.iter (s#hoist_vars);

  method hoist_statements_vars statements =
    statements |> List.iter (s#hoist_vars);

  method hoist_vars (s_loc, statement) =
    let open Statement in
    match statement with
    | Empty -> ()
    | Block {Block.body} ->
      s#hoist_statements_vars body;
    | Expression _ -> ()
    | If {If.consequent; alternate; _} ->
      s#hoist_vars consequent;
      (match alternate with
      | Some alternate -> s#hoist_vars alternate;
      | None -> ());
    | Labeled {Labeled.body; _} ->
      s#hoist_vars body;
    | Break _ -> ()
    | Continue _ -> ()
    | With {With.body; _} ->
      s#hoist_vars body;
    | TypeAlias _ -> ()
    | Switch {Switch.cases; _} ->
      cases |> List.iter
        (fun (_, {Switch.Case.consequent; _}) ->
          s#hoist_statements_vars consequent);
    | Return _ -> ()
    | Throw _ -> ()
    | Try {Try.block = (_, {Block.body}); handler; finalizer; _} ->
      s#hoist_statements_vars body;
      (match handler with
      | None -> ()
      | Some (_, {Try.CatchClause.body = (_, {Block.body}); _}) ->
        s#hoist_statements_vars body);
      (match finalizer with
      | None -> ()
      | Some (_, {Block.body}) ->
        s#hoist_statements_vars body);
    | While {While.body; _} ->
      s#hoist_vars body;
    | DoWhile {DoWhile.body; _} ->
      s#hoist_vars body;
    | For {For.init; body; _} ->
      (match init with
      | Some (For.InitDeclaration (_,
          {VariableDeclaration.declarations; kind })) ->
        s#hoist_variable_declarations declarations kind
      | _ -> ());
      s#hoist_vars body;
    | ForIn {ForIn.left; body; _} ->
      (match left with
      | ForIn.LeftDeclaration (_, {VariableDeclaration.declarations; kind }) ->
        s#hoist_variable_declarations declarations kind
      | _ -> ());
      s#hoist_vars body;
    | ForOf {ForOf.left; body; _} ->
      (match left with
      | ForOf.LeftDeclaration (_, {VariableDeclaration.declarations; kind }) ->
        s#hoist_variable_declarations declarations kind
      | _ -> ());
      s#hoist_vars body;
    | Debugger -> ()
    | ClassDeclaration {Class.id = cid; body; _} ->
      (match cid with
      | None -> ()
      | Some (loc, name) ->
        let scope = s#get_scope_to_hoist_into() in
        scope.functions <-
          SMap.add name (loc, name) scope.functions;
        s#setup_class_maps s_loc cid body);
    | FunctionDeclaration {Function.id = fid; _} ->
      (match fid with
      | None -> failwith "function statement has no identifier"
      | Some (loc, name) ->
        let scope = s#get_scope_to_hoist_into() in
        scope.functions <-
          SMap.add name (s_loc, name) scope.functions;
        s#setup_function_maps (s#get_some_type_for loc) s_loc loc fid);
    | VariableDeclaration {VariableDeclaration.declarations; kind } ->
      s#hoist_variable_declarations declarations kind;
    | InterfaceDeclaration
        {Interface.body = (_, {Type.Object.properties; _}); _} ->
      s#setup_interface_maps s_loc properties;
    | DeclareVariable _ -> ()
    | DeclareFunction _ -> ()
    | DeclareClass _ -> ()
    | DeclareModule _ -> ()
    | DeclareModuleExports _ -> ()
    | DeclareExportDeclaration _ -> ()
    | ExportNamedDeclaration _ -> ()
    | ExportDefaultDeclaration _ -> ()
    | ImportDeclaration _ -> ()

  method setup_interface_maps s_loc props =
    let iface_t =
      match s#get_some_type_for s_loc with
      | Ty.ClassT t -> t
      | _ -> failwith "expected a ClassT for an interface declarations" in
    let interface_name = s#get_class_name_for iface_t in
    obj_typedef_for_name <-
      SMap.add interface_name iface_t obj_typedef_for_name;
    let saved_bound_this_t = current_bound_this_t in
    current_bound_this_t <- iface_t;
    let proto_t = s#get_prototype_for iface_t in
    let proto_name = s#get_class_name_for proto_t in
    obj_typedef_for_name <-
      SMap.add proto_name proto_t obj_typedef_for_name;
    s#setup_interface_method_maps props iface_t;
    current_bound_this_t <- saved_bound_this_t;

  method setup_interface_method_maps props t =
    match t with
    | Ty.InstanceT (_, _, _, _, {Ty.methods_tmap; _}) ->
      let method_types = Context.find_props context methods_tmap in
      props |> List.iter
        (fun prop ->
          match prop with
          | Type.Object.Property (s_loc,
              {Type.Object.Property._method = true; key; _}) ->
            (match key with
            | Expression.Object.Property.Identifier ((loc, name) as id) ->
              (match SMap.get name method_types with
              | Some  (Ty.Field (((Ty.FunT _) as fty), _)) ->
                s#setup_function_maps ~is_method:true fty s_loc loc (Some id);
                instance_type_for_method <-
                  TypeMap.add fty t instance_type_for_method;
              | _ -> ())
            | _ -> ());
          | _ -> ());
    | _ -> failwith "t is not an InstanceT"

  method setup_class_maps loc id_opt (_, body) =
    let this_t =
      match s#get_some_type_for loc with
      | Ty.ThisClassT t -> s#merge_possible_types t
      | Ty.ClassT _ -> failwith "ClassT not yet implemented"
      | t -> failwith ("unexpected type for class" ^
         (Debug_js.dump_t context t)) in
    let saved_bound_this_t = current_bound_this_t in
    current_bound_this_t <- this_t;
    let this_class_name =
      s#get_class_name_for this_t in
    obj_typedef_for_name <-
      SMap.add this_class_name this_t obj_typedef_for_name;
    let proto_t = s#get_prototype_for this_t in
    let proto_class_name = s#get_class_name_for proto_t in
    obj_typedef_for_name <-
      SMap.add proto_class_name proto_t obj_typedef_for_name;
    let static_t = s#get_static_type_for this_t in
    let get_fun_type params =
      let params_tlist = params |> List.map
        (fun param -> s#get_pattern_type param) in
      let rest_param = None in (* TODO *)
      let funtype = {Ty.this_t; params_tlist; params_names = None;
        rest_param; return_t = Ty.VoidT inference; closure_t = 0;
        is_predicate = false; changeset = Changeset.empty} in
      Ty.FunT (inference, static_t, proto_t, funtype) in
    let params, _ = s#get_constructor_info body in
    let fty = get_fun_type params in
    constructor_type_for_instance <-
      TypeMap.add this_t fty constructor_type_for_instance;
    constructor_type_for <-
      SMap.add proto_class_name fty constructor_type_for;
    let funcob_class_name =
      s#get_funcob_class_name_for static_t fty in
    funcob_typedef_for_name <-
      SMap.add funcob_class_name fty funcob_typedef_for_name;
    s#setup_method_maps body this_t;
    s#setup_method_maps body static_t;
    (match id_opt with
    | None -> ()
    | Some (id_loc, name) ->
      let class_name = s#get_class_name_for this_t in
      constructor_name_for <-
        SMap.add class_name name constructor_name_for;
      constructor_type_for <-
        SMap.add name fty constructor_type_for;
      Hashtbl.add (Context.type_table context) id_loc fty);
    current_bound_this_t <- saved_bound_this_t;

  method setup_method_maps body t =
    match t with
    | Ty.InstanceT (_, _, _, _, {Ty.methods_tmap; _}) ->
      let method_types = Context.find_props context methods_tmap in
      body.Class.Body.body |> List.iter
        (fun e ->
          match e with
          | Class.Body.Method (s_loc,
              {Class.Method.kind = Class.Method.Method; key; _}) ->
            (match key with
            | Expression.Object.Property.Identifier ((loc, name) as id) ->
              (match SMap.get name method_types with
              | Some  (Ty.Field (((Ty.FunT _) as fty), _)) ->
                s#setup_function_maps ~is_method:true fty s_loc loc (Some id);
                instance_type_for_method <-
                  TypeMap.add fty t instance_type_for_method;
              | _ -> ())
            | _ -> failwith "method id not an Identifier")
          | _ -> ());
    | _ -> failwith "t is not an InstanceT"

  method setup_function_maps ?(is_method=false) ft s_loc loc id_opt =
    let static_t, proto_t, this_t, _return_t =
      s#get_non_param_types_for_function (Some ft) in
    instance_type_for_constructor <-
      TypeMap.add ft this_t instance_type_for_constructor;
    let funcob_class_name =
      s#get_funcob_class_name_for ~is_method static_t ft in
    funcob_typedef_for_name <-
      SMap.add funcob_class_name ft funcob_typedef_for_name;
    let proto_class_name = s#get_class_name_for proto_t in
    obj_typedef_for_name <-
      SMap.add proto_class_name proto_t obj_typedef_for_name;
    (if s#is_unboxed_object (s#classify_type this_t) then
      let this_class_name =
        s#get_class_name_for this_t in
      obj_typedef_for_name <-
        SMap.add this_class_name this_t obj_typedef_for_name);
    (match id_opt with
    | Some (_, name) ->
      ignore (s#get_prototype_name_for loc name proto_t);
      constructor_name_for <-
        SMap.add funcob_class_name name constructor_name_for;
    | None ->
      let pcname = s#get_class_name_for proto_t in
      let pname =
        "(new " ^ pcname ^ " ~prototype:(Js_runtime.Object.prototype " ^
        ":> Js_runtime.baseObject))" in
      prototype_name_map <-
        TypeMap.add proto_t pname prototype_name_map);
    Hashtbl.replace (Context.type_table context) s_loc ft;
    constructor_type_for <-
      SMap.add funcob_class_name ft constructor_type_for;
    match id_opt with
    | Some (_, name) ->
      constructor_type_for <-
        SMap.add name ft constructor_type_for;
    | _ -> ();

  method hoist_variable_declarations declarations kind =
    if kind = Statement.VariableDeclaration.Var then
      s#add_declarations_to_scope declarations kind

  method dump_toplevel_functions (_loc, statements, _comments) =
    statements |> List.iter (s#dump_toplevel_function);

  method dump_toplevel_function statement =
    s#dump_function_statement statement;
    assert (indent_count = 0);
    assert (func_level = 0);
    assert ((Stack.length target_stack) = 1);
    assert ((Stack.top target_stack).finally_count = 0);

  method dump_functions statements =
    statements |> List.iter (s#dump_function_statement);

  method dump_function_statement (loc, statement) =
    let open Statement in
    match statement with
    | ClassDeclaration {Class.id; body; superClass; typeParameters;
        superTypeParameters; implements; classDecorators} ->
      (match id with
      | None -> failwith "class statement without id";
      | Some (_, name) ->
        s#new_line "id"; s#add_escaped name; s#add " := ");
      s#dump_class_declaration_as_funcob_instance
        loc id body superClass typeParameters
        superTypeParameters implements classDecorators;
      s#new_line "";
    | FunctionDeclaration {Function.id; params; body;
        async; generator; predicate; expression; returnType; typeParameters} ->
      (match id with
      | None -> failwith "function statement without id";
      | Some (_, name) ->
        s#new_line "id"; s#add_escaped name; s#add " := ");
      ignore (s#dump_function_declaration_as_funcob_instance
        ~is_method:false loc id params body
        async generator predicate expression returnType typeParameters);
      s#new_line "";
    | _ -> () (* dealt with later on *)

  method dump_toplevel_statements (_loc, statements, _comments) =
    s#new_line "let _ = ();;";
    s#indent "try";
    statements |> List.iter (s#dump_toplevel_statement);
    s#new_line "();";
    s#unindent "with Js_runtime.ThrownValue v -> ";
    s#add "failwith (Js_runtime.cnv_mixed_to_str v)";
    s#new_line "";

  method dump_toplevel_statement statement =
    s#dump_statement statement;
    assert (indent_count = 1);
    assert (func_level = 0);
    assert ((Stack.length target_stack) = 1);
    assert ((Stack.top target_stack).finally_count = 0);

  method dump_statements statements =
    statements |> List.iter (s#dump_statement);

  method dump_statement (loc, statement) =
    s#new_line "(* ";
    s#add (Loc.to_string loc);
    s#add " *)";
    let open Statement in
    match statement with
    | Empty ->
      s#new_line "();";
    | Block {Block.body} ->
      s#dump_block body;
    | Expression {Expression.expression; directive = _} ->
      s#dump_expression_statement expression;
    | If {If.test; consequent; alternate} ->
      s#dump_if test consequent alternate;
    | Labeled {Labeled.label; body} ->
      s#dump_labeled statement label body;
    | Break {Break.label} ->
      s#dump_break label;
    | Continue {Continue.label} ->
      s#dump_continue label;
    | With {With._object; body} ->
      s#dump_with _object body;
    | TypeAlias {TypeAlias.id; typeParameters; right} ->
      s#dump_type_alias id typeParameters right;
    | Switch {Switch.discriminant; cases} ->
      s#dump_switch statement discriminant cases;
    | Return {Return.argument} ->
      s#dump_return argument;
    | Throw {Throw.argument} ->
      s#dump_throw argument;
    | Try {Try.block; handler; finalizer } ->
      s#dump_try statement block handler finalizer;
    | While {While.test; body} ->
      s#dump_while statement test body;
    | DoWhile {DoWhile.body; test} ->
      s#dump_doWhile statement body test;
    | For {For.init; test; update; body } ->
      s#dump_for statement init test update body;
    | ForIn {ForIn.left; right; body; each} ->
      s#dump_for_in statement left right body each;
    | ForOf {ForOf.left; right; body; async = false} ->
      s#dump_for_of statement left right body;
    | ForOf {ForOf.async = true; _} ->
      (* TODO: Flow implements the async iteration proposal, which is described
       * here: https://github.com/tc39/proposal-async-iteration cc samgoldman *)
      failwith "TODO: Unsupported for-await-of statement"
    | Debugger ->
      s#new_line "(* debugger *) ();"
    | FunctionDeclaration _ -> () (* already hoisted *)
    | VariableDeclaration {VariableDeclaration.declarations; kind } ->
      s#dump_variable_declarations declarations kind;
    | ClassDeclaration _ -> () (* already hoisted *);
    | InterfaceDeclaration {Interface.id;
        typeParameters; body; extends; mixins} ->
      s#dump_interfaceDeclaration id typeParameters body extends mixins;
    | DeclareVariable {DeclareVariable.id; typeAnnotation} ->
      s#dump_declareVariable id typeAnnotation;
    | DeclareFunction {DeclareFunction.id; typeAnnotation; predicate} ->
      s#dump_declareFunction id typeAnnotation predicate;
    | DeclareClass {Interface.id; typeParameters; body; extends; mixins} ->
      s#dump_declareClass id typeParameters body extends mixins;
    | DeclareModule {DeclareModule.id; body; kind} ->
      s#dump_declareModule id body kind;
    | DeclareModuleExports typeAnnotation ->
      s#dump_declareModuleExports typeAnnotation
    | DeclareExportDeclaration {DeclareExportDeclaration.default; declaration;
        specifiers; source} ->
      s#dump_declareExportDeclaration default declaration specifiers source;
    | ExportNamedDeclaration {ExportNamedDeclaration.declaration;
        specifiers; source; exportKind} ->
      s#dump_exportNamedDeclaration declaration specifiers source exportKind;
    | ExportDefaultDeclaration {ExportDefaultDeclaration.declaration;
        exportKind} ->
      s#dump_exportDefaultDeclaration declaration exportKind;
    | ImportDeclaration {ImportDeclaration.importKind; source; specifiers} ->
      s#dump_importDeclaration importKind source specifiers;

  method dump_statement_as_block (loc, statement) =
    let open Statement in
    match statement with
    | Block {Block.body} ->
      s#dump_block body;
    | _ ->
      s#dump_block [loc, statement];

  method dump_block statements =
    let outer_scope = current_scope in
    let block_scope =
      {kind = BlockScope; parent_scope = Some outer_scope;
      constants = SMap.empty; functions = SMap.empty; variables = SMap.empty} in
    current_scope <- block_scope;
    s#indent "(";
    s#dump_functions statements;
    s#dump_statements statements;
    s#unindent ")";
    assert (current_scope == block_scope);
    current_scope <- outer_scope;

  method dump_expression_statement expression =
    s#new_line "ignore ";
    ignore (s#dump_expression expression);
    s#add ";";

  method dump_if test consequent alternate_opt =
    s#new_line "if ";
    s#dump_conversion test boolT;
    s#add " then";
    s#dump_statement_as_block consequent;
    (match alternate_opt with
    | None -> ()
    | Some alternate ->
      s#new_line "else";
      s#dump_statement_as_block alternate);
    s#add ";";

  method dump_labeled lstatement (_, label) statement =
    labels <- SSet.add label labels;
    Stack.push { target = lstatement; finally_count = 0 } target_stack;
    s#new_line "(try";
    s#dump_statement_as_block statement;
    s#new_line "with Break"; s#add_escaped label; s#add " -> ());";
    let t = Stack.pop target_stack in
    assert (t.target == lstatement);
    assert (t.finally_count = 0);

  method dump_break label_opt =
    let open Statement in
    match label_opt with
    | None ->
      s#wrap_with_finallies
        (fun target ->
          match target with
          | While _ | DoWhile _ | For _ | ForIn _ | ForOf _  | Switch _ -> true
          | _ -> false)
        (fun () -> s#add "Js_runtime.Nested_break")
        (fun () -> s#new_line "raise Js_runtime.Break;");
    | Some (_, name) ->
      s#wrap_labeled_break_continue name "raise Break";

  method dump_continue label_opt =
    let open Statement in
    match label_opt with
    | None ->
      s#wrap_with_finallies
        (fun target ->
          match target with
          | While _ | DoWhile _ | For _ | ForIn _ | ForOf _ -> true
          | _ -> false)
        (fun () -> s#add "Js_runtime.Nested_continue")
        (fun () -> s#new_line "raise Js_runtime.Continue;");
    | Some (_, name) ->
      s#wrap_labeled_break_continue name "raise Continue";

  method wrap_labeled_break_continue name raise_break_continue =
    let open Statement in
    s#wrap_with_finallies
      (fun target ->
        match target with
        | Labeled {Labeled.label = (_, label_name); _ } -> label_name = name
        | _ -> false)
      (fun () ->
        s#add "(Js_runtime.Nested_labeled_break_or_continue ";
        s#add "(fun () -> ";
        s#add raise_break_continue;
        s#add_escaped name;
        s#add ";))")
      (fun () ->
        s#new_line raise_break_continue;
        s#add_escaped name;
        s#add ";");

  method dump_with with_object statement =
    s#indent "(try";
    s#new_line "Js_runtime.push_with_object ";
    s#dump_conversion with_object mixedT;
    s#add ";";
    let outer_scope = current_scope in
    let with_scope =
      {kind = WithScope None; (* todo: look up the type *)
      parent_scope = Some current_scope;
      constants = SMap.empty; functions = SMap.empty; variables = SMap.empty} in
    current_scope <- with_scope;
    s#dump_statement_as_block statement;
    s#add ";";
    s#new_line "Js_runtime.pop_with_object ();";
    s#unindent "with e ->";
    s#indent "(";
    s#new_line "Js_runtime.pop_with_object ();";
    s#new_line "raise e;";
    s#unindent "));";
    assert (current_scope == with_scope);
    current_scope <- outer_scope;

  method dump_type_alias _id _typeParameters _right =
    ()

  method dump_switch statement ((loc, _) as discriminant) cases =
    s#indent "(try"; s#new_line "";
    let discrT = s#dump_as_temp discriminant "discriminant" in
    let discriminant_id = loc, "discriminant" in
    type_for_temp <- LocMap.add loc discrT type_for_temp;
    Stack.push { target = statement; finally_count = 0 } target_stack;
    s#dump_non_default_cases discriminant_id cases;
    s#dump_default_case cases;
    let t = Stack.pop target_stack in t.finally_count <- t.finally_count - 1;
    s#unindent "with Js_runtime.Break -> ());";

  method dump_non_default_cases ?(got_def=false) ?(got_non_def=false)
      discr_id cases =
    let open Statement.Switch.Case in
    match cases with
    | [] -> if got_non_def then s#new_line "else "
    | (_, {test = None; _})::remaining ->
      s#dump_non_default_cases ~got_def:true ~got_non_def discr_id remaining
    | (loc, {test = Some expr; consequent})::remaining ->
      if got_non_def then s#new_line "else if " else s#new_line "if ";
      let comparison = loc, Expression.Binary
        {Expression.Binary.operator = Expression.Binary.StrictEqual;
        left = (fst discr_id), Expression.Identifier discr_id;
        right = expr} in
      s#dump_conversion comparison boolT;
      s#add " then";
      s#dump_consequent consequent remaining;
      s#dump_non_default_cases ~got_def ~got_non_def:true discr_id remaining;

  method dump_default_case cases =
    let open Statement.Switch.Case in
    match cases with
    | [] -> ()
    | (_, {test = None; consequent})::remaining_cases ->
      s#dump_consequent consequent remaining_cases;
    | _::remaining_cases -> s#dump_default_case remaining_cases;

  method dump_consequent consequent _remaining_cases =
    s#dump_block consequent
    (* todo: if the consequent can fall through then
      generate code to evaluate the consequents of the remaining
      cases, stopping when a consequent will not fall through. *)

  method dump_return argument_opt =
    (match argument_opt with
    | None -> ();
    | Some argument ->
      s#new_line "__return_value := (";
      let source_type = s#dump_as_temp argument "ret_val" in
      s#dump_cnv_temp_from_to "ret_val" source_type current_return_t;
      s#add ");");
    s#wrap_with_finallies
      (fun target -> target = Statement.Empty)
      (fun () -> s#add "Js_runtime.Nested_return")
      (fun () -> s#new_line "raise Js_runtime.Return;");

  method dump_throw argument =
    s#wrap_with_finallies
      (fun target ->
        match target with
        | Statement.Try {Statement.Try.handler; _ } -> handler != None
        | _ -> false)
      (fun () ->
        s#add "(Js_runtime.Nested_thrownValue ";
        s#dump_conversion argument mixedT;
        s#add ")";)
      (fun () ->
        s#new_line "ignore (raise (Js_runtime.ThrownValue ";
        s#dump_conversion argument mixedT;
        s#add "));";);

  method wrap_with_finallies target_match with_wrapper without_wrapper =
    let finally_count_sum = ref 0 in
    (try
      target_stack |> Stack.iter
        (fun {target; finally_count} ->
          finally_count_sum := !finally_count_sum + finally_count;
          if target_match target then raise LoopBreak);
    with LoopBreak -> ());
    if !finally_count_sum == 0 then
      without_wrapper()
    else begin
      s#new_line "raise (Js_runtime.Finally ";
      for i = 2 to !finally_count_sum do
        s#add "(Js_runtime.Nested_finally ";
      done;
      with_wrapper();
      for i = 2 to !finally_count_sum do
        s#add ")";
      done;
      s#add ");";
    end;

  method dump_try statement (loc, block) handler_opt finalizer_opt =
    match finalizer_opt with
    | Some (_, finally_block) ->
      s#indent "(try";
      let t = Stack.top target_stack in t.finally_count <- t.finally_count + 1;
      s#dump_try statement (loc, block) handler_opt None;
      s#new_line "raise (Js_runtime.Finally Js_runtime.Nested_none);";
      let t = Stack.top target_stack in t.finally_count <- t.finally_count - 1;
      s#unindent "with Js_runtime.Finally (nested_exception) ->";
      s#indent "(";
      s#dump_block finally_block.Statement.Block.body;
      s#add ";";
      s#new_line "match nested_exception with";
      s#new_line "| Js_runtime.Nested_break -> raise Js_runtime.Break;";
      s#new_line "| Js_runtime.Nested_continue -> raise Js_runtime.Continue;";
      s#new_line "| Js_runtime.Nested_labeled_break_or_continue (f) -> f(); ";
      s#new_line "| Js_runtime.Nested_finally (ne) -> ";
      s#add "raise (Js_runtime.Finally ne);";
      s#new_line "| Js_runtime.Nested_none -> ();";
      s#new_line "| Js_runtime.Nested_return -> raise Js_runtime.Return;";
      s#new_line "| Js_runtime.Nested_thrownValue (value) -> ";
      s#add "raise (Js_runtime.ThrownValue value);";
      s#unindent "));";
    | None ->
      Stack.push {target = statement; finally_count = 0} target_stack;
      (match handler_opt with
      | None ->
        s#dump_block block.Statement.Block.body;
        s#add ";";
      | Some (_, handler) ->
        let pattern = handler.Statement.Try.CatchClause.param in
        s#new_line "(try";
        s#dump_block block.Statement.Block.body;
        s#new_line "with Js_runtime.ThrownValue thrown_value ->";
        s#indent "(";
        s#add_variables_to_current_scope pattern;
        s#dump_assign_temp_to_pattern ~tt:mixedT
          Statement.VariableDeclaration.Let pattern "thrown_value";
        let _, catch_block = handler.Statement.Try.CatchClause.body in
        s#dump_block catch_block.Statement.Block.body;
        s#unindent "));";
      );
      let t = Stack.pop target_stack in
      assert (t.target == statement);
      assert (t.finally_count = 0);

  method dump_while statement test body =
    s#indent "(try";
    s#indent "while ";
    s#dump_conversion test boolT;
    s#add " do";
    s#new_line "try";
    s#dump_statement_as_block body;
    s#new_line "with Js_runtime.Continue";
    s#dump_labeled_loop_continue statement;
    s#add " -> ();";
    s#unindent "done;";
    s#unindent "with Js_runtime.Break -> ());";

  method dump_doWhile statement body test =
    s#indent "(try";
    s#indent "while true do ";
    s#new_line "(try";
    s#dump_statement_as_block body;
    s#new_line "with Js_runtime.Continue";
    s#dump_labeled_loop_continue statement;
    s#add " -> ());";
    s#new_line "if not ";
    s#dump_conversion test boolT;
    s#add " then raise Js_runtime.Break;";
    s#unindent "done;";
    s#unindent "with Js_runtime.Break -> ());";

  method dump_for statement init_opt test_opt update_opt body =
    let open Statement.For in
    let outer_scope = current_scope in
    let block_scope =
      {kind = BlockScope; parent_scope = Some outer_scope;
      constants = SMap.empty; functions = SMap.empty; variables = SMap.empty} in
    current_scope <- block_scope;
    s#indent "(";
    (match init_opt with
    | None -> ();
    | Some InitDeclaration (loc, decl) ->
      s#dump_statement (loc, Statement.VariableDeclaration decl);
    | Some InitExpression expr ->
      s#new_line "ignore "; ignore (s#dump_expression expr); s#add ";");
    s#indent "try";
    s#indent "while true do ";
    (match test_opt with
    | None -> ();
    | Some test ->
      s#new_line "if not ";
      s#dump_conversion test boolT;
      s#add " then raise Js_runtime.Break;");
    s#new_line "(try";
    s#dump_statement_as_block body;
    s#new_line "with Js_runtime.Continue";
    s#dump_labeled_loop_continue statement;
    s#add " -> ());";
    (match update_opt with
    | None -> ();
    | Some update ->
      s#new_line "ignore "; ignore (s#dump_expression update); s#add ";");
    s#unindent "done;";
    s#unindent "with Js_runtime.Break -> ()";
    s#unindent ");";
    assert (current_scope == block_scope);
    current_scope <- outer_scope;

  method dump_for_in statement left right body each =
    if each then failwith "Did not expect to see 'for each' statement";
    let left =
      match left with
      | Statement.ForIn.LeftDeclaration decl -> ForInOf.LeftDeclaration decl
      | Statement.ForIn.LeftExpression expr -> ForInOf.LeftExpression expr in
    s#dump_for_in_of statement left right body "property" stringT;

  method dump_for_of statement left right body =
    let left =
      match left with
      | Statement.ForOf.LeftDeclaration decl -> ForInOf.LeftDeclaration decl
      | Statement.ForOf.LeftExpression expr -> ForInOf.LeftExpression expr in
    s#dump_for_in_of statement left right body "value" mixedT;

  method dump_for_in_of statement left right body suffix iter_ty =
    let open Statement in
    let open ForInOf in
    let outer_scope = current_scope in
    let block_scope =
      {kind = BlockScope; parent_scope = Some outer_scope;
      constants = SMap.empty; functions = SMap.empty; variables = SMap.empty} in
    current_scope <- block_scope;
    (match left with
    | LeftDeclaration (_, {VariableDeclaration.declarations; kind }) ->
      s#add_declarations_to_scope declarations kind;
    | _ -> ());
    s#indent "(";
    s#new_line "let iterator = Js_runtime.get_iter_"; s#add suffix;
    s#add " ";
    s#dump_conversion right mixedT;
    s#add " in ";
    s#indent "try";
    s#indent "while true do ";
    s#indent "(try";
    s#new_line ("match Js_runtime.step_iter_" ^ suffix ^ " iterator with ");
    s#new_line "| None -> raise Js_runtime.Break";
    s#new_line "| Some current_value ->";
    s#indent "(";
    s#dump_for_in_of_lh_assignment left iter_ty;
    s#dump_statement_as_block body;
    s#unindent ")";
    s#unindent "with Js_runtime.Continue";
    s#dump_labeled_loop_continue statement;
    s#add " -> ());";
    s#unindent "done;";
    (* todo: catch exceptions and call iterator return method *)
    s#unindent "with Js_runtime.Break -> ()";
    s#unindent ");";
    assert (current_scope == block_scope);
    current_scope <- outer_scope;

  method dump_for_in_of_lh_assignment left iter_ty =
    let open ForInOf in
    let open Statement.VariableDeclaration in
    match left with
    | LeftDeclaration
        (_, {declarations; Statement.VariableDeclaration.kind }) ->
      (match declarations with
      | [(_, {Declarator.id; init})] when init = None ->
        s#dump_assign_temp_to_pattern ~tt:iter_ty kind id "current_value";
      | _ -> failwith "Unexpected lh pattern in for in/of");
    | LeftExpression (_, Expression.Identifier (_, name)) ->
      let _, id_t, _ = s#get_binding name (Some current_scope) 0 in
      s#dump_assign_temp_to_identifier
        ~tt:iter_ty ~id_t Var name "current_value";
    | LeftExpression (_, Expression.Assignment
        {Expression.Assignment.operator; left; right})
        when operator = Expression.Assignment.Assign ->
      let lh_t = s#infer_assignment operator left right in
      s#dump_assign_temp_to_lh_with_default Var lh_t left right "current_value";
    | _ -> failwith "Unexpected for lh in/of";

  method dump_labeled_loop_continue loop =
    let open Statement in
    match (Stack.top target_stack).target with
    | Labeled { Labeled.
        label = (_, name);
        body
      } when (snd body) == loop ->
      s#add " | Continue"; s#add_escaped name;
    | _ -> ()

  method dump_function_declaration_as_funcob_instance
      ~is_method expr_loc id_opt params body async
      generator predicate expression returnType typeParameters =
    let loc, name =
      match id_opt with
      | None -> expr_loc, ""
      | Some (id_loc, name) -> id_loc, name in
    let t = s#get_some_type_for loc in
    let fty, static_t =
      match t with
      | Ty.FunT (_, statics, _, _) -> t, statics
      | _ -> failwith "funcob type has no constructor type" in
    let instance_prototype = s#get_updated_prototype_for fty in
    let props_tmap =
      match static_t with
      | Ty.ObjT (_, {Ty.props_tmap; _}) -> props_tmap
      | _ -> Ty.Properties.fake_id in
    s#add "(new ";
    s#add (s#get_funcob_class_name_for ~is_method static_t fty);
    s#add " ~length:";
    ignore (s#dump_literal
      (Literal.Number (float_of_int (List.length (fst params)))) "");
    s#add " ~name:";
    ignore (s#dump_literal (Literal.String name) "");
    s#add " ~prototype:Js_runtime.Function.prototype";
    s#add " ~instance_prototype:";
    s#dump_default_prototype_value_for instance_prototype;
    if props_tmap <> Ty.Properties.fake_id then
      s#dump_constructor_args props_tmap "";
    s#add " ~constructor:(";
    ignore (s#dump_function loc None params body async
      generator predicate expression returnType typeParameters);
    s#add "))";
    if (expr_loc = loc) then
      ()
    else if func_level = 0 then
      s#add ";;"
    else if name <> "" then
      s#add ";";
    t

  method dump_function expr_loc id_opt (params, rest) body _async
      _generator _predicate _expression _returnType _typeParameters =
    let open Function in
    (* In the case of function expressions, Flow associates the function
       type with the location of the expression *)
    let ft = s#get_type_for expr_loc in
    let outer_scope = current_scope in
    let scope =
      {kind = FunctionScope; parent_scope = Some outer_scope;
      constants = SMap.empty; functions = SMap.empty; variables = SMap.empty} in
    current_scope <- scope;
    params |> List.iteri (fun i (loc, param) ->
      (match ft with
      | Some (Ty.FunT (_, _, _, {Ty.params_tlist;_})) ->
        if i < List.length params_tlist then begin
          let t = List.nth params_tlist i in
          Hashtbl.add (Context.type_table context) loc t;
        end;
      | _ ->
        ()
      );
      (* Add missing type for parameter *)
      s#add_variables_to_current_scope (loc, param)
    );
    (match rest with
    | None -> ();
    | Some (_, {Function.RestElement.argument=((loc, _) as expr)}) ->
      (match ft with
      | Some (Ty.FunT (_, _, _, {Ty.rest_param;_})) ->
        (match rest_param with
        | None -> failwith "function type does not match function AST";
        | Some (_, _, t) ->
          Hashtbl.add (Context.type_table context) loc t);
      | _ ->
        ()
      );
      (* Add missing type for rest parameter *)
      s#add_variables_to_current_scope expr
    );
    let param_ids = SSet.of_list (SMap.keys scope.variables) in
    func_level <- func_level + 1;
    Stack.push { target = Statement.Empty; finally_count = 0 } target_stack;
    let prefix, id, ft, postfix =
      match id_opt with
      | None -> assert (expr_loc <> Loc.none); "fun ", "", ft, "->"
      | Some (loc, name) ->
        "let rec func", name,
        (if expr_loc <> Loc.none then ft
        else
          (* This is a function declaration, Flow associates the function
             type with the function id. *)
          s#get_type_for loc),
        " =" in
    let saved_bound_this_t, saved_this_t, saved_return_t =
      current_bound_this_t, current_this_t, current_return_t in
    let _, _, this_t, return_t = s#get_non_param_types_for_function ft in
    current_bound_this_t <- this_t;
    current_this_t <- this_t;
    current_return_t <- return_t;
    s#new_line prefix;
    if id <> "" then s#add_escaped id;
    s#add " _this ";
    params |> List.iteri (fun i _ -> s#add (spf "param_%n " (i+1)));
    (match rest with
    | None -> ();
    | Some _ -> s#add (spf "param_%n" (List.length params + 1));
    );
    if postfix = " =" then begin
      s#add ": ";
      s#dump_return_type ft;
    end;
    s#add postfix;
    let outer_buffer = buffer in
    let body_buffer = Buffer.create 10240 in
    buffer <- body_buffer;
    (match body with
    | BodyBlock (_, {Statement.Block.body}) ->
      s#hoist_statements_vars body;
      s#dump_func_body body ft;
    | BodyExpression expr ->
      s#add " ";
      s#dump_conversion expr return_t);
    buffer <- outer_buffer;
    indent_count <- indent_count + 1;
    s#dump_param_init 1 params rest;
    s#dump_hoisted_declarations param_ids;
    indent_count <- indent_count - 1;
    s#add_buffer body_buffer;
    assert (scope == current_scope);
    current_scope <- outer_scope;
    let t = Stack.pop target_stack in
    assert (t.target == Statement.Empty);
    assert (t.finally_count = 0);
    func_level <- func_level - 1;
    current_bound_this_t <- saved_bound_this_t;
    current_this_t <- saved_this_t;
    current_return_t <- saved_return_t;
    if id <> "" then
      if func_level = 0 then
        s#add ";;"
      else begin
        s#add "in (ignore func";
        s#add_escaped id;
        s#add ")";
        if expr_loc = Loc.none then
          (* this is a statement *)
          s#add ";";
        end;
    match ft with Some t -> t | None -> mixedT

  method dump_func_body body funcType =
    s#indent "  let __return_value = ref (";
    s#dump_default_return_value_for funcType;
    s#add ") in";
    s#new_line "(try";
    s#dump_block body;
    s#new_line "with Js_runtime.Return -> ());";
    s#new_line "!__return_value";
    s#unindent "";

  method dump_param_init i params rest =
    let open Statement.VariableDeclaration in
    match params with
    | p::ps ->
      let temp = (spf "param_%n" i) in
      let tt = s#infer_pattern_type p mixedT in
      s#dump_assign_temp_to_pattern ~tt Let p temp;
      s#dump_param_init (i+1) ps rest;
    | _ ->
      match rest with
      | None -> ()
      | Some (_, {Function.RestElement.argument=((loc, _) as p)}) ->
        let t = s#get_some_type_for loc in
        s#new_line "let js_array_rest = new Js_runtime.";
        s#add (s#get_array_type_name t);
        s#add (spf " Js_runtime.Array.prototype param_%n in" i);
        let tt = s#infer_pattern_type p mixedT in
        s#dump_assign_temp_to_pattern ~tt Let p "js_array_rest";

  method dump_hoisted_declarations param_ids =
    let is_global = current_scope.kind = GlobalScope in
    let dump_decl var_name (loc, _) =
      if not (SSet.mem var_name param_ids) then
        let t = s#get_some_type_for loc in
        s#new_line "let id";
        s#add_escaped var_name;
        (match t with
        | Ty.FunT _
        | Ty.InstanceT _
        | Ty.ObjT _ ->
          s#add " : "; s#dump_type t;
          s#add " ref = ref (Obj.magic ())";
        | _ ->
          s#add " = ref ";
          s#dump_default_value_for_some t);
        if is_global then
          s#add ";;"
        else begin
          s#add " in ignore (id";
          s#add_escaped var_name;
          s#add ");";
        end in
    SMap.iter dump_decl current_scope.variables;
    SMap.iter dump_decl current_scope.functions

  method get_non_param_types_for_function ft_opt =
    match ft_opt with
    | None -> mixedT, mixedT, mixedT, mixedT
    | Some ft ->
      let get_non_dummy this_t =
        match this_t with
        | Ty.AnyT r when (Reason.desc_of_reason r) = Reason.RDummyThis ->
          (match current_bound_this_t with
          | Ty.AnyT _ -> mixedT (* builtin function *)
          | _ -> current_bound_this_t)
        | _ ->
          s#merge_possible_types this_t in
      match ft with
      | Ty.FunT (_, statics, prototype, {Ty.return_t; this_t; _}) ->
        (s#merge_possible_types statics),
        (s#merge_possible_types prototype),
        (get_non_dummy (s#merge_possible_types this_t)),
        (s#merge_possible_types return_t)
      | Ty.CustomFunT _ -> mixedT, mixedT, mixedT, mixedT
      | Ty.IntersectionT _ ->
        (match s#merge_possible_types ft with
        | (Ty.FunT _) as mft -> s#get_non_param_types_for_function (Some mft)
        | _ -> mixedT, mixedT, mixedT, mixedT)
      | Ty.PolyT (_, t) ->
        s#get_non_param_types_for_function (Some t) (* todo: specialize? *)
      | Ty.MixedT _ ->
        mixedT, mixedT, mixedT, mixedT
      | _ ->
        failwith ("unexpected type for function " ^
          (Debug_js.dump_t context ft))

  method get_return_type_for ft_opt =
    let _funob_t, _proto_t, _this_t, return_t =
      s#get_non_param_types_for_function ft_opt in
    return_t

  method dump_default_return_value_for ft_opt =
    let rt = s#get_return_type_for ft_opt in
    s#dump_default_value_for_some rt

  method get_element_type ty =
    match ty with
    | Ty.OpenT _ ->
      s#get_element_type (s#merge_possible_types ty)
    | Ty.ArrT (_, (Ty.ArrayAT (elemt, _)
                 | Ty.TupleAT (elemt, _)
                 | Ty.ROArrayAT (elemt))) -> elemt
    | Ty.AbstractT t -> t
    | _ -> mixedT

  method dump_default_value_for t_opt =
    let t =
      match t_opt with
      | None -> Ty.AnyT (Reason.locationless_reason (Reason.RCustom "failure"))
      | Some t -> t in
    s#dump_default_value_for_some t

  method dump_default_value_for_some ?(missing=false) ?(use_null=false) ty =
    (* do this before opening Ty and thus masking TypeMap *)
    let get_builtin_proto_name pt =
      TypeMap.get pt builtin_prototype_name_map in
    let get_proto_name pt =
      TypeMap.get pt prototype_name_map in
    let js_value v =
      if missing then "Js_runtime.Missing" else v in
    let open Ty in
    match ty with
    | OpenT _ ->
      s#dump_default_value_for_some
        ~missing ~use_null (s#merge_possible_types ty);
    | NumT _ -> s#add "nan";
    | StrT _ -> s#add "\"\""
    | BoolT _ -> s#add "false"
    | Ty.AnyT r when (Reason.desc_of_reason r) = Reason.RDummyThis ->
      s#dump_default_value_for_some current_bound_this_t
    | EmptyT _
    | MixedT _
    | AnyT _  -> s#add (js_value "Js_runtime.Undefined")
    | NullT _ -> s#add (js_value "Js_runtime.Null")
    | VoidT _ -> s#add (js_value "Js_runtime.Undefined")

    | FunT _ ->
      s#add "(Obj.magic ())";
    | FunProtoT _ ->
      s#add "Js_runtime.Function.prototype";
    | FunProtoApplyT _ ->
      s#add "Js_runtime.Function.prototype#get_apply";
    | FunProtoBindT _ ->
      s#add "Js_runtime.Function.prototype#get_bind";
    | FunProtoCallT _ ->
      s#add "Js_runtime.Function.prototype#get_call";

    | ObjT (r, {props_tmap; _}) ->
      let dump_obj_literal () =
        let proto_t = s#get_prototype_for ty in
        let proto_n = get_proto_name proto_t in
        s#dump_default_object_literal ty props_tmap proto_t proto_n None in
      let dump_null_or_literal () =
        if use_null then
          s#add "(Obj.magic ())"
        else
          dump_obj_literal() in
      (match (Reason.desc_of_reason r) with
      | Reason.RPrototype ->
        (match get_builtin_proto_name ty with
        | Some n -> s#add n;
        | None -> dump_null_or_literal ());
      | _ -> dump_null_or_literal ());
    | ObjProtoT r
        when Reason.desc_of_reason r = Reason.RCustom "global object" ->
      s#add "Js_runtime.Global.instance"
    | ObjProtoT r
        when Reason.desc_of_reason r = Reason.RDummyPrototype ->
      s#add "(new Js_runtime.objectInstance Js_runtime.Object.prototype)";
    | ObjProtoT r
        when Reason.desc_of_reason r = Reason.RObjectType ->
      s#add "(new Js_runtime.objectInstance Js_runtime.Object.prototype)";
    | ObjProtoT _ ->
      s#add "Js_runtime.Object.prototype";
    | ArrT _ ->
      s#add "(new Js_runtime.";
      s#add (s#get_array_type_name ty);
      s#add " Js_runtime.Array.prototype [| |])";

    | ClassT t ->
      s#dump_default_value_for_some ~missing ~use_null t;
    | InstanceT (_, _, super, _, {fields_tmap; _ }) ->
      (match (Reason.desc_of_reason (Ty.reason_of_t ty)) with
      | Reason.RCustom "$Iterator" ->
        (* TODO: this should be strongly typed *)
        s#add "Js_runtime.Undefined";
      | Reason.RCustom "Array" ->
        s#add ("((Obj.magic ()) :> Js_runtime.js_array_instance)");
      | Reason.RCustom name when is_symbol name ->
        s#add (js_value "(Js_runtime.Symbol(0, \"\"))");
      | Reason.RCustom name when is_builtin_cons name ->
        s#add ("Js_runtime." ^ name ^ ".empty");
      | Reason.RCustom name when is_builtin_inst name ->
        s#add ("Js_runtime." ^ name ^ ".instance");
      | _ ->
        let proto_t = s#get_prototype_for ty in
        let proto_n = get_proto_name proto_t in
        s#dump_default_object_literal
          ty fields_tmap proto_t proto_n (Some super));

    | OptionalT _ ->
      s#add "Js_runtime.Undefined"
    | AbstractT t ->
      s#dump_default_value_for_some ~missing ~use_null t;

    | EvalT (t, defer_use, _) ->
      (match defer_use with
      | DestructuringT (_, selector) ->
        (match selector with
        | Elem _ ->
          s#dump_default_value_for_some
            ~missing ~use_null (s#get_element_type t)
        | ArrRest _ ->
          s#dump_default_value_for_some ~missing ~use_null t
        | _ -> s#add (js_value "Js_runtime.Undefined")
        )
      | TypeDestructorT _ ->
        s#add (js_value "Js_runtime.Undefined")
      )
    | PolyT (_, t) ->
      s#dump_default_value_for_some ~missing ~use_null t;
    | TypeAppT (t, _) ->
      s#dump_default_value_for_some ~missing ~use_null t;
    | ThisClassT t ->
      (match (Reason.desc_of_reason (Ty.reason_of_t ty)) with
      | Reason.RClassType d ->
        (match d with
        | Reason.RCustom name when is_builtin_cons name ->
          s#add ("Js_runtime." ^ name ^ ".constructor");
        | Reason.RCustom name when is_builtin_inst name ->
          s#add ("Js_runtime." ^ name ^ ".instance");
        | _ ->
          s#dump_default_value_for_some ~missing ~use_null t
        )
      | _ ->
        s#dump_default_value_for_some ~missing ~use_null t
      )
    | ThisTypeAppT (t, _, _) ->
      s#dump_default_value_for_some ~missing ~use_null t;

    | BoundT {bound; _} ->
      s#dump_default_value_for_some ~missing ~use_null bound;
    | ExistsT _ ->
      s#add (js_value "Js_runtime.Undefined")

    | ExactT (_, t) ->
      s#dump_default_value_for_some ~missing ~use_null t;

    | MaybeT _ ->
      s#add "None";

    | TaintT _ ->
      failwith "taint types not supported"

    | IntersectionT _ ->
      s#add "Js_runtime.Object.empty";

    | UnionT _ ->
      s#dump_default_value_for_some
        ~missing ~use_null (s#merge_possible_types ty);

    | AnyWithLowerBoundT _
    | AnyWithUpperBoundT _ ->
      s#add (js_value "Js_runtime.Undefined")

    | AnyObjT _ ->
      s#add "Js_runtime.Object.empty";
    | AnyFunT _ ->
      s#add "Js_runtime.Function.empty";

    | ShapeT t ->
      s#dump_default_value_for_some ~missing ~use_null t;
    | DiffT _ ->
      s#add "Js_runtime.Object.empty";

    | KeysT _ ->
      s#add "(new Js_runtime.stringArrayInstance [| |])";
    | SingletonStrT (_, str) ->
      ignore (s#dump_literal (Literal.String str) "");
    | SingletonNumT (_, (num, _)) ->
      ignore (s#dump_literal (Literal.Number num) "");
    | SingletonBoolT (_, b) ->
      ignore (s#dump_literal (Literal.Boolean b) "");

    | TypeT (_, t) ->
      s#dump_default_value_for_some ~missing ~use_null t;

    | AnnotT t ->
      s#dump_default_value_for_some ~missing ~use_null t;

    | ModuleT _ ->
      s#add (js_value "Js_runtime.Undefined")

    | ExtendsT _ ->
      s#add "Js_runtime.Object.empty";

    | ChoiceKitT _
    | CustomFunT _ ->
      let t_str = Type_printer.string_of_t context ty in
      failwith ("Unsupported type label: " ^ t_str);

    | IdxWrapper (_, _t) ->
      s#add " Js_runtime.Idx_wrapper.empty";

    | OpenPredT (_, t, _, _) ->
      s#dump_default_value_for_some ~missing ~use_null t

    | ReposT (_, t)
    | ReposUpperT (_, t) ->
      s#dump_default_value_for_some ~missing t

    | TypeMapT _ ->
      failwith "type map not supported"

  method get_property_types t =
    match t with
    | Ty.ObjT (_, {Ty.props_tmap; _}) ->
      Context.find_props context props_tmap
    | _ -> SMap.empty

  method dump_default_object_literal
      t props_tmap proto_t proto_n super_opt =
    let rec dump_super_prop_names super_opt =
      match super_opt with
      | Some Ty.ThisTypeAppT (t, _, _) ->
        (match (s#merge_possible_types t) with
        | Ty.ThisClassT (Ty.InstanceT (_, _, super, _, {Ty.fields_tmap; _ })) ->
          dump_super_prop_names (Some super);
          s#dump_constructor_args fields_tmap "";
        | _ -> ());
      | _ -> () in
    let dump_prototype_instance cn =
      if cn = (s#get_class_name_for proto_t) then
        s#add "(Js_runtime.Object.prototype :> Js_runtime.baseObject)"
      else begin
        s#add "(";
        (match proto_n with
        | Some pn ->
          s#add pn;
        | None ->
          s#dump_default_prototype_value_for proto_t);
        s#add " :> ";
        (match proto_t with
        | Ty.ObjProtoT _ -> s#add "Js_runtime.baseObject"
        | _ -> s#dump_type proto_t);
        s#add ")"
      end in
    let class_name = s#get_class_name_for t in
    s#add "(new "; s#add class_name;
    dump_super_prop_names super_opt;
    s#dump_constructor_args props_tmap class_name;
    s#add " ~prototype:";
    dump_prototype_instance class_name;
    s#add ")";

  method dump_default_prototype_value_for proto_t =
    match TypeMap.get proto_t prototype_name_map with
    | Some pn -> s#add pn;
    | None ->
      match proto_t with
      | Ty.ObjProtoT _ ->
        s#add "Js_runtime.Object.prototype"
      | _ ->
        s#dump_default_value_for_some proto_t;

  method dump_constructor_args props_tmap class_name =
    let prop_types = Context.find_props context props_tmap in
    let prop_names = SMap.keys prop_types |> List.sort String.compare in
    prop_names |> List.iter
      (fun name ->
        if s#is_local_property name class_name then
          match SMap.find name prop_types with
          | Ty.Field (t, _) ->
            s#add " ~cons_param"; s#add_escaped name; s#add ":";
            s#dump_default_value_for_some ~use_null:true t
          | _ -> ());

  method is_local_property name class_name =
    if name = "" then
      true
    else if (String.get name 0) = '.' then
      false
    else if String.length class_name >= 7 &&
        Str.first_chars class_name 7 = "funcobj" then
      name <> "length" && name <> "name" && name <> "prototype"
    else if String.length class_name >= 5 &&
        Str.first_chars class_name 5 = "proto" then
      name <> "constructor"
    else
      true

  method dump_param_types this_t params_tlist rest_param =
    (this_t :: params_tlist) |> List.iter
      (fun t -> s#dump_type t; s#add " -> ");
    match rest_param with
    | None -> ();
    | Some (_, _, t) -> s#dump_rest_param_type t; s#add " -> "

  method dump_return_type rt_opt =
    match rt_opt with
    | None -> s#add "unit"
    | Some ft ->
      match ft with
      | Ty.FunT (_, _, _, {Ty.return_t = t; _}) ->
        s#dump_type t;
      | Ty.PolyT (_, rt) ->
        (* todo: dump the type parameters *)
        s#dump_return_type (Some rt);
      | _ ->
        let t_str = Ty.string_of_ctor ft in
        failwith ("unexpected type for function id: " ^ t_str)

  method dump_function_type ty =
    match ty with
    | Ty.FunT (_, _, _, {Ty.this_t; params_tlist; rest_param; return_t; _}) ->
      s#dump_param_types this_t params_tlist rest_param;
      s#dump_type return_t;
    | _ -> failwith "type is not a FunT"

  method dump_rest_param_type ty =
    match ty with
    | Ty.OpenT _ ->
      s#dump_rest_param_type (s#merge_possible_types ty);
    | Ty.ArrT (r, arrtype) ->
      let elemt = Ty.elemt_of_arrtype r arrtype in
      s#dump_type elemt; s#add " array";
    | Ty.EmptyT _ ->
      s#add "Js_runtime.js_value array";
    | _ -> s#dump_type ty

  method dump_type ty =
    let module TyMap = TypeMap in
    let open Ty in
    match ty with
    | OpenT _ ->
      s#dump_type (s#merge_possible_types ty);
    | NumT _ -> s#add "float";
    | StrT _ -> s#add "string";
    | BoolT _ -> s#add "bool";
    | Ty.AnyT r when (Reason.desc_of_reason r) = Reason.RDummyThis ->
      (match current_bound_this_t with
      | Ty.AnyT r when (Reason.desc_of_reason r) = Reason.RDummyThis ->
        failwith "current_bound_this_t is a dummy"
      | _ ->
        s#dump_type current_bound_this_t);
    | EmptyT _
    | MixedT _
    | AnyT _
    | NullT _
    | VoidT _ -> s#add "Js_runtime.js_value";

    | Ty.FunT (_, statics, _, _) ->
      let funcob_name = s#get_funcob_class_name_for statics ty in
      if not (SMap.mem funcob_name funcob_typedef_for_name) then
        s#add (s#get_extern_funcob_type_name_for ty)
      else
        s#add funcob_name;
    | FunProtoT _ ->
      s#add "Js_runtime.functionPrototype";
    | FunProtoApplyT _ ->
      s#add "Js_runtime.functionPrototypeApply";
    | FunProtoBindT _ ->
      s#add "Js_runtime.functionPrototypeBind";
    | FunProtoCallT _ ->
      s#add "Js_runtime.functionPrototypeCall";

    | ObjT _ ->
      s#dump_object_type ty
    | ObjProtoT r
        when Reason.desc_of_reason r = Reason.RCustom "global object" ->
      s#add "Js_runtime.globalInstance"
    | ObjProtoT _ ->
      s#add "Js_runtime.baseObject";
    | ArrT (r, arrtype) ->
      let elemt = elemt_of_arrtype r arrtype in
      s#dump_type elemt; s#add " Js_runtime.js_typed_array";
    | ClassT _->
      s#add "Js_runtime.baseObject";
    | InstanceT _ ->
      (match (Reason.desc_of_reason (Ty.reason_of_t ty)) with
      | Reason.RCustom "$Iterator" ->
        s#add "Js_runtime.js_value";
      | Reason.RCustom "Array" ->
        s#add "Js_runtime.js_array_instance";
      | Reason.RCustom name when is_symbol name ->
        s#add "Js_runtime.js_value"
      | Reason.RCustom "JSON" ->
        s#add "Js_runtime.jsonInstance"
      | Reason.RCustom "URIError"
      | Reason.RCustom "TypeError"
      | Reason.RCustom "SyntaxError"
      | Reason.RCustom "ReferenceError"
      | Reason.RCustom "RangeError"
      | Reason.RCustom "EvalError" ->
        s#add "Js_runtime.nativeErrorInstance"
      | Reason.RCustom name when is_builtin_name name ->
        s#add ("Js_runtime." ^ (String.uncapitalize_ascii name) ^ "Instance");
      | _ ->
        s#dump_object_type ty);

    | OptionalT _ ->
      s#add "Js_runtime.js_value"
    | AbstractT t ->
      s#add "("; s#dump_type t; s#add " array)";

    | EvalT (t, defer_use, _) ->
      (match defer_use with
      | DestructuringT (_, selector) ->
        (match selector with
        | Elem _ -> s#dump_type (s#get_element_type t)
        | ArrRest _ -> s#dump_type t
        | _ -> s#add "Js_runtime.js_value")
      | TypeDestructorT _ ->
        s#add "Js_runtime.js_value")

    | PolyT (_, t) ->
      s#dump_type t;
    | TypeAppT (t, _) ->
      s#dump_type t;
    | ThisClassT t ->
      s#dump_type t;
    | ThisTypeAppT (t, _, _) ->
      s#dump_type t;

    | BoundT {bound; _} ->
      s#dump_type bound;
    | ExistsT _ ->
      s#add "Js_runtime.js_value"

    | ExactT (_, t) ->
      s#dump_type t;

    | MaybeT (_, t) ->
      s#dump_type t; s#add " option"

    | TaintT _ ->
      failwith "taint types not supported"

    | IntersectionT _ ->
      s#add "Js_runtime.baseObject";

    | UnionT _ ->
      s#dump_type (s#merge_possible_types ty);

    | AnyWithLowerBoundT _
    | AnyWithUpperBoundT _ ->
      s#add "Js_runtime.js_value"

    | AnyObjT _ ->
      s#add "Js_runtime.baseObject";
    | AnyFunT _ ->
      (match TyMap.get ty funcob_type_name_map with
      | Some n -> s#add n
      | None -> s#add "Js_runtime.functionInstance");

    | ShapeT _
    | DiffT _ ->
      s#add "Js_runtime.baseObject";

    | KeysT _ ->
      s#add "Js_runtime.stringArrayInstance";
    | SingletonStrT _ ->
      s#add "string";
    | SingletonNumT _ ->
      s#add "float";
    | SingletonBoolT _ ->
      s#add "bool";

    | TypeT (_, t) ->
      s#dump_type t;

    | AnnotT t ->
      s#dump_type t;

    | ModuleT _ ->
      s#add "Js_runtime.baseObject";

    | ExtendsT _ ->
      s#add "Js_runtime.baseObject";

    | ChoiceKitT _
    | CustomFunT _ ->
      let t_str = Type_printer.string_of_t context ty in
      failwith ("Unsupported type label: " ^ t_str);

    | IdxWrapper (_, t) ->
      s#dump_type t;
      s#add " Js_runtime.idx_wrapper";

    | OpenPredT (_, t, _, _) ->
      s#dump_type t;
      s#add " Js_runtime.js_value"

    | ReposT (_, t)
    | ReposUpperT (_, t) ->
      s#dump_type t

    | TypeMapT _ ->
      failwith "type map not supported"

  method dump_object_type t =
    let r = Ty.reason_of_t t in
    match t, Reason.desc_of_reason r with
    | Ty.ObjT _, Reason.RNewObject ->
      let loc = Reason.loc_of_reason r in
      let nt = s#merge_possible_types (s#get_open_expression_type loc) in
      let t =
        match nt with
        | Ty.ObjT (_, {Ty.proto_t; _}) ->
          ignore (s#get_class_name_for proto_t);
          nt
        | Ty.FunT (_, _, _, {Ty.this_t; _}) ->
          s#merge_possible_types this_t
        | _ -> t in
      s#add (s#get_class_name_for t);
    | _ ->
      match TypeMap.get t funcob_type_name_map with
      | Some n -> s#add n
      | None ->
        (match t with
        | Ty.ObjT (_, {Ty.proto_t; _}) ->
          ignore (s#get_class_name_for proto_t);
        | _ -> ());
        s#add (s#get_class_name_for t)

 method dump_js_type_comment t =
   s#new_line "(* ";
   s#add (Debug_js.dump_t context t);
   (*s#new_line (Debug_js.jstr_of_t ~depth:5 context t);*)
   s#add " *)";

  method dump_class_definition t =
    s#dump_js_type_comment t;
    match t with
    | Ty.ObjT (_, {Ty.props_tmap; _}) ->
      let class_name = s#get_class_name_for t in
      s#dump_class_definition_for_ObjT
        t class_name props_tmap None
    | Ty.InstanceT (_, _, super, _, {Ty.fields_tmap; _ }) ->
      let class_name = s#get_class_name_for t in
      s#dump_class_definition_for_InstanceT
        t class_name super fields_tmap
    | _ ->
      failwith ("not yet implemented: " ^ (Debug_js.dump_t context t))

  method construct_class_name t =
    let mt = s#merge_possible_types t in
    match mt with
    | Ty.ObjT (r, _) ->
      let signature = s#construct_type_signature mt in
      let r = match Reason.desc_of_reason r with
      | Reason.RPrototype -> "proto_" ^ signature
      | _ -> "obj_" ^ signature in
      obj_typedef_for_name <- SMap.add r mt obj_typedef_for_name;
      r
    | Ty.InstanceT (r, _, _, _, _) ->
      (match (Reason.desc_of_reason r) with
      | Reason.RStatics (Reason.RCustom name)
          when is_builtin_cons name || is_builtin_inst name ->
        "builtin_" ^ name
      | _ ->
        let signature = s#construct_type_signature mt in
        "instance_" ^ signature)
    | Ty.ThisClassT t
    | Ty.ClassT t
    | Ty.ThisTypeAppT (t, _, _) ->
      s#construct_class_name t
    | Ty.EmptyT _
    | Ty.MixedT _ ->
      "Js_runtime.baseObject"
    | Ty.FunProtoT _ ->
      "Js_runtime.functionPrototype"
    | Ty.FunProtoApplyT _ ->
      "Js_runtime.functionPrototypeApply"
    | Ty.FunProtoBindT _ ->
      "Js_runtime.functionPrototypeBind"
    | Ty.FunProtoCallT _ ->
      "Js_runtime.functionPrototypeCall"
    | Ty.ObjProtoT _ ->
      "Js_runtime.objectPrototype"
    | _ ->
      (* unexpected *)
      Ty.string_of_ctor t

  method construct_funcob_class_name ?(is_method = false) static_t fty =
    let mt = s#merge_possible_types static_t in
    match mt with
    | Ty.ObjT _
    | Ty.InstanceT _
    | Ty.EmptyT _
    | Ty.ObjProtoT _
    | Ty.MixedT _ ->
      "funcobj_" ^ (s#construct_function_object_signature mt fty)
    | Ty.ClassT t
    | Ty.ThisClassT t
    | Ty.ThisTypeAppT (t, _, _) ->
      s#construct_funcob_class_name ~is_method t fty
    | Ty.AnyFunT r ->
      (match Reason.desc_of_reason r with
      | Reason.RStatics (Reason.RConstructor)
      | Reason.RStatics Reason.RFunctionType
      | Reason.RStatics (Reason.RProperty _) when is_method ->
        "funcobj_" ^
          (s#construct_function_object_signature mt fty)
      | Reason.RAnyFunction -> "Js_runtime.builtinFunction"
      | Reason.RStatics Reason.RFunctionType
      | Reason.RStatics (Reason.RBound (Reason.RFunction _))
      | Reason.RStatics (Reason.RProperty _) ->
        s#construct_extern_funcob_type_name fty
      | d -> Reason.string_of_desc d)
    | Ty.FunT (_, static, _, _) ->
      s#construct_funcob_class_name ~is_method static fty
    | _ ->
      Ty.string_of_ctor mt

  method construct_extern_funcob_type_name fty =
    let signature = s#construct_type_signature fty in
    let n = "extern_funcob_" ^ signature in
    extern_funcob_name_map <-
      TypeMap.add fty n extern_funcob_name_map;
    n

  method construct_type_signature t  =
    let main_buffer = buffer in
    buffer <- Buffer.create 1024;
    s#add_type_signature 3 t;
    let contents = Buffer.contents buffer in
    buffer <- main_buffer;
    Digest.to_hex (Digest.string contents)

  method add_type_signature d t =
    let mt = s#merge_possible_types t in
    match mt with
    | Ty.ObjT (_, {Ty.props_tmap; proto_t; _}) ->
      s#add_object_type_signature d props_tmap proto_t None
    | Ty.InstanceT (_, _, super, _, {Ty.fields_tmap; methods_tmap; _ }) ->
      let saved_bound_this_t = current_bound_this_t in
      current_bound_this_t <- mt;
      s#add_instance_type_signature d super fields_tmap methods_tmap emptyT;
      current_bound_this_t <- saved_bound_this_t;
    | Ty.FunT _ ->
      s#add_function_type_signature d mt;
    | Ty.ThisClassT t
    | Ty.ClassT t
    | Ty.ThisTypeAppT (t, _, _) ->
      s#add_type_signature d t
    | Ty.ObjProtoT r
        when Reason.desc_of_reason r = Reason.RCustom "global object" ->
      s#add "Js_runtime.globalInstance";
    | Ty.AnyT r when (Reason.desc_of_reason r) = Reason.RDummyThis ->
      s#add_type_signature d current_bound_this_t
    | _ ->
      s#add (Ty.string_of_ctor mt)

  method add_object_type_signature d props_tmap proto_t fty_opt =
    if d > 0 then begin
      let prop_types = Context.find_props context props_tmap in
      let prop_names = SMap.keys prop_types |> List.sort String.compare in
      prop_names |> List.iter
        (fun name ->
          s#add_property_name (d-1) name (SMap.find name prop_types));
      s#add "proto_t"; s#add_type_signature (d-1) proto_t;
      (match fty_opt with
      | Some fty -> s#add "function"; s#add_type_signature (d-1) fty;
      | None -> ());
    end

  method add_property_name d name p =
    match p with
    | Ty.Field (t, _)
    | Ty.Get t | Ty.Set t | Ty.GetSet (t, _)
    | Ty.Method t ->
      s#add name;
      s#add_type_signature d t;

  method add_instance_type_signature d super fields_tmap methods_tmap fty =
    if d > 0 then begin
      let field_types = Context.find_props context fields_tmap in
      let field_names = SMap.keys field_types |> List.sort String.compare in
      let method_types = Context.find_props context methods_tmap in
      let method_names = SMap.keys method_types |> List.sort String.compare in
      field_names |> List.iter
        (fun n -> s#add_property_name (d-1) n (SMap.find n field_types));
      method_names |> List.iter
        (fun n -> s#add_property_name (d-1) n (SMap.find n method_types));
      s#add_type_signature (d-1) super;
      s#add_type_signature (d-1) fty;
    end

  method add_function_type_signature d t =
    if d > 0 then begin
      match s#merge_possible_types t with
      | Ty.FunT (_, _, _, {Ty.this_t; return_t; params_tlist; rest_param; _}) ->
        s#add "this_t"; s#add_type_signature (d-1) this_t;
        s#add "return_t";s#add_type_signature (d-1) return_t;
        params_tlist |> List.iter
          (fun t -> s#add_type_signature (d-1) t);
        (match rest_param with
        | None -> ();
        | Some (_, _, t) -> s#add_type_signature (d-1) t);
      | _ -> failwith "passed non-function to add_function_type_signature";
    end

  method construct_function_object_signature static_t fty =
    let main_buffer = buffer in
    buffer <- Buffer.create 1024;
    s#add_type_signature 2 static_t;
    s#add_function_type_signature 2 fty;
    let contents = Buffer.contents buffer in
    buffer <- main_buffer;
    Digest.to_hex (Digest.string contents)

  method get_class_name_for t =
    match TypeMap.get t object_type_name_map with
    | Some name -> name
    | None ->
      let r = s#construct_class_name t in
      object_type_name_map <- TypeMap.add t r object_type_name_map;
      r

  method get_funcob_class_name_for ?(is_method=false) static_t fty =
    match TypeMap.get fty funcob_type_name_map with
    | Some name -> name
    | None ->
      let r = s#construct_funcob_class_name ~is_method static_t fty in
      funcob_type_name_map <- TypeMap.add fty r funcob_type_name_map;
      r

  method get_extern_funcob_type_name_for fty =
    match TypeMap.get fty extern_funcob_name_map with
    | Some name -> name
    | None ->
      let r = s#construct_extern_funcob_type_name fty in
      extern_funcob_name_map <- TypeMap.add fty r extern_funcob_name_map;
      r

  method dump_rtti class_name =
    tags <- SSet.add class_name tags;
    s#new_line "";
    s#add "method! is_type tag = ";
    s#add ("tag == tag_" ^ class_name ^ " || super#is_type tag");
    s#new_line "";

  method dump_class_definition_for_ObjT
      class_t class_name props_tmap super_opt =
    s#indent "and "; s#add class_name;
    let prototype = s#get_prototype_for class_t in
    let rec dump_super_prop_names super_opt =
      match super_opt with
      | Some Ty.ThisTypeAppT (t, _, _) ->
        (match (s#merge_possible_types t) with
        | Ty.ThisClassT (Ty.InstanceT (_, _, super, _, {Ty.fields_tmap; _ })) ->
          dump_super_prop_names (Some super);
          s#dump_constructor_params fields_tmap class_name;
        | _ -> ());
      | _ -> () in
    dump_super_prop_names super_opt;
    s#dump_constructor_params props_tmap class_name;
    s#add " ~prototype = object";
    s#new_line "inherit Js_runtime.baseObject as super";
    if class_name = s#get_class_name_for prototype then
      s#dump_typed_prototype_properties objT
    else
      s#dump_typed_prototype_properties prototype;
    let rec dump_super_properties super_opt =
      match super_opt with
      | Some Ty.ThisTypeAppT (t, _, _) ->
        (match (s#merge_possible_types t) with
        | Ty.ThisClassT ((Ty.InstanceT (_, _, super, _,
            {Ty.fields_tmap; _ })) as sup_t) ->
          dump_super_properties (Some super);
          s#dump_properties fields_tmap "" sup_t;
        | t -> s#add (Ty.string_of_ctor t));
      | _ -> () in
    dump_super_properties super_opt;
    s#dump_properties props_tmap class_name class_t;
    s#dump_rtti class_name;
    s#new_line "method! setup_props = let t = super#setup_props in";
    s#dump_property_initializer props_tmap class_name class_t;
    s#new_line "t";
    s#unindent "end";

  method get_property_type name props_tmap class_t =
    let t =
      if name = "constructor"
      then TypeMap.get class_t constructor_type_for_instance
      else None
    in match t with
    | Some t -> t
    | None ->
      let prop_types = Context.find_props context props_tmap in
      s#get_type_of_property (SMap.find name prop_types)

  method dump_property_initializer props_tmap class_name class_t =
    s#indent "let t = Js_runtime.setup_props t [";
    let prop_types = Context.find_props context props_tmap in
    let prop_names = SMap.keys prop_types |> List.sort String.compare in
    prop_names |> List.iter (fun name ->
      if s#is_local_property name class_name then begin
        let pt = s#get_property_type name props_tmap class_t in
        let n = escape_name name in
        s#new_line "(";
        s#add (s#symbol_name name);
        s#add ", (object ";
        s#indent "  inherit Js_runtime.property ";
        s#add "~enumerable: true ";
        s#add "~configurable: true ";
        s#add "~writable: true ";
        s#new_line "method! has_value = true";
        s#new_line "method! get_value = ";
        s#dump_cnv_temp_from_to ("field" ^ n) pt mixedT;
        s#new_line ("method! set_value v = " ^ "field" ^ n ^ " <- ");
        s#dump_cnv_temp_from_to "v" mixedT pt;
        s#unindent "end)";
        s#add ");";
      end;
    );
    s#unindent "] in";

  method dump_typed_prototype_properties prototype =
    s#new_line "";
    s#new_line "val mutable typed_prototype : ";
    let proto_t = s#merge_possible_types prototype in
    s#dump_type proto_t;
    s#add " = prototype";
    s#new_line "method get_typed_prototype = typed_prototype";
    s#new_line "method set_typed_prototype p = typed_prototype <- p";
    s#new_line "method getPrototypeOf = ";
    s#add "Some (typed_prototype :> Js_runtime.baseObject)";
    s#new_line "method setPrototypeOf _ = ";
    s#add "raise (Js_runtime.js_TypeException ())";
    s#new_line "";

  method dump_constructor_property constructor_type =
    s#new_line "";
    s#new_line "val mutable constructor : ";
    s#dump_function_type constructor_type;
    s#add " = constructor";
    s#new_line "method get_constructor = constructor";
    s#new_line "method set_constructor c = constructor <- c";

  method dump_call_methods
     is_constructable constructor_type instance_prototype =
    match constructor_type with
    | Ty.FunT (_, _, _,
        {Ty.this_t; return_t; params_tlist; rest_param; _}) ->
      let this_t = s#merge_possible_types this_t in
      let return_t = s#merge_possible_types return_t in

      (* Typed call method. *)
      let n = List.length params_tlist in
      s#new_line "method call (this:";
      s#dump_type this_t; s#add ")";
      let dump_param i t =
        s#add (spf " (param_%d : " i);
        s#dump_type t; s#add ")" in
      params_tlist |> List.iteri dump_param;
      (match rest_param with
      | None -> ()
      | Some (_, _, t) ->
        s#add (spf " (param_%d : " n);
        s#dump_rest_param_type t; s#add ")");
      s#add " = constructor this";
      for i = 0 to n-1 do s#add (spf " param_%d" i); done;
      (match rest_param with
      | None -> ()
      | Some _ -> s#add (spf " param_%d" n));

      let dump_arg i ty =
        let ty = s#merge_possible_types ty in
        s#new_line (spf "let arg_%d = " i);
        s#dump_cnv_from_to (fun () ->
          s#add (spf "(if %d >= len" i);
          s#add (spf " then Js_runtime.Missing");
          s#add (spf " else Array.unsafe_get args %d)" i)
        ) mixedT ty;
        s#add " in";
      in

      let dump_rest_arg n ety =
        s#new_line (spf "let rest_len = max 0 (len - %d) in " n);
        s#new_line (spf "let arg_%d = " n);
        s#add "(Array.make rest_len ";
        s#dump_default_value_for_some ety; s#add ") in";
        s#indent "for i = 0 to rest_len-1 do";
        s#new_line (spf "let rest_arg = Array.unsafe_get args (%d+i) in" n);
        s#new_line (spf "Array.unsafe_set arg_%d i " n);
        s#dump_cnv_temp_from_to "rest_arg" mixedT ety;
        s#add ";";
        s#unindent "done;" in

      (* Dynamic call method. *)
      s#indent "method! dyn_call this args = ";
      if List.length params_tlist > 0 || rest_param != None then
        s#new_line "let len = Array.length args in"
      else
        s#new_line "ignore(args);";
      List.iteri dump_arg params_tlist;
      let last_arg =
        (match rest_param with
        | None -> n
        | Some (_, _, t) -> dump_rest_arg n (s#get_element_type t); n+1) in
      s#new_line "";
      s#dump_cnv_from_to (fun () ->
        s#add "(self#call ";
        s#dump_cnv_temp_from_to "this" mixedT this_t;
        for i = 0 to last_arg - 1 do s#add (spf " arg_%d" i); done;
        s#add ")";
      ) return_t mixedT;
      s#unindent "";

      if is_constructable constructor_type then begin
        (* Typed construct method *)
        s#dump_construct_method constructor_type instance_prototype;

        (* Dynamic construct method *)
        s#indent "method! dyn_construct args = ";
        if List.length params_tlist > 0 || rest_param != None then
          s#new_line "let len = Array.length args in"
        else
          s#new_line "ignore(args);";
        List.iteri dump_arg params_tlist;
        let last_arg =
          (match rest_param with
          | None -> n
          | Some (_, _, t) -> dump_rest_arg n (s#get_element_type t); n+1) in
        s#new_line "";
        s#dump_cnv_from_to (fun () ->
          s#add "(self#construct ";
          for i = 0 to last_arg - 1 do s#add (spf " arg_%d" i); done;
          s#add ")";
        ) this_t mixedT;
        s#unindent "";

      end else
        s#dump_dummy_cons_method constructor_type;

    | _ -> failwith "constructor type not a function"

  method dump_dummy_call_method fty =
    s#new_line "method call";
    match fty with
    | Ty.FunT (_, _, _, {Ty.this_t; params_tlist; rest_param; return_t; _}) ->
      let this_t = s#merge_possible_types this_t in
      s#add " (_ : "; s#dump_type this_t; s#add ")";
      params_tlist |> List.iter
        (fun t -> s#add " (_ : ";
           s#dump_type (s#merge_possible_types t);
           s#add ")");
      (match rest_param with
      | None -> ()
      | Some (_, _, t) -> s#add " (_ : "; s#dump_type t; s#add ")");
      s#add " : "; (s#dump_type (s#merge_possible_types return_t)); s#add " =";
      s#new_line "  failwith \"dummy\"";
    | _ -> s#add (Debug_js.dump_t context fty)

  method dump_dummy_cons_method fty =
    s#new_line "method construct";
    match fty with
    | Ty.FunT (_, _, _, {Ty.params_tlist; rest_param; _}) ->
      params_tlist |> List.iter
        (fun t -> s#add " (_ : ";
           s#dump_type (s#merge_possible_types t);
           s#add ")");
      (match rest_param with
      | None -> ()
      | Some (_, _, t) -> s#add " (_ : "; s#dump_type t; s#add ")");
      s#add " : Js_runtime.baseObject =";
      s#new_line "  failwith \"dummy\"";
    | _ -> s#add (Debug_js.dump_t context fty)

  method dump_construct_method constructor_type instance_prototype =
    let pget = "self#get_prototype" in
    s#new_line "method construct ";
    match constructor_type with
    | Ty.FunT (_, _, _, {Ty.this_t; params_tlist; rest_param; return_t; _}) ->
      let this_t = s#merge_possible_types this_t in
      let n = List.length params_tlist in
      params_tlist |> List.iteri
        (fun i t ->
          s#add (spf " (param_%n : " i);
          s#dump_type t; s#add ")");
      (match rest_param with
      | None -> ()
      | Some (_, _, t) ->
        s#add (spf " (param_%n : " n); s#dump_rest_param_type t; s#add ")");
      s#add " : ";
      s#dump_type this_t;
      s#add " = let this = ";
      (match this_t with
      | Ty.ObjT (_, {Ty.props_tmap; _}) ->
        s#dump_default_object_literal
          this_t props_tmap instance_prototype (Some pget) None;
      | Ty.InstanceT (_, _, super, _, {Ty.fields_tmap; _ }) ->
        s#dump_default_object_literal
          this_t fields_tmap instance_prototype
          (Some pget) (Some super);
      | Ty.FunT _ ->
         s#dump_default_value_for_some this_t;
      | Ty.ObjProtoT r
          when Reason.desc_of_reason r = Reason.RCustom "global object" ->
        s#add "Js_runtime.Global.instance"
      | Ty.ObjProtoT _ ->
        s#add "new Js_runtime.objectInstance (";
        s#add pget; s#add " :> Js_runtime.baseObject)";
      | _ ->
        (* the this value must always be an object, so AnyT and so on
        need to end up as tagged empty objects rather than as Undefined *)
        s#add "Js_runtime.Object (new Js_runtime.objectInstance (";
        s#add pget; s#add " :> Js_runtime.baseObject))");
      s#add " in let result = constructor this";
      for i = 0 to n-1 do s#add (spf " param_%n" i); done;
      (match rest_param with
      | None -> ()
      | Some _ -> s#add (spf " param_%n" n));
      s#add " in (";
      (match s#classify_type (s#merge_possible_types return_t) with
      | Array_t
      | Function_t
      | Boolean_t
      | Number_t
      | String_t
      | Symbol_t
      | Void_t ->
        s#add "ignore (result); this";
      | Tagged_t ->
        s#dump_cnv_from_to
          (fun () ->
            s#add "(Js_runtime.return_if_object_else_default result (";
            s#dump_cnv_temp_from_to "this" this_t objT;
            s#add " :> Js_runtime.baseObject))";
          )
          mixedT this_t
      | Object_t ->
        s#dump_cnv_from_to (fun () -> s#add "result") return_t this_t
      );
      s#add ")";
    | _ ->
      failwith ("constructor type not a function: " ^
        (Debug_js.dump_t context constructor_type));

  method dump_constructor_params props_tmap class_name =
    let prop_types = Context.find_props context props_tmap in
    let prop_names = SMap.keys prop_types |> List.sort String.compare in
    prop_names |> List.iter
      (fun name ->
        if s#is_local_property name class_name then
          (s#add " ~cons_param"; s#add_escaped name));

  method dump_properties props_tmap class_name class_t =
    let prop_types = Context.find_props context props_tmap in
    let prop_names = SMap.keys prop_types |> List.sort String.compare in
    prop_names |> List.iter
      (fun name ->
        if s#is_local_property name class_name then begin
          let t = s#get_property_type name props_tmap class_t in
          symbols <- SSet.add name symbols;
          s#new_line "val mutable field"; s#add_escaped name; s#add ": ";
          s#dump_type t; s#add " = cons_param"; s#add_escaped name;
          s#new_line "method get"; s#add_escaped name;
          s#add " = field"; s#add_escaped name;
          s#new_line "method set"; s#add_escaped name;
          s#add " param"; s#add_escaped name;
          s#add " = field"; s#add_escaped name;
          s#add " <- param"; s#add_escaped name;
          s#new_line ""
        end);

  method dump_extern_funcob_class_definition fty class_name =
    s#indent "and "; s#add class_name; s#add " = object";
    s#new_line "inherit Js_runtime.builtinFunction ~length:0.0 ~name:\"\"";
    s#add " ~tag:Js_runtime.tag_Function";
    s#dump_dummy_call_method fty;
    s#dump_dummy_cons_method fty;
    s#unindent "end";

  method dump_class_definition_for_InstanceT
     t class_name super fields_tmap =
   s#dump_class_definition_for_ObjT
     t class_name fields_tmap (Some super)

  method dump_funcob_class_definition t class_name =
    s#dump_js_type_comment t;
    let is_constructable constructor_type =
      let instance_prototype = s#get_prototype_for t in
      (match constructor_type with
      | Ty.FunT (_, _, _, {Ty.this_t; _}) ->
        (match s#merge_possible_types this_t with
        | Ty.ObjT (_, {Ty.proto_t; _}) ->
          let proto_t = s#merge_possible_types proto_t in
          instance_prototype = proto_t
        | Ty.AnyT r when (Reason.desc_of_reason r) = Reason.RDummyThis ->
          false
        | _ -> true)
      | _ -> false) in
    let instance_prototype = s#get_updated_prototype_for t in
    match t with
    | Ty.ObjT _->
      failwith "Should not see ObjT anymore"
    | Ty.InstanceT (_, _, _, _, {Ty.fields_tmap; methods_tmap; _ }) ->
      let saved_bound_this_t, saved_this_t =
        current_bound_this_t, current_this_t in
      current_bound_this_t <- t;
      current_this_t <- t;
      let constructor_type = TypeMap.find t constructor_type_for_instance in
      s#indent "and "; s#add class_name;
      s#add " ~length ~name ~prototype ~instance_prototype ~constructor";
      s#dump_constructor_params fields_tmap class_name;
      s#dump_constructor_params methods_tmap class_name;
      s#add " = object(self)";
      s#new_line "inherit [";
      s#dump_type instance_prototype;
      s#add "] Js_runtime.baseConstructor ";
      s#add "length name instance_prototype as super";
      s#dump_typed_prototype_properties funProtoT;
      s#dump_construct_method constructor_type instance_prototype;
      s#new_line "";
      s#dump_properties fields_tmap class_name t;
      s#dump_properties methods_tmap class_name t;
      s#dump_rtti class_name;
      s#new_line "method! setup_props = let t = super#setup_props in";
      s#dump_property_initializer fields_tmap class_name t;
      s#dump_property_initializer methods_tmap class_name t;
      s#new_line "t";
      s#unindent "end";
      current_bound_this_t <- saved_bound_this_t;
      current_this_t <- saved_this_t;
    | Ty.FunT (_, static, _, _) ->
      let saved_bound_this_t, saved_this_t =
        current_bound_this_t, current_this_t in
      (match TypeMap.get t instance_type_for_method with
      | Some inst_t ->
        current_bound_this_t <- inst_t;
        current_this_t <- inst_t;
      | None -> ());
      s#indent "and "; s#add class_name;
      s#add " ~length ~name ~prototype ~instance_prototype ~constructor";
      let props_tmap =
        match static with
        | Ty.ObjT (_, {Ty.props_tmap; _}) -> props_tmap
        | _ -> Ty.Properties.fake_id in
      if props_tmap <> Ty.Properties.fake_id then
        s#dump_constructor_params props_tmap class_name;
      s#add " = object(self)";
      s#new_line "inherit [";
      s#dump_type instance_prototype;
      s#add "] Js_runtime.baseConstructor ";
      s#add "length name instance_prototype as super";
      s#dump_typed_prototype_properties funProtoT;
      s#dump_constructor_property t;
      s#dump_call_methods is_constructable t instance_prototype;
      s#new_line "";
      if props_tmap <> Ty.Properties.fake_id then
        s#dump_properties props_tmap class_name static;
      s#dump_rtti class_name;
      if props_tmap <> Ty.Properties.fake_id then (
        s#new_line "method! setup_props = let t = super#setup_props in";
        s#dump_property_initializer props_tmap class_name static;
        s#new_line "t");
      s#unindent "end";
      current_bound_this_t <- saved_bound_this_t;
      current_this_t <- saved_this_t;
    | Ty.AnyFunT _ -> (* method *)
      failwith "Should not see AnyFunT anymore"
    | _ ->
      (* We get here because Flow has failed to provide useful types
         Until we do more local inference this is just enough to pass some
         tests. *)
      s#indent "and "; s#add class_name;
      s#add " ~length ~name ~prototype ~instance_prototype ~constructor";
      s#add " = object";
      s#new_line "inherit [";
      s#dump_type instance_prototype;
      s#add "] Js_runtime.baseConstructor ";
      s#add "length name instance_prototype as super";
      s#dump_typed_prototype_properties objProtoT;
      s#unindent "end";

  method get_updated_prototype_for t =
    match TypeMap.get t updated_prototype_map with
    | Some ut -> ut
    | None -> s#get_prototype_for t

  method get_prototype_name_for loc func_name proto_t =
    match LocMap.get loc prototype_name_map_loc with
    | Some (name, _) -> name
    | None ->
      let saved_buffer = buffer in
      buffer <- Buffer.create 1024;
      s#add "proto";
      s#add_escaped func_name;
      let pname = Buffer.contents buffer in
      buffer <- saved_buffer;
      prototype_name_map_loc <-
        LocMap.add loc (pname, proto_t) prototype_name_map_loc;
      prototype_name_map <-
        TypeMap.add proto_t ("!" ^ pname) prototype_name_map;
      pname

  method dump_prototype_definition proto_t pn =
    s#new_line "let "; s#add pn; s#add " = ref (";
    s#dump_default_value_for_some proto_t; s#add ");;";

  method get_classified_type_name ty : string =
    match s#classify_type ty with
    | Array_t -> "object"
    | Boolean_t -> "boolean"
    | Function_t -> "function"
    | Number_t -> "number"
    | Object_t -> "object"
    | String_t -> "string"
    | Symbol_t -> "symbol"
    | Tagged_t -> "tagged"
    | Void_t -> "undefined"

  method classify_type (ty : Ty.t) : js_type =
    let open Ty in
    match ty with
    | OpenT _ ->
      s#classify_type (s#merge_possible_types ty);
    | NumT _ -> Number_t
    | StrT _ -> String_t
    | BoolT _ -> Boolean_t
    | EmptyT _ -> Tagged_t
    | MixedT _ -> Tagged_t
    | Ty.AnyT r when (Reason.desc_of_reason r) = Reason.RDummyThis -> Object_t
    | AnyT _  -> Tagged_t
    | NullT _ -> Tagged_t
    | VoidT _ -> Void_t

    | FunT _
    | FunProtoT _
    | FunProtoApplyT _
    | FunProtoBindT _
    | FunProtoCallT _ ->
      Function_t

    | ObjT _->
      Object_t
    | ObjProtoT _ ->
      Object_t
    | ArrT _ ->
      Array_t

    | ClassT t ->
      s#classify_type t

    | InstanceT _ ->
      (match (Reason.desc_of_reason (Ty.reason_of_t ty)) with
      | Reason.RCustom "$Iterator" ->
        (* TODO: this should be strongly typed *)
        Tagged_t
      | Reason.RCustom name when is_symbol name ->
        Symbol_t
      | _ ->
        Object_t
      )

    | OptionalT _ ->
      Tagged_t
    | AbstractT _ ->
      Array_t

    | EvalT (t, defer_use, _) ->
      (match defer_use with
      | DestructuringT (_, selector) ->
        (match selector with
        | Elem _ ->
          s#classify_type (s#get_element_type t)
        | ArrRest _ -> s#classify_type t
        | _ -> Tagged_t)
      | TypeDestructorT _ ->
        Tagged_t)

    | PolyT (_, t) ->
      s#classify_type t;
    | TypeAppT (t, _) ->
      s#classify_type t;
    | ThisClassT t ->
      (match (Reason.desc_of_reason (Ty.reason_of_t t)) with
      | Reason.RCustom name when is_builtin_inst name ->
        Object_t
      | Reason.RCustom name when is_builtin_name name ->
        Function_t
      | _ ->
        s#classify_type t;
      )
    | ThisTypeAppT (t, _, _) ->
      s#classify_type t;

    | BoundT {bound; _} ->
      s#classify_type bound;
    | ExistsT _ ->
      Tagged_t

    | ExactT (_, t) ->
      s#classify_type t;

    | MaybeT _ ->
      Tagged_t;

    | TaintT _ ->
      failwith "taint types not supported"

    | IntersectionT _ ->
      Object_t

    | UnionT _ ->
      s#classify_type (s#merge_possible_types ty);

    | AnyWithLowerBoundT _
    | AnyWithUpperBoundT _ ->
      Tagged_t

    | AnyObjT _ ->
      Object_t
    | AnyFunT _ ->
      Function_t

    | ShapeT _
    | DiffT _ ->
      Object_t

    | KeysT _ ->
      Array_t
    | SingletonStrT _ ->
      String_t
    | SingletonNumT _ ->
      Number_t
    | SingletonBoolT _ ->
      Boolean_t

    | TypeT (_, t) ->
      s#classify_type t;

    | AnnotT t ->
      s#classify_type t;

    | ModuleT _ ->
      Object_t

    | ExtendsT _ ->
      Object_t

    | ChoiceKitT _ ->
      let t_str = Type_printer.string_of_t context ty in
      failwith ("Unsupported type label: " ^ t_str);
    | CustomFunT _ ->
      Object_t

    | IdxWrapper (_, _t) ->
      Object_t

    | OpenPredT (_, t, _, _) ->
      s#classify_type t

    | ReposT (_, t)
    | ReposUpperT (_, t) ->
      s#classify_type t

    | TypeMapT _ ->
      failwith "type map not supported"

  method is_unboxed_object t =
    match t with
    | Array_t -> true
    | Boolean_t -> false
    | Function_t -> true
    | Number_t -> false
    | Object_t -> true
    | String_t -> false
    | Symbol_t -> false
    | Tagged_t -> false
    | Void_t -> false

  method dump_variable_declarations declarations kind =
    s#add_declarations_to_scope declarations kind;
    declarations |> List.iter
      (fun declaration -> s#dump_variable_declaration declaration kind)

  method dump_variable_declaration (_, declaration) kind =
    let open Statement.VariableDeclaration in
    let lh = declaration.Declarator.id in
    let init = declaration.Declarator.init in
    s#dump_assign_initializer_to_pattern kind lh init;

  method add_declarations_to_scope declarations kind =
    declarations |> List.iter
      (fun declaration -> s#add_variables_to_scope declaration kind)

  method add_variables_to_scope (_, declaration) kind =
    let open Statement.VariableDeclaration in
    let lh = declaration.Declarator.id in
    (match kind with
    | Var ->
      s#add_variables_to_hoist_scope lh;
    | Let ->
      s#add_variables_to_current_scope lh;
    | Const ->
      s#add_constants_to_current_scope lh;);

  method add_variables_to_hoist_scope pattern =
    let scope = s#get_scope_to_hoist_into() in
    s#add_lhside_identifiers pattern
      (fun name id ->
        scope.variables <- SMap.add name id scope.variables);

  method add_variables_to_current_scope pattern =
    let scope = current_scope in
    s#add_lhside_identifiers pattern
      (fun name id ->
        scope.variables <- SMap.add name id scope.variables);

  method add_constants_to_current_scope pattern =
    let scope = current_scope in
    s#add_lhside_identifiers pattern
      (fun name id ->
        scope.constants <- SMap.add name id scope.constants);

  method add_lhside_identifiers (loc, pattern) add =
    let open Pattern in
    match pattern with
    | Object {Object.properties; _} ->
      let open Object in
      properties |> List.iter
        (fun prop ->
          match prop with
          | Property (loc, {Property.key; _}) ->
            (match key with
            | Property.Identifier (_, name as id) ->
              add name id;
            | _ ->
              failwith (spf "Found an object property that is not an Id: %s"
                (Loc.to_string ~include_source:true loc)))
          | RestProperty (_, {RestProperty.argument}) ->
            s#add_lhside_identifiers argument add);
    | Array {Array.elements; _} ->
      let open Array in
      elements |> List.iter
        (fun e_opt ->
          match e_opt with
          | None -> ()
          | Some (Element p) ->
            s#add_lhside_identifiers p add;
          | Some (RestElement (_, {RestElement.argument})) ->
            s#add_lhside_identifiers argument add);
    | Assignment {Assignment.left; _} ->
      s#add_lhside_identifiers left add;
    | Identifier { Identifier.name = (_, name); _;} ->
      add name (loc, name);
    | Expression _ ->
      failwith (spf "Found an expression lhside : %s"
      (Loc.to_string ~include_source:true loc));

  method type_lhside_id loc name =
    let _, t, _ = s#get_binding name (Some current_scope) 0 in
    Hashtbl.add (Context.type_table context) loc t;

  method type_lhside_identifiers (loc, pattern) =
    let open Pattern in
    match pattern with
    | Object {Object.properties; _} ->
      let open Object in
      properties |> List.iter
        (fun prop ->
          match prop with
          | Property (loc, {Property.key; _}) ->
            (match key with
            | Property.Identifier (loc, name) ->
              s#type_lhside_id loc name
            | _ ->
              failwith (spf "Found an object property that is not an Id: %s"
                (Loc.to_string ~include_source:true loc)))
          | RestProperty (_, {RestProperty.argument}) ->
            s#type_lhside_identifiers argument);
    | Array {Array.elements; _} ->
      let open Array in
      elements |> List.iter
        (fun e_opt ->
          match e_opt with
          | None -> ()
          | Some (Element p) ->
            s#type_lhside_identifiers p;
          | Some (RestElement (_, {RestElement.argument})) ->
            s#type_lhside_identifiers argument);
    | Assignment {Assignment.left; _} ->
      s#type_lhside_identifiers left;
    | Identifier {Identifier.name = (_, name); _} ->
      s#type_lhside_id loc name
    | Expression _ -> failwith (spf "Found an expression lhside : %s"
      (Loc.to_string ~include_source:true loc));

  method dump_assign_temp_to_identifier ~tt ~id_t kind id_name temp_name =
    let open Statement.VariableDeclaration in
    (match id_t with
    | Ty.FunT (_, statics, _, _) ->
      ignore (s#get_funcob_class_name_for statics id_t);
    | _ -> ());
    (match kind with
    | Var ->
      s#new_line "id"; s#add_escaped id_name; s#add " := ";
    | Let ->
      s#new_line "let id"; s#add_escaped id_name; s#add " = ref ";
    | Const ->
      s#new_line "let id"; s#add_escaped id_name; s#add " = ";);
    s#dump_cnv_temp_from_to temp_name tt id_t;
    (match kind with
    | Var -> s#add ";";
    | Let | Const ->
      s#add " in ignore (id"; s#add_escaped id_name; s#add ");");

  method dump_assign_temp_to_pattern ~tt kind (loc, pattern) temp_name =
    let open Pattern in
    match pattern with
    | Object {Object.properties; _} ->
      let prop_types = s#get_property_types tt in
      let open Object in
      (* this assumes that the RestProperty is always the last in the list *)
      ignore (properties |> List.fold_left
        (fun acc prop ->
          match prop with
          | Property (loc, {Property.key; _}) ->
            (match key with
            | Property.Identifier (loc, name) ->
              (* TODO: Property maps values used to be Ty.t, but are now
               * Ty.property. Properties can be fields with a given variance or
               * getters/setters. The dumper has not been properly updated to
               * support property variance. cc samgoldman *)
              (match SMap.get name prop_types with
              | Some (Ty.Field (t, _)) ->
                s#indent "(let p_temp = ("; s#add temp_name; s#add ")#get";
                s#add_escaped name; s#add " in ";
                let id_t = s#get_some_type_for loc in
                s#dump_assign_temp_to_identifier ~tt:t ~id_t kind name "p_temp";
                s#unindent ");";
              | Some _ ->
                failwith "TODO: Unsupported property"
              | None ->
                let ptemp_name = (spf "(Js_runtime.lookup_property %s %s)"
                  (s#symbol_name name) temp_name) in
                let id_t = s#get_some_type_for loc in
                s#dump_assign_temp_to_identifier
                  ~tt:mixedT ~id_t kind name ptemp_name;);
              (spf "%s" (s#symbol_name name)) :: acc
            | _ ->
              failwith (spf "Found an object property that is not an Id: %s"
                (Loc.to_string ~include_source:true loc)))
          | RestProperty (_, {RestProperty.argument}) ->
            s#new_line "let "; s#add temp_name; s#add " = ";
            s#add (spf "(Js_runtime.object_without_properties [%s] %s) in"
              (String.concat ", " acc) temp_name);
            s#dump_assign_temp_to_pattern ~tt:mixedT kind argument temp_name;
            acc)
         []);
    | Array {Array.elements; _} ->
      let et = s#get_element_type tt in
      let open Array in
      elements |> List.iteri
        (fun i e_opt ->
          match e_opt with
          | None -> ()
          | Some (Element p) ->
            s#new_line "let "; s#add temp_name; s#add "_elem = ";
            (match et with
            | Ty.MixedT _ ->
              s#add (spf "(Js_runtime.lookup_element %n %s) in" i temp_name);
            | _ ->
              s#add (spf "(%s#get_typed_element %n) in" temp_name i));
            s#dump_assign_temp_to_pattern
              ~tt:et kind p (temp_name ^ "_elem");
          | Some (RestElement (_, {RestElement.argument})) ->
            s#new_line "let s_"; s#add temp_name; s#add " = ";
            s#add (spf "(Js_runtime.array_slice_from %n %s) in" i temp_name);
            s#dump_assign_temp_to_pattern
              ~tt:mixedT kind argument (temp_name ^ "_elem"));
    | Assignment {Assignment.left; right} ->
      let lh_t = s#infer_assignment Expression.Assignment.Assign left right in
      s#new_line "let "; s#add temp_name; s#add " = if "; s#add temp_name;
      s#add " <> Js_runtime.Undefined then ";
      s#dump_cnv_temp_from_to temp_name tt lh_t;
      s#add " else ";
      s#dump_conversion right lh_t;
      s#add " in";
      s#dump_assign_temp_to_pattern ~tt:lh_t kind left temp_name;
    | Identifier { Identifier.name = (_, name); _; } ->
      let id_t = s#get_some_type_for loc in
      s#dump_assign_temp_to_identifier ~tt ~id_t kind name temp_name;
    | Expression _ -> failwith (spf "Found an expression lhside : %s"
      (Loc.to_string ~include_source:true loc));

  method dump_assign_initializer_to_pattern kind (loc, pattern) init_opt =
    let open Statement.VariableDeclaration in
    let open Pattern in
    match pattern with
    | Identifier { Identifier.name = _, name; _; } ->
      (match kind with
      | Var ->
        ();
      | Let ->
        s#new_line "let id"; s#add_escaped name; s#add " = ref ";
      | Const ->
        s#new_line "let id"; s#add_escaped name; s#add " = ");
      let vt = s#get_some_type_for loc in
      (match vt with
      | Ty.FunT (_, statics, _, _) ->
        ignore (s#get_funcob_class_name_for statics vt);
      | _ -> ());
      (match init_opt with
      | None ->
        (match kind with
        | Var -> s#add "()"; (* hoisting took care of initialization *)
        | _ -> s#dump_default_value_for (Some vt););
      | Some init ->
        (match kind with
        | Var ->
          s#new_line "id"; s#add_escaped name; s#add " := ";
        | _ -> ());
        (match vt with
        | Ty.FunT (r, _, _, _) ->
          (match Reason.desc_of_reason r with
          | Reason.RMethodCall (Some "bind") ->
            (* todo: turn init into an expression that constructs
            a function object instance with a well typed call method *)
            s#dump_conversion init vt;
          | _ ->
            s#dump_conversion init vt;);
        | Ty.InstanceT _ ->
          (match (Reason.desc_of_reason (Ty.reason_of_t vt)) with
          | Reason.RCustom name when is_builtin_name name ->
            s#add "(";
            let init_t = s#dump_as_temp init "arg1" in
            (match init_t with
            | Ty.ObjT _ ->
              (match (Reason.desc_of_reason (Ty.reason_of_t init_t)) with
              | Reason.RPrototype ->
                object_type_name_map <- TypeMap.add init_t
                  ("Js_runtime." ^ (String.uncapitalize_ascii name) ^
                    "Prototype") object_type_name_map;
                builtin_prototype_name_map <- TypeMap.add init_t
                  ("Js_runtime." ^ name ^ ".prototype")
                  builtin_prototype_name_map;
                Hashtbl.replace (Context.type_table context) loc init_t;
                s#add "arg1)";
              | _ ->
                s#dump_cnv_temp_from_to "arg1" init_t vt;
                s#add ")");
            | _ ->
              s#dump_cnv_temp_from_to "arg1" init_t vt;
              s#add ")");
          | _ ->
            s#dump_conversion init vt;);
        | _ ->
          s#dump_conversion init vt;));
      (match kind with
      | Var ->
        s#add ";";
      | Let | Const ->
        s#add " in ignore (id"; s#add_escaped name; s#add ");");
    | Assignment _ ->
      failwith (spf "Declaration lh in assignment at %s"
       (Loc.to_string ~include_source:true loc));
    | _ ->
      match init_opt with
      | None ->
        failwith (spf "Declaration with lh pattern and no initializer at %s"
          (Loc.to_string ~include_source:true loc));
      | Some init ->
        let tt = s#dump_as_temp init "temp" in
        s#dump_assign_temp_to_pattern ~tt kind (loc, pattern) "temp"

  method dump_assign_temp_to_lh_with_default kind lh_t lh default temp_name =
    s#new_line "let "; s#add temp_name;
    s#add " = if "; s#add temp_name; s#add  " = JsRuntime.Undefined then ";
    s#dump_conversion default lh_t;
    s#add " else "; s#add temp_name; s#add " in";
    s#dump_assign_temp_to_pattern ~tt:lh_t kind lh temp_name;

  method get_constructor_info body =
    let constructor_opt =
      try
        Some (body.Class.Body.body |> List.find
         (fun e ->
           match e with
           | Class.Body.Method (_,
               {Class.Method.kind = Class.Method.Constructor; _}) ->
             true
           | _ -> false))
      with Not_found -> None in
    match constructor_opt with
    | Some (Class.Body.Method (_, {Class.Method.value = (_, func); _})) ->
      let (params, _rest) = func.Function.params in
      params, (Some func.Function.body)
    | _ -> [], None

  method dump_class_declaration_as_funcob_instance
      loc id_opt (_, body) superClass _typeParameters
      _superTypeParameters _implements _classDecorators =
    let this_t =
      match s#get_some_type_for loc with
      | Ty.ThisClassT t -> t
      | Ty.ClassT _ -> failwith "ClassT not yet implemented"
      | _ -> failwith "unexpected type for class" in
    let saved_bound_this_t = current_bound_this_t in
    let saved_this_t = current_this_t in
    current_bound_this_t <- this_t;
    current_this_t <- this_t;
    let instance_proto_t = s#get_prototype_for this_t in
    let static_t = s#get_static_type_for this_t in
    let constructor_type = TypeMap.find this_t constructor_type_for_instance in
    let prop_types =
      match static_t with
      | Ty.InstanceT (_, _, _, _, {Ty.fields_tmap; _ }) ->
        Context.find_props context fields_tmap
      | _ -> failwith "funcob_t type is not an InstanceT" in
    s#add "(new ";
    s#add (TypeMap.find constructor_type funcob_type_name_map);
    let length = ref 0. in
    body.Class.Body.body |> List.iter
      (fun e ->
        match e with
        | Class.Body.Method (_,
            {Class.Method.kind = Class.Method.Constructor; _}) -> ()
        | Class.Body.Method (s_loc,
            {Class.Method.key;
             value = (_superTypeParameters,
               {Function.params; body; async; generator; predicate;
               expression; returnType; typeParameters; _});
            static = true; _}) ->
          (match key with
          | Expression.Object.Property.Identifier (_, name) ->
            s#add " ~cons_param";
            s#add_escaped name;
            s#add ":";
            ignore (s#dump_function_declaration_as_funcob_instance
              ~is_method:true s_loc (Some (s_loc, name)) params body
              async generator predicate expression returnType typeParameters);
            s#add " ";
          | _ ->
            failwith (spf "Found an object method key that is not an Id: %s"
              (Loc.to_string ~include_source:true loc)));
        | Class.Body.Property (_,
            {Class.Property.static = false; _}) ->
          length := !length +. 1.;
        | Class.Body.Property (_,
            {Class.Property.key; value; static = true; _}) ->
          (match key with
          | Expression.Object.Property.Identifier (_, name) ->
            s#add " ~cons_param";
            s#add_escaped name;
            s#add ":";
            (match value with
            | Some v ->
              ignore (s#dump_expression v);
            | None ->
              let p = SMap.find name prop_types in
              (* TODO: Property maps values used to be Ty.t, but are now
               * Ty.property. Properties can be fields with a given variance or
               * getters/setters. The dumper has not been properly updated to
               * support property variance. cc samgoldman *)
              match p with
              | Ty.Field (t, _) ->
                s#dump_default_value_for_some t
              | _ ->
                failwith "TODO: Unsupported property");
          | _ ->
            failwith (spf "Found an object property key that is not an Id: %s"
              (Loc.to_string ~include_source:true loc)));
        | _ -> ());
    s#add " ~length:";
    ignore (s#dump_literal (Literal.Number !length) "");
    let id =
      match id_opt with
      | None -> ""
      | Some (_, name) -> name in
    s#add " ~name:";
    ignore (s#dump_literal (Literal.String id) "");
    s#add " ~prototype:Js_runtime.Function.prototype";
    s#add " ~instance_prototype:";
    s#add "(new "; s#add (s#get_class_name_for instance_proto_t); s#add " ";
    s#add "~prototype:";
    (match superClass with
    | Some sce ->
      s#add "(";
      ignore (s#dump_expression sce);
      s#add "#get_prototype) ";
    | None ->
      s#add "(Js_runtime.Object.prototype :> Js_runtime.baseObject) ");
    body.Class.Body.body |> List.iter
      (fun e ->
        match e with
        | Class.Body.Method (_,
            {Class.Method.kind = Class.Method.Constructor; _}) -> ()
        | Class.Body.Method (s_loc,
            {Class.Method.key;
             value = (_superTypeParameters,
               {Function.params; body; async; generator; predicate;
               expression; returnType; typeParameters; _});
            static = false; _}) ->
          (match key with
          | Expression.Object.Property.Identifier (_, name) ->
            s#add "~cons_param";
            s#add_escaped name;
            s#add ":";
            ignore (s#dump_function_declaration_as_funcob_instance
              ~is_method:true s_loc (Some (s_loc, name)) params body
              async generator predicate expression returnType typeParameters);
            s#add " ";
          | _ ->
            failwith (spf "Found an object method key that is not an Id: %s"
              (Loc.to_string ~include_source:true loc)));
        | _ -> ());
    s#add ") ~constructor:(";
    s#dump_class_declaration_as_constructor_function loc body superClass;
    s#add "));";
    if func_level = 0 then s#add ";";
    current_bound_this_t <- saved_bound_this_t;
    current_this_t <- saved_this_t

  method dump_class_declaration_as_constructor_function loc body superClass =
    let this_t =
      match s#get_some_type_for loc with
      | Ty.ThisClassT t -> t
      | Ty.ClassT _ -> failwith "ClassT not yet implemented"
      | _ -> failwith "unexpected type for class" in
    let static_t = s#get_static_type_for this_t in
    let constructor_type = TypeMap.find this_t constructor_type_for_instance in
    let prop_types =
      match static_t with
      | Ty.InstanceT (_, _, _, _, {Ty.fields_tmap; _ }) ->
        Context.find_props context fields_tmap
      | _ -> failwith "static_t type is not an InstanceT" in
    let outer_scope = current_scope in
    let scope =
      {kind = FunctionScope; parent_scope = Some outer_scope;
      constants = SMap.empty; functions = SMap.empty; variables = SMap.empty} in
    (match superClass with
    | Some ((loc, _) as sce) ->
      (match s#get_expression_type sce with
      | Ty.ThisClassT instance_t ->
        let id = loc, "super" in
        type_for_temp <- LocMap.add loc instance_t type_for_temp;
        outer_scope.functions <- SMap.add "super" id outer_scope.functions;
      | _ -> ())
    | None -> ());
    let saved_bound_this_t, saved_this_t, saved_return_t =
      current_bound_this_t, current_this_t, current_return_t in
    let _, _, this_t, return_t =
      s#get_non_param_types_for_function (Some constructor_type) in
    current_bound_this_t <- this_t;
    current_this_t <- this_t;
    current_return_t <- return_t;
    current_scope <- scope;
    let params, cons_body_opt = s#get_constructor_info body in
    params |> List.iter (fun param -> s#add_variables_to_current_scope param);
    let param_ids = SSet.of_list (SMap.keys scope.variables) in
    func_level <- func_level + 1;
    Stack.push { target = Statement.Empty; finally_count = 0 } target_stack;
    s#new_line "fun ";
    s#add " (_this : ";
    s#add (s#get_class_name_for this_t);
    s#add ") ";
    params |> List.iteri (fun i _ -> s#add (spf "param_%n " (i+1)));
    s#add "->";
    let outer_buffer = buffer in
    let body_buffer = Buffer.create 10240 in
    buffer <- body_buffer;
    (match cons_body_opt with
    | None -> s#new_line "  ()"; s#new_line "";
    | Some body ->
      (match body with
      | Function.BodyBlock (_, {Statement.Block.body}) ->
        s#hoist_statements_vars body;
        s#dump_func_body body None;
      | Function.BodyExpression expr ->
        s#add " ";
        ignore (s#dump_expression expr)));
    buffer <- outer_buffer;
    indent_count <- indent_count + 1;
    body.Class.Body.body |> List.iter
      (fun e ->
        match e with
        | Class.Body.Property (_,
            {Class.Property.key; value = Some v; static = false; _}) ->
          (match key with
          | Expression.Object.Property.Identifier (_, name) ->
            s#new_line "_this#set";
            s#add_escaped name;
            s#add " ";
            let prop_type =
              match SMap.get name prop_types with
              | Some (Ty.Field (t, _)) -> t
              | _ -> mixedT in
            s#dump_conversion v prop_type;
            s#add ";";
          | _ ->
            failwith (spf "Found an object property key that is not an Id: %s"
              (Loc.to_string ~include_source:true loc)));
        | _ -> ());
    s#dump_param_init 1 params None;
    s#dump_hoisted_declarations param_ids;
    indent_count <- indent_count - 1;
    s#add_buffer body_buffer;
    assert (scope == current_scope);
    current_scope <- outer_scope;
    let t = Stack.pop target_stack in
    assert (t.target == Statement.Empty);
    assert (t.finally_count = 0);
    func_level <- func_level - 1;
    current_bound_this_t <- saved_bound_this_t;
    current_this_t <- saved_this_t;
    current_return_t <- saved_return_t;

  method dump_interfaceDeclaration _id _typeParameters _body _extends _mixins =
    ()

  method dump_declareVariable _id _typeAnnotation =
    ()

  method dump_declareFunction _id _typeAnnotation _predicate =
    ()

  method dump_declareClass _id _typeParameters _body _extends _mixins =
    ()

  method dump_declareModule _id _body _kind =
    ()

  method dump_declareModuleExports _typeAnnotations =
    ()

  method dump_declareExportDeclaration
       _default _declaration _specifiers _source =
    ()

  method dump_exportNamedDeclaration
      _declaration _specifiers _source _exportKind =
    ()

  method dump_exportDefaultDeclaration _declaration _exportKind =
    ()

  method dump_importDeclaration _importKind _source _specifiers =
    ()

  method dump_conversion_to_int e =
    let dump_as_runtime_call e =
      s#add "(int_of_float ";
      s#dump_conversion e numT;
      s#add ")" in
    match e with
    | (_, Expression.Literal {Literal.value; raw }) ->
      (match value with
      | Literal.Number n when (float_of_int (int_of_float n)) = n ->
        s#add raw;
      | _ -> dump_as_runtime_call e);
    | _ -> dump_as_runtime_call e;

  method dump_conversion_to_int32 e =
    let dump_as_runtime_call e =
      s#add "(Int32.of_float ";
      s#dump_conversion e numT;
      s#add ")" in
    match e with
    | (_, Expression.Literal {Literal.value; raw }) ->
      (match value with
      | Literal.Number n when (Int32.to_float (Int32.of_float n)) = n ->
        s#add raw;
        s#add "l"
      | _ -> dump_as_runtime_call e);
    | _ -> dump_as_runtime_call e;

  method dump_primitive_conversion_to_string temp_name temp_t =
    match s#merge_possible_types temp_t with
    | Ty.NullT _ ->
      ignore (s#dump_literal (Literal.String "null") "");
    | Ty.NumT (r, (Ty.Literal (_, raw)))
        when r == inference && raw <> "" ->
      ignore (s#dump_literal (Literal.String raw) "");
    | _ ->
      s#add "(Js_runtime.to_primitive_string ";
      s#add "(Js_runtime.to_primitive ";
      s#dump_cnv_temp_from_to temp_name temp_t mixedT;
      s#add "))";

  method dump_as_temp ?(hint=None) e temp_name =
    s#add "let "; s#add temp_name; s#add " = ";
    let t = s#dump_expression ~hint:hint e in
    s#add " in ";
    t

  method dump_conversion e target_type =
    match e, (s#merge_possible_types target_type) with
    | (_, Expression.Literal {Literal.value = Literal.Number _; raw }),
        Ty.StrT _ ->
      ignore (s#dump_literal (Literal.String raw) "");
    | (_, Expression.Literal {Literal.value = Literal.Null; _ }), Ty.StrT _ ->
      ignore (s#dump_literal (Literal.String "null") "");
    | _, tt ->
      s#add "(";
      let source_type = s#dump_as_temp ~hint:(Some tt) e "arg1" in
      s#dump_cnv_temp_from_to "arg1" source_type tt;
      s#add ")";

  method dump_cnv_temp_from_to temp_name source_type target_type =
    s#dump_cnv_from_to (fun () -> s#add temp_name) source_type target_type

  method classify_element_type ty = match ty with
    | Ty.OpenT _ -> s#classify_element_type (s#merge_possible_types ty)
    | (Ty.ArrT (_, (Ty.ArrayAT (elemt, _)
                  | Ty.TupleAT (elemt, _)
                  | Ty.ROArrayAT (elemt)))) ->
        s#classify_type elemt
    | Ty.AbstractT t -> s#classify_type t
    | _ -> Tagged_t

  method dump_cnv_from_to f source_type target_type =
    match source_type, target_type with
    | Ty.AnyT r, _ when (Reason.desc_of_reason r) = Reason.RDummyThis ->
      (match current_bound_this_t with
      | Ty.AnyT r when (Reason.desc_of_reason r) = Reason.RDummyThis ->
        failwith "current_bound_this_t is a dummy"
      | _ ->
        s#dump_cnv_from_to f current_bound_this_t target_type)
    | _, Ty.AnyT r when (Reason.desc_of_reason r) = Reason.RDummyThis ->
      (match current_bound_this_t with
      | Ty.AnyT r when (Reason.desc_of_reason r) = Reason.RDummyThis ->
        failwith "current_bound_this_t is a dummy"
      | _ ->
        s#dump_cnv_from_to f source_type current_bound_this_t)
    | Ty.MaybeT (_, t1), Ty.MaybeT (_, t2) ->
      s#add "(match "; f(); s#add " with None -> None ";
      s#add "| Some s -> Some ";
      s#dump_cnv_temp_from_to "s" t1 t2;
      s#add ")";
    | Ty.MaybeT (_, t1), _ ->
      let mf () =
        s#add "(match "; f(); s#add " with None -> Js_runtime.Null ";
        s#add "| Some s -> ";
        s#dump_cnv_temp_from_to "s" t1 mixedT;
        s#add ")" in
      s#dump_cnv_from_to mf mixedT target_type
    | _, Ty.MaybeT (_, t2) ->
      s#add "(";
      (match s#classify_type source_type with
      | Tagged_t ->
        s#add "match "; f(); s#add " with ";
        s#add "Js_runtime.Undefined | Js_runtime.Null | Js_runtime.Missing -> ";
        s#add "None | v -> Some ";
        s#dump_cnv_temp_from_to "v" mixedT t2
      | _ ->
        s#add "Some ";
        s#dump_cnv_from_to f source_type t2);
      s#add ")";
    | _ ->
      s#dump_cnv_from_to_no_maybe f source_type target_type

  method dump_cnv_from_to_no_maybe f source_type target_type =
    let prefix = function
      | Array_t -> "object"
      | Boolean_t -> "bool"
      | Function_t -> "object"
      | Number_t -> "number"
      | Object_t -> "object"
      | String_t -> "string"
      | Symbol_t -> "symbol"
      | Tagged_t -> "tag"
      | Void_t -> "tag" in

    match (s#classify_type source_type), (s#classify_type target_type) with
    | Array_t, Array_t ->
      (match source_type, target_type with
      | src, dst when is_empty_array src && is_empty_array dst ->
        f ();
      | src, Ty.ArrT (_, _) when is_empty_array src ->
        (* Since the type of the empty array is "unknown", a tag array is
           generated when the literal is dumped. However, if the empty array
           is assigned to an array of known type, a new empty array of known
           type is generated to replace it. *)
        s#add "(ignore("; f(); s#add ");";
        s#add "(new Js_runtime.";
        s#add (s#get_array_type_name target_type);
        s#add " Js_runtime.Array.prototype [||]))";
      | _, _ ->
        let etl = s#classify_element_type source_type in
        let etr = s#classify_element_type target_type in
        (match etl, etr with
        | l, r when l = r ->
          f();
        | _, Tagged_t ->
          s#add "(new Js_runtime.arrayWrapper "; f(); s#add ")";
        | Tagged_t, _ ->
          s#add "("; f(); s#add ")";
        | _ ->
          (* TODO: probably needs a wrapper of some sort *)
          f()
        );
      );
    | Array_t, Tagged_t ->
      s#add "(Js_runtime.cnv_array_to_mixed ";
      f();
      s#add ")";

    | Boolean_t, Boolean_t
    | Function_t, Object_t
    | Number_t, Number_t
    | Object_t, Function_t
    | String_t, String_t
    | Symbol_t, Symbol_t
    | Tagged_t, Tagged_t
    | Void_t, Void_t ->
      f();

    | Object_t, Object_t ->
      if source_type = target_type then
        f()
      else begin
        s#add "((Obj.magic "; f(); s#add ") :> ";
        s#dump_type target_type; s#add ")"
      end;

    | Function_t, Function_t ->
      if source_type = target_type then
        f()
      else begin
        s#add "("; f(); s#add " :> "; s#dump_type target_type; s#add ")"
      end;

    | Array_t, Void_t
    | Boolean_t, Void_t
    | Function_t, Void_t
    | Number_t, Void_t
    | Object_t, Void_t
    | String_t, Void_t
    | Symbol_t, Void_t
    | Tagged_t, Void_t ->
      s#add "(ignore("; f(); s#add "); Js_runtime.Undefined)";

    | Boolean_t, Tagged_t ->
      s#add "(Js_runtime.Boolean "; f(); s#add ")";
    | Number_t, Tagged_t ->
      s#add "(Js_runtime.Number "; f(); s#add ")";
    | Function_t, Tagged_t
    | Object_t, Tagged_t ->
      s#add "(Js_runtime.Object (";
      f();
      s#add " :> Js_runtime.baseObject))";
    | String_t, Tagged_t ->
      s#add "(Js_runtime.String "; f(); s#add ")";
    | Symbol_t, Tagged_t ->
      f();
    | Void_t, Tagged_t ->
      f();

    | Boolean_t, Number_t ->
      s#add "(Js_runtime.cnv_bool_to_num "; f(); s#add ")";
    | Boolean_t, String_t ->
      s#add "(Js_runtime.cnv_bool_to_str "; f(); s#add ")";
    | Number_t, String_t ->
      s#add "(Js_runtime.cnv_num_to_str "; f(); s#add ")";
    | Number_t, Boolean_t ->
      s#add "(Js_runtime.cnv_num_to_bool "; f(); s#add ")";
    | String_t, Number_t ->
      s#add "(Js_runtime.cnv_str_to_num "; f(); s#add ")";
    | String_t, Boolean_t ->
      s#add "(Js_runtime.cnv_str_to_bool "; f(); s#add ")";

    | _, Array_t ->
      let etr = s#classify_element_type target_type in
      s#add "(Js_runtime.cnv_mixed_to_"; s#add (prefix etr); s#add "_array ";
      s#dump_cnv_to_mixed f source_type;
      s#add ")";
    | _, Boolean_t ->
      s#add "(Js_runtime.cnv_mixed_to_bool ";
      s#dump_cnv_to_mixed f source_type;
      s#add ")";
    | _, Function_t ->
      s#add "((Obj.magic (Js_runtime.cnv_mixed_to_object ";
      s#dump_cnv_to_mixed f source_type;
      s#add ")) :> "; s#dump_type target_type; s#add ")";
    | _, Number_t ->
      s#add "(Js_runtime.cnv_mixed_to_num ";
      s#dump_cnv_to_mixed f source_type;
      s#add ")";
    | _, Object_t ->
      s#add "((Obj.magic (Js_runtime.cnv_mixed_to_object ";
      s#dump_cnv_to_mixed f source_type;
      s#add ")) :> "; s#dump_type target_type; s#add ")";
    | _, String_t ->
      s#add "(Js_runtime.cnv_mixed_to_str ";
      s#dump_cnv_to_mixed f source_type;
      s#add ")";

    | _, _ ->
      s#dump_cnv_from_to (fun () -> s#dump_cnv_from_to f source_type mixedT)
        mixedT target_type;

  method dump_expression ?(hint=None) expr : Ty.t =
    let open Expression in
    let loc, e = expr in
    match e with
    | This ->
      s#dump_this ();
    | Super ->
      s#dump_identifier loc "super";
    | Array {Array.elements} ->
      s#dump_array expr elements;
    | Object {Object.properties} ->
      s#dump_object expr properties;
    | Function {Function.id; params; body; async;
        generator; predicate; expression; returnType; typeParameters} ->
      s#dump_function_expression expr id params body async
        generator predicate expression returnType typeParameters;
    | ArrowFunction {Function.id; params; body; async;
        generator; predicate; expression; returnType; typeParameters} ->
      s#dump_function_expression expr id params body async
        generator predicate expression returnType typeParameters;
    | Sequence {Sequence.expressions} ->
      s#dump_sequence loc expressions;
    | Unary {Unary.operator; prefix; argument} ->
      s#dump_unary operator prefix argument;
    | Binary {Binary.operator; left; right} ->
      s#dump_binary operator left right;
    | Assignment {Assignment.operator; left; right} ->
      s#dump_assignment expr operator left right;
    | Update {Update.operator; argument; prefix} ->
      s#dump_update expr operator argument prefix;
    | Logical {Logical.operator; left; right} ->
      s#dump_logical ~hint:hint operator left right;
    | Conditional {Conditional.test; consequent; alternate} ->
      s#dump_conditional expr test consequent alternate;
    | New {New.callee; arguments} ->
      s#dump_new expr callee arguments;
    | Call {Call.callee; arguments} ->
      s#dump_call callee arguments;
    | Member {Member._object; property; computed} ->
      s#dump_member ~hint:hint _object property computed;
    | Yield {Yield.argument; delegate} ->
      s#dump_yield argument delegate;
    | Comprehension {Comprehension.blocks; filter} ->
      s#dump_comprehension blocks filter;
    | Generator {Generator.blocks; filter} ->
      s#dump_generator blocks filter;
    | Identifier (loc, name) ->
      s#dump_identifier loc name;
    | Literal {Literal.value; raw} ->
      s#dump_literal value raw;
    | TemplateLiteral {TemplateLiteral.quasis; expressions} ->
      s#dump_templateLiteral quasis expressions;
    | TaggedTemplate {TaggedTemplate.tag; quasi} ->
      s#dump_taggedTemplate tag quasi;
    | JSXElement {JSX.openingElement; closingElement; children} ->
      s#dump_jsxElement openingElement closingElement children;
    | Class {Class.id; body; superClass; typeParameters;
        superTypeParameters; implements; classDecorators} ->
      s#dump_class id body superClass typeParameters
        superTypeParameters implements classDecorators;
    | TypeCast {TypeCast.expression; typeAnnotation} ->
      s#dump_typeCast expression typeAnnotation;
    | MetaProperty {MetaProperty.meta; property} ->
      s#dump_meta_property meta property

  method dump_this () : Ty.t =
    s#add "_this";
    current_this_t

  method get_array_type_name array_type =
    match s#classify_element_type array_type with
    | Boolean_t -> "booleanArrayInstance"
    | Number_t -> "numberArrayInstance"
    | Array_t
    | Function_t
    | Object_t -> "objectArrayInstance"
    | Symbol_t -> "symbolArrayInstance"
    | String_t -> "stringArrayInstance"
    | _ -> "tagArrayInstance"

  method dump_array expr elements =
    (* todo: infer the element type *)
    let array_type = s#get_expression_type expr in
    match array_type with
    | Ty.ArrT (_, Ty.ArrayAT (Ty.EmptyT _, _)) ->
      s#add "(new Js_runtime.tagArrayInstance Js_runtime.Array.prototype [||])";
      array_type
    | Ty.ArrT (_, (Ty.ArrayAT (elemt, _)
                 | Ty.TupleAT (elemt, _)
                 | Ty.ROArrayAT (elemt))) ->
      let t = s#get_array_type_name array_type in
      s#add ("(new Js_runtime." ^ t ^ " Js_runtime.Array.prototype [|");
      let spread_element = ref None in
      elements |> List.iter
        (fun elem ->
          match elem with
          | Some (Expression.Expression e) ->
            s#dump_conversion e elemt; s#add "; ";
          | Some (Expression.Spread
              (_, {Expression.SpreadElement.argument})) ->
            spread_element := Some argument;
          | None ->
            s#add "(Obj.magic Js_runtime.undefined_magic);"
          );
      s#add "|]";
      (match !spread_element with
      | None -> ()
      | Some e ->
        s#add " |> Js_runtime.Array.add_spread ";
        s#dump_conversion e mixedT
      );
      s#add ")";
      array_type;
    | _ -> failwith "unexpected type for array literal"

  method dump_object expr properties =
    let open Expression.Object in
    let object_type = s#get_expression_type expr in
    match object_type with
    | Ty.ObjT (_, {Ty.flags; Ty.props_tmap; _}) when flags.Ty.exact ->
      let prop_map = Context.find_props context props_tmap in
      let class_name = s#get_class_name_for object_type in
      obj_typedef_for_name <-
        SMap.add class_name object_type obj_typedef_for_name;
      s#add "(new "; s#add class_name;
      let initialized_prop_names = properties |> List.fold_left
        (fun acc prop ->
          match prop with
          | Property (_, {Property.key; value; _method; shorthand }) ->
            let prop_name =
              s#dump_property key value _method shorthand prop_map in
            SSet.add prop_name acc
          | SpreadProperty _ -> failwith "spread property")
        SSet.empty in
      ignore (SMap.fold
        (fun pn p acc ->
          if SSet.mem pn acc then acc
          else
            match p with
            | Ty.Field (t, _) ->
              s#add " ~cons_param"; s#add_escaped pn; s#add ":";
              s#dump_default_value_for_some t;
              SSet.add pn acc
            | _ ->
              failwith "TODO: Unsupported property")
        prop_map
        initialized_prop_names);
      s#add " ~prototype:(";
      let proto_t = s#get_updated_prototype_for object_type in
      if class_name = (s#get_class_name_for proto_t) then
        s#add "Js_runtime.Object.prototype"
      else
        s#dump_default_value_for_some (s#merge_possible_types proto_t);
      s#add " :> Js_runtime.baseObject))";
      object_type
    | _ -> failwith (spf "unsupported object literal at %s "
      (Loc.to_string ~include_source:true (fst expr)));

  method dump_property key value _method shorthand prop_map =
    let open Expression.Object.Property in
    if shorthand then failwith "property is shorthand";
    s#add " ~cons_param";
    let value = match value with
    | Init value -> value
    | Get (loc, f) -> s#add "_get"; loc, Expression.Function f
    | Set (loc, f) -> s#add "_set"; loc, Expression.Function f
    in
    let prop_name = match key with
      | Identifier (_, name) ->
        s#add_escaped name; name
      | Literal (_, {Literal.value = Literal.String str; _}) ->
        s#add_escaped str; str
      | _ -> failwith "Non identifier key not yet supported" in
    s#add ":";
    let prop_type =
      match SMap.get prop_name prop_map with
      | Some (Ty.Field (t, _)) -> t
      | _ -> mixedT in
    s#dump_conversion value prop_type;
    prop_name

  method dump_function_expression
      ((loc, _) as expr) id params body async
      generator predicate expression returnType typeParameters : Ty.t =
    if id = None then (
      func_level <- func_level + 1;
      s#setup_function_maps (s#get_expression_type expr) loc loc None;
      s#indent "(";
      let funcob_t = s#dump_function_declaration_as_funcob_instance
        ~is_method:false loc id params body async
        generator predicate expression returnType typeParameters in
      s#unindent ")";
      func_level <- func_level - 1;
      funcob_t)
    else (
      func_level <- func_level + 1;
      s#indent "(";
      let fty = s#dump_function loc id params body async
        generator predicate expression returnType typeParameters in
      s#unindent ")";
      func_level <- func_level - 1;
      fty);

  method dump_sequence loc expressions =
    s#add "(";
    let n = List.length expressions in
    expressions |> List.iteri
      (fun i e ->
        if i = (n - 1) then
          ignore (s#dump_expression e)
        else
          (s#add "ignore "; ignore (s#dump_expression e); s#add "; "));
    s#add ")";
    s#get_some_type_for loc

  method dump_unary operator prefix argument =
    assert prefix; (* seems like a useless relic from the past *)
    let open Expression in
    let open Expression.Unary in
    match operator with
    | Minus ->
      s#add "(";
      let argument_t = s#dump_as_temp argument "argument" in
      s#add " ~-. ";
      s#dump_cnv_temp_from_to "argument" argument_t numT;
      s#add ")";
      numT
    | Plus ->
      s#add "(";
      let argument_t = s#dump_as_temp argument "argument" in
      s#dump_cnv_temp_from_to "argument" argument_t numT;
      s#add ")";
      numT
    | Not ->
      s#add "(";
      let argument_t = s#dump_as_temp argument "argument" in
      s#add " not ";
      s#dump_cnv_temp_from_to "argument" argument_t boolT;
      s#add ")";
      boolT
    | BitNot ->
      s#add "(";
      let argument_t = s#dump_as_temp argument "argument" in
      s#add " Int32.to_float (Int32.lognot (Int32.of_float ";
      s#dump_cnv_temp_from_to "argument" argument_t numT;
      s#add ")))";
      numT
    | Typeof ->
      s#add "(";
      let argument_t = s#dump_as_temp argument "argument" in
      s#dump_typeof "argument" argument_t
    | Void ->
      s#add "(ignore (";
      ignore (s#dump_expression argument);
      s#add "); Js_runtime.Undefined)";
      mixedT
    | Delete ->
      (match argument with
      | _, (Member {Member._object = (_, Super); _}) ->
        s#add "(raise (Js_runtime.referenceError ()))";
      | _, (Member {Member._object; property; _}) ->
        s#add "(";
        let obj_t = s#dump_as_temp _object "obj" in
        let obj_c = s#classify_type obj_t in
        (match obj_c with
        | Object_t
        | Tagged_t
        | Function_t
        | Array_t ->
          (match property with
          | Expression.Member.PropertyIdentifier (_, name) ->
            s#add "(let sym = ";
            s#add (s#symbol_name name);
            s#add " in ";
            (match obj_c with
            | Function_t
            | Object_t
            | Array_t ->
              s#add "obj#delete sym";
            | _ ->
              s#add "(Js_runtime.delete obj sym)";
            );
            s#add ")";
          | Expression.Member.PropertyExpression expr ->
            let expr_t = s#dump_as_temp expr "expr" in
            s#add "(let sym = Js_runtime.js_SymbolFor ";
            s#dump_cnv_temp_from_to "expr" expr_t stringT;
            s#add " in ";
            (match obj_c with
            | Array_t
            | Object_t ->
              s#add "obj#delete sym";
            | _ ->
              s#add "(Js_runtime.delete obj sym)";
            );
            s#add ")";
          );
        | _ ->
          s#add "ignore (obj); true";
        );
        s#add ")";
      | _, (Identifier _ ) ->
        (* TODO: delete properties of the global object *)
        s#add "false";
      | _ ->
        s#add "(ignore (";
        ignore (s#dump_expression argument);
        s#add "); true)";
      );
      boolT
    | Await ->
      failwith "not implemented: await";

  method dump_typeof temp_name temp_t =
    (match s#classify_type temp_t with
    | Tagged_t
    | Object_t ->
      s#add "Js_runtime.typeof ";
      s#dump_cnv_temp_from_to temp_name temp_t mixedT;
      s#add ")";
    | _ ->
      s#add "(ignore "; s#add temp_name; s#add ")";
      s#add "; \"";
      s#add (s#get_classified_type_name temp_t);
      s#add"\")");
    stringT

  method dump_binary operator left right : Ty.t =
    let open Expression.Binary in
    match operator with
    | Equal
    | NotEqual
    | StrictEqual
    | StrictNotEqual ->
      s#dump_equal operator left right
    | LessThan
    | LessThanEqual
    | GreaterThan
    | GreaterThanEqual ->
      s#dump_relational operator left right
    | LShift
    | RShift
    | RShift3 ->
      s#dump_shift operator left right
    | BitOr
    | Xor
    | BitAnd ->
      s#dump_bitwise_binop operator left right
    | Minus
    | Mult
    | Exp
    | Div ->
      s#dump_arith_binop operator left right
    | Mod ->
      s#dump_mod left right
    | Plus ->
      s#dump_plus left right
    | In ->
      s#dump_in left right
    | Instanceof ->
      s#dump_instanceof left right

  method dump_equal operator left right =
    let open Expression.Binary in
    s#add "(";
    let left_t = s#dump_as_temp left "left" in
    let right_t = s#dump_as_temp right "right" in
    match  (s#classify_type left_t),  (s#classify_type right_t) with
    | Boolean_t, Boolean_t
    | Number_t, Number_t
    | String_t, String_t ->
      (match operator with
      | Equal | StrictEqual -> s#add "left = right"
      | NotEqual | StrictNotEqual | _ -> s#add "left <> right");
      s#add ")";
      boolT
    | lt, rt when s#is_unboxed_object lt && s#is_unboxed_object rt &&
        (operator = StrictEqual || operator = StrictNotEqual) ->
      s#add "(left :> Js_runtime.baseObject)";
      (match operator with
      | StrictEqual -> s#add " == "
      | StrictNotEqual | _ -> s#add " != ");
      s#add "(right :> Js_runtime.baseObject)";
      s#add ")";
      boolT
    | _ ->
      s#add "let lhs = ";
      s#dump_cnv_temp_from_to "left" left_t mixedT;
      s#add " in let rhs = ";
      s#dump_cnv_temp_from_to "right" right_t mixedT;
      s#add " in ";
      (match operator with
      | Equal -> s#add "Js_runtime.equals";
      | NotEqual -> s#add "Js_runtime.not_equals";
      | StrictEqual -> s#add "Js_runtime.strict_equals";
      | StrictNotEqual | _ -> s#add "Js_runtime.strict_not_equals"
      );
      s#add " lhs rhs)";
      boolT

  method dump_relational operator left right : Ty.t =
    let open Expression.Binary in
    s#add "(";
    let left_t = s#dump_as_temp left "left" in
    let right_t = s#dump_as_temp right "right" in
    match  (s#classify_type left_t),  (s#classify_type right_t) with
    | String_t, String_t ->
      s#add "left";
      (match operator with
      | LessThan -> s#add " < "
      | LessThanEqual -> s#add " <= "
      | GreaterThan -> s#add " > "
      | GreaterThanEqual | _ -> s#add " >= ");
      s#add "right)";
      boolT
    | Number_t, Number_t ->
      s#add "left";
      (match operator with
      | LessThan -> s#add " < "
      | LessThanEqual -> s#add " <= "
      | GreaterThan -> s#add " > "
      | GreaterThanEqual | _ -> s#add " >= ");
      s#add "right)";
      boolT
    | Number_t, _ ->
      s#add "left";
      (match operator with
      | LessThan -> s#add " < "
      | LessThanEqual -> s#add " <= "
      | GreaterThan -> s#add " > "
      | GreaterThanEqual | _ -> s#add " >= ");
      s#dump_to_primitive_number "right" right_t; s#add ")";
      boolT
    | _, Number_t ->
      s#dump_to_primitive_number "left" left_t;
      (match operator with
      | LessThan -> s#add " < "
      | LessThanEqual -> s#add " <= "
      | GreaterThan -> s#add " > "
      | GreaterThanEqual | _ -> s#add " >= ");
      s#add "right)";
      boolT
    | _ ->
      (match operator with
      | LessThan -> s#add "Js_runtime.less_than"
      | LessThanEqual -> s#add "Js_runtime.less_than_or_equal"
      | GreaterThan -> s#add "Js_runtime.greater_than"
      | GreaterThanEqual | _ -> s#add "Js_runtime.greater_than_or_equal");
      s#add " ";
      s#dump_cnv_temp_from_to "left" left_t mixedT;
      s#add " ";
      s#dump_cnv_temp_from_to "right" right_t mixedT;
      s#add ")";
      boolT

  method dump_shift operator left right =
    let open Expression.Binary in
    s#add "(Int32.to_float (";
    s#add (match operator with
    | LShift -> "Int32.shift_left "
    | RShift -> "Int32.shift_right "
    | RShift3 | _ -> "Int32.shift_right_logical ");
    s#dump_conversion_to_int32 left;
    s#add " ";
    s#dump_conversion_to_int right;
    s#add "))";
    numT

  method dump_bitwise_binop operator left right =
    let open Expression.Binary in
    s#add "(Int32.to_float (";
    s#add (match operator with
    | BitOr -> "Int32.logor "
    | Xor -> "Int32.logxor "
    | BitAnd | _ -> "Int32.logand ");
    s#dump_conversion_to_int32 left;
    s#add " ";
    s#dump_conversion_to_int32 right;
    s#add "))";
    numT

  method dump_arith_binop operator left right =
    let open Expression.Binary in
    s#add "(";
    s#add "let left = "; s#dump_conversion left numT; s#add " in ";
    s#add "let right = "; s#dump_conversion right numT; s#add " in ";
    s#add " left ";
    s#add (match operator with
    | Minus -> " -. "
    | Mult -> " *. "
    | Exp -> " ** "
    | Div | _ -> " /. ");
    s#add "right)";
    numT

  method dump_mod left right =
    s#add "(mod_float ";
    s#dump_conversion left numT;
    s#add " ";
    s#dump_conversion right numT;
    s#add ")";
    numT

  method dump_plus left right =
    s#add "(";
    let left_t = s#dump_as_temp left "left" in
    let right_t = s#dump_as_temp right "right" in
    match  (s#classify_type left_t),  (s#classify_type right_t) with
    | Number_t, Number_t ->
      s#add "left +. right)";
      numT
    | String_t, String_t ->
      s#add "left ^ right)";
      stringT
    | String_t, _ ->
      s#add "ignore (right); left ^ ";
      s#dump_primitive_conversion_to_string "right" right_t;
      s#add ")";
      stringT
    | _, String_t ->
      s#add "ignore (left); ";
      s#dump_primitive_conversion_to_string "left" left_t;
      s#add " ^ right)";
      stringT
    | _, _ ->
      s#add "Js_runtime.plus ";
      s#dump_cnv_temp_from_to "left" left_t mixedT;
      s#add " ";
      s#dump_cnv_temp_from_to "right" right_t mixedT;
      s#add ")";
      mixedT

  method dump_in left right =
    s#add "(";
    let left_t = s#dump_as_temp left "left" in
    let right_t = s#dump_as_temp right "right" in
    let dump_runtime_lookup () =
      s#add "let lhs = ";
      s#dump_cnv_temp_from_to "left" left_t mixedT;
      s#add " in let rhs = ";
      s#dump_cnv_temp_from_to "right" right_t mixedT;
      s#add " in (Js_runtime.has_property rhs";
      s#add " (Js_runtime.to_property_key lhs)))"; in
    (match left_t, right_t with
    | (Ty.StrT (_, (Ty.Literal str))), (Ty.ObjT _)->
      (match SMap.get str (s#get_property_types right_t) with
      | Some _ -> s#add "(ignore (left); ignore(right); true))"
      | None -> dump_runtime_lookup ()) (* TODO: check sealed *)
    | _ -> dump_runtime_lookup());
    boolT

  method dump_instanceof left right =
    s#add "(";
    let left_t = s#dump_as_temp left "left" in
    let right_t = s#dump_as_temp right "right" in
    s#add "let lhs = ";
    s#dump_cnv_temp_from_to "left" left_t mixedT;
    s#add " in let rhs = ";
    s#dump_cnv_temp_from_to "right" right_t mixedT;
    s#add " in Js_runtime.instanceof_operator lhs rhs)";
    boolT

  method dump_to_primitive_number temp_name temp_type =
    s#add "(Js_runtime.to_primitive_number ";
    s#dump_cnv_temp_from_to temp_name temp_type mixedT;
    s#add ")";

  method dump_assignment ?(prefix=true) expr operator left right =
    let open Expression in
    let bin_op =
      match operator with
      | Assignment.Assign -> Binary.Equal
      | Assignment.PlusAssign -> Binary.Plus
      | Assignment.MinusAssign -> Binary.Minus
      | Assignment.MultAssign -> Binary.Mult
      | Assignment.ExpAssign -> Binary.Exp
      | Assignment.DivAssign -> Binary.Div
      | Assignment.ModAssign -> Binary.Mod
      | Assignment.LShiftAssign -> Binary.LShift
      | Assignment.RShiftAssign -> Binary.RShift
      | Assignment.RShift3Assign -> Binary.RShift3
      | Assignment.BitOrAssign -> Binary.BitOr
      | Assignment.BitXorAssign -> Binary.Xor
      | Assignment.BitAndAssign -> Binary.BitAnd in
    match left with
    | (id_loc, Pattern.Identifier {Pattern.Identifier.name; _}) ->
      s#dump_assign_to_identifier prefix expr bin_op id_loc name right
    | (_, Pattern.Expression
        (_, Member {Member._object; property; _})) ->
      (match property with
      | Member.PropertyIdentifier (_, name) ->
        s#dump_assign_to_named_property
          prefix expr bin_op _object name left right
      | Member.PropertyExpression prop_expr ->
        s#dump_assign_to_computed_property
          prefix expr bin_op _object prop_expr left right)
    | _ ->
      if operator = Assignment.Assign then begin
        s#type_lhside_identifiers left;
        s#add "(";
        s#dump_assign_initializer_to_pattern
           Statement.VariableDeclaration.Var left (Some right);
        s#add " ())";
        mixedT
      end else
        failwith (spf "Unexpected AssignmentOp lh at %s"
         (Loc.to_string ~include_source:true (fst expr)));

  method dump_assign_to_identifier
    prefix assign bin_op id_loc (name_loc, name) right =
    let open Expression in
    let loc, _ = assign in
    s#add "(";
    let expr = if bin_op = Binary.Equal then
      right
    else begin
      let t = s#dump_as_temp
        (id_loc, (Expression.Identifier (name_loc, name))) "left" in
      type_for_temp <- LocMap.add id_loc t type_for_temp;
      let lh_value = id_loc, Expression.Identifier (id_loc, "left") in
      (loc, Binary
        {Binary.operator = bin_op;
        left = lh_value;
        right = right})
    end in
    let rht = s#dump_as_temp expr "rh_value" in
    let with_count, lt, binding = s#get_binding name (Some current_scope) 0 in
    (match with_count, binding with
    | 0, VarBinding ->
      s#add "id"; s#add_escaped name; s#add " := ";
      s#dump_cnv_temp_from_to "rh_value" rht lt;
    | n, VarBinding ->
      s#add (spf "(if not (Js_runtime.assigned_to_with_prop %s %n "
        (s#symbol_name name) n);
      s#dump_cnv_temp_from_to "rh_value" rht mixedT;
      s#add ") then ";
      s#add "id"; s#add_escaped name; s#add " := ";
      s#dump_cnv_temp_from_to "rh_value" rht lt;
      s#add ")";
    | _, UnknownBinding ->
      s#add (spf "Js_runtime.set_identifier %s "
        (s#symbol_name name));
      s#dump_cnv_temp_from_to "rh_value" rht mixedT;
    | _ ->
      s#add "ignore (raise (Js_runtime.js_ReferenceError ()));";
    );
    if prefix then
      (s#add "; rh_value)"; rht)
    else
      (s#add "; left)"; lt)

  method infer_op_assign_type bin_op t rt =
    if bin_op <> Expression.Binary.Plus then
      numT
    else
      match t, rt with
      | Ty.StrT _, _ -> t
      | _, Ty.StrT _ -> rt
      | Ty.NumT _, Ty.NumT _ -> t
      | _, _ -> mixedT

  method dump_assign_to_named_property
      prefix assign bin_op _object prop_name (lloc, _) right =
    s#add "(";
    let loc, _ = assign in
    let rloc, _ = right in
    let ot = s#dump_as_temp _object "obj" in
    let rt = s#dump_as_temp right "right" in
    s#dump_assign_to_named_property_with_temps
      loc lloc rloc ot rt prefix bin_op _object prop_name

  method dump_assign_to_named_property_with_temps
      loc lloc rloc ot rt prefix bin_op _object prop_name =
    let open Expression in
    let rec dump ot =
      match ot with
      | Ty.ObjT (_, {Ty.props_tmap; _}) ->
        let prop_map = Context.find_props context props_tmap in
        s#dump_assign_to_named_property_or_field
          ot prefix loc bin_op prop_name prop_map lloc rloc rt
      | Ty.InstanceT (_, _, _, _, {Ty.fields_tmap; _ }) ->
        let prop_map = Context.find_props context fields_tmap in
        s#dump_assign_to_named_property_or_field
          ot prefix loc bin_op prop_name prop_map lloc rloc rt
      | Ty.FunT (_, funcob_t, _, _) ->
        let props_tmap =
          match funcob_t with
          | Ty.ObjT (_, {Ty.props_tmap; _}) -> props_tmap
          | _ -> failwith "funcob_t is not an InstanceT" in
        let prop_map = Context.find_props context props_tmap in
        s#dump_assign_to_named_property_or_field
          ot prefix loc bin_op prop_name prop_map lloc rloc rt
      | Ty.MaybeT (_, t) ->
        s#add "let obj = ";
        s#dump_cnv_temp_from_to "obj" ot t;
        s#add " in ";
        dump t
      | _ ->
        s#add "(Js_runtime.set_property_expression ";
        s#dump_cnv_temp_from_to "obj" ot mixedT;
        s#add " ";
        s#add (s#symbol_name prop_name);
        s#add " ";
        s#dump_cnv_temp_from_to "right" rt mixedT;
        s#add "; right))";
        rt
    in
      if prop_name = "prototype" then begin
        match _object with
        | _, Identifier (_, name) ->
          let wc, t, binding = s#get_binding name (Some current_scope) 0 in
          if t != ot then failwith "type inference from identifier wrong";
          (match wc, binding with
          | 0, FuncBinding loc ->
            let prototype = s#get_prototype_for t in
            updated_prototype_map <-
              TypeMap.add t rt updated_prototype_map;
            let pn = s#get_prototype_name_for loc name rt in
            prototype_name_map <-
              TypeMap.add prototype ("!" ^ pn) prototype_name_map;
            s#add "(obj#set_prototype right))";
            rt
          | _ ->
            dump ot
          );
        | _ ->
          dump ot;
      end else
        dump ot;

  method dump_assign_to_named_property_or_field
      ot prefix loc bin_op prop_name prop_map lloc rloc rt =
    let open Expression in
    let rh_value' = rloc, (Expression.Identifier (rloc, "right")) in
    type_for_temp <- LocMap.add rloc rt type_for_temp;
    let rh_value = if bin_op = Binary.Equal then
      rh_value'
    else
      loc, Expression.Binary
        {Binary.operator = bin_op;
        left = (lloc, Expression.Identifier (lloc, "left"));
        right = rh_value'} in
    match SMap.get prop_name prop_map with
    | Some (Ty.Field (t, _)) ->
      if bin_op <> Binary.Equal then
        (s#add "let left = obj#get";
        s#add_escaped prop_name; s#add " in ");
      type_for_temp <- LocMap.add lloc t type_for_temp;
      let rt = s#dump_as_temp rh_value "rh_value" in
      s#add "let arg = ";
      s#dump_cnv_temp_from_to "rh_value" rt t;
      s#add " in ";
      s#add "(obj#set"; s#add_escaped prop_name; s#add " arg";
      if prefix then
        (s#add "; rh_value))"; rt)
      else
        (s#add "; left))"; t)
    | Some _ ->
      failwith "TODO: Unsupported property";
    | None ->
      let rt = s#dump_as_temp rh_value "rh_value" in
      s#add "(Js_runtime.set_property_expression ";
      s#dump_cnv_temp_from_to "obj" ot mixedT;
      s#add " ";
      s#add (s#symbol_name prop_name);
      s#add " ";
      s#dump_cnv_temp_from_to "rh_value" rt mixedT;
      s#add "; rh_value))";
      rt

  method dump_assign_to_computed_property
      prefix assign bin_op _object prop_expr left right : Ty.t =
    let open Expression in
    let loc, _ = assign in
    let lloc, _ = left in
    let rloc, _ = right in
    s#add "(";
    let ot = s#dump_as_temp _object "obj" in
    let et = s#get_element_type ot in
    let pt = s#dump_as_temp prop_expr "index" in
    let rt = s#dump_as_temp right "right" in
    (match ot, pt with
    | Ty.ObjT _, Ty.SingletonStrT (_, str)
    | Ty.ObjT _, Ty.StrT (_, Ty.Literal str) ->
      s#add " ignore (index); ";
      s#dump_assign_to_named_property_with_temps
        loc lloc rloc ot rt prefix bin_op _object str
    | _ ->
      if bin_op <> Binary.Equal then
        (match ot, pt with
        | Ty.ArrT _, Ty.NumT _
        | Ty.ArrT _, Ty.SingletonNumT _ ->
          (* todo: do conversion inside get_element *)
          s#add "let left = obj#get_typed_element (int_of_float index) in ";
        | _ ->
          s#add "let left = Js_runtime.lookup_property_expression ";
          s#dump_cnv_temp_from_to "index" pt mixedT;
          s#dump_cnv_temp_from_to "obj" ot mixedT;
          s#add " in ");
      let rh_value' = rloc,
        (Expression.Identifier (rloc, "right")) in
      type_for_temp <- LocMap.add rloc rt type_for_temp;
      type_for_temp <- LocMap.add lloc et type_for_temp;
      let rh_value = if bin_op = Binary.Equal then
        rh_value'
      else begin
        (loc, Binary
          {Binary.operator = bin_op;
          left = (lloc, Expression.Identifier (lloc, "left"));
          right = rh_value'}) end in
      let rht = s#dump_as_temp rh_value "rh_value" in
      (match ot, pt with
      | Ty.ArrT _, Ty.NumT _
      | Ty.ArrT _, Ty.SingletonNumT _ ->
        s#add "obj#set_typed_element (int_of_float index) ";
        s#dump_cnv_temp_from_to "rh_value" rht (s#get_element_type ot);
      | _ ->
        s#add "Js_runtime.set_property_expression ";
        s#dump_cnv_temp_from_to "obj" ot mixedT;
        s#add " ";
        s#dump_cnv_temp_from_to "index" pt mixedT;
        s#add " ";
        s#dump_cnv_temp_from_to "rh_value" rht mixedT);
     if prefix then
      (s#add "; rh_value)"; rht)
    else
      (s#add "; left)"; et))

  method dump_update expr operator argument prefix =
    let open Expression in
    let left =
      match argument with
      | (loc, Identifier id) -> loc, (Pattern.Identifier { Pattern.Identifier.
          name = id;
          typeAnnotation = None;
          optional = false;
        })
      | (loc, _) -> loc, (Pattern.Expression argument) in
    let right = (fst expr),
      (Literal {Literal.value = Literal.Number 1.0; raw = ""}) in
    let assign_op = match operator with
      | Update.Increment -> Assignment.PlusAssign
      | Update.Decrement -> Assignment.MinusAssign in
    s#dump_assignment ~prefix expr assign_op left right

  method dump_logical ?(hint=None) operator left right =
    let open Expression in
    match hint with
    | Some (Ty.BoolT _) ->
      s#add (if operator = Logical.Or then "(if not " else "(if ");
      s#dump_conversion left boolT;
      s#add " then ";
      s#dump_conversion right boolT;
      s#add (if operator = Logical.Or then " else true)" else " else false)");
      boolT
    | _ ->
      s#add "(";
      let lt = s#dump_as_temp left "lval" in
      s#add " (if ";
      if operator = Logical.Or then s#add "not ";
      s#dump_cnv_temp_from_to "lval" lt boolT;
      s#add " then (";
      let rt = s#dump_as_temp right "rval" in
      let t = s#merge_possible_types
        (Ty.UnionT (inference, Ty.UnionRep.make lt rt [])) in
      s#dump_cnv_temp_from_to "rval" rt t;
      s#add ") else ";
      s#dump_cnv_temp_from_to "lval" lt t;
      s#add "))";
      t

  method dump_conditional _e test consequent alternate =
    let saved_buffer = buffer in
    buffer <- Buffer.create 64;
    let lt = s#dump_as_temp consequent "lval" in
    let lbuffer = buffer in
    buffer <- Buffer.create 64;
    let rt = s#dump_as_temp alternate "rval" in
    let rbuffer = buffer in
    buffer <- saved_buffer;
    let t = s#merge_possible_types
      (Ty.UnionT (inference, Ty.UnionRep.make lt rt [])) in
    s#add "(if ";
    s#dump_conversion test boolT;
    s#add " then (";
    s#add_buffer lbuffer;
    s#dump_cnv_temp_from_to "lval" lt t;
    s#add ") else (";
    s#add_buffer rbuffer;
    s#dump_cnv_temp_from_to "rval" rt t;
    s#add "))";
    t

  method dump_new expr callee arguments =
    match s#get_expression_type expr with
    | (Ty.ArrT (_, (Ty.ArrayAT(et, _)
                  | Ty.TupleAT(et, _)
                  | Ty.ROArrayAT(et)))) as arrT ->
      (match arguments with
      | (Expression.Expression e)::[] ->
        (match s#classify_type (s#get_expression_type e) with
        | Number_t ->
          let t = s#get_array_type_name arrT in
          s#add ("(new Js_runtime." ^ t ^ " Js_runtime.Array.prototype ");
          s#add "(Array.make (int_of_float ";
          ignore (s#dump_expression e);
          s#add ") ";
          s#dump_default_value_for_some et;
          s#add "))";
          arrT
        | _ -> s#dump_call ~invoke_name:"construct" callee arguments)
      | _ -> s#dump_call ~invoke_name:"construct" callee arguments)
    | _ ->
      s#dump_call ~invoke_name:"construct" callee arguments

  method dump_arguments arguments types rest_param =
    let rec dump i arguments types = match arguments, types with
      | args, [] ->
        i, args
      | [], t::arg_types ->
        s#add ("let arg_" ^ string_of_int i ^ " = ");
        s#dump_default_value_for_some ~missing:true t;
        s#add " in ";
        dump (i + 1) [] arg_types
      | (Expression.Expression arg)::args, t::arg_types ->
        s#add ("let arg_" ^ string_of_int i ^ " = (");
        s#dump_conversion arg t;
        s#add ") in ";
        dump (i + 1) args arg_types;
      | _, _ ->
        failwith "unimplemented: argument spread" in
    let n, rest_args = dump 1 arguments types in
    let rest_t =
      match rest_param with
      | None -> mixedT
      | Some (_, _, t) -> s#get_element_type t in
    rest_args |> List.iteri
      (fun i arg ->
        match arg with
        | Expression.Expression arg ->
          s#add ("let arg_" ^ string_of_int (i + n) ^ " = (");
          s#dump_conversion arg rest_t;
          s#add ") in ";
        | _ ->
          failwith "unimplemented: argument spread"
      );

  method dump_call ?(invoke_name="call") callee arguments =
    let open Expression in
    let dump_args_def callee_t =
      let tlist, rest_param = match callee_t with
      | Ty.FunT (_, _, _, {Ty.params_tlist; rest_param; _}) ->
        params_tlist, rest_param
      | Ty.CustomFunT (_, Ty.ObjectGetPrototypeOf) ->
        [mixedT], None
      | Ty.FunProtoApplyT _ ->
        [mixedT; mixedT], None
      | Ty.FunProtoBindT _
      | Ty.FunProtoCallT _  ->
        [mixedT], Some (None, Loc.none, mixedT)
      | Ty.ThisClassT (Ty.EmptyT r)
          when (Reason.desc_of_reason r) = Reason.RCustom "Array" ->
        [], Some (None, Loc.none, mixedT)
      | Ty.InstanceT (r, _, _, _, _) ->
        (match Reason.desc_of_reason r with
        | Reason.RCustom "Array"
        | Reason.RCustom "Function" ->
          [], Some (None, Loc.none, mixedT)
        | Reason.RCustom "Date" ->
          [mixedT; mixedT; mixedT; mixedT; mixedT; mixedT; mixedT], None
        | Reason.RCustom "RegExp" ->
          [mixedT; mixedT], None
        | Reason.RCustom name when is_builtin_cons name ->
          [mixedT], None
        | _ -> [], None)
      | _ -> [], None in
      s#dump_arguments arguments tlist rest_param;
    in let dump_args_ref this_arg_t callee_t =
      (* Find the type of this & count other args. *)
      let param_count, has_rest, this_t = match callee_t with
      | Ty.FunT (_, _, _, {Ty.this_t; params_tlist; rest_param; _}) ->
        let this_t = s#merge_possible_types this_t in
        let param_count = List.length params_tlist in
        param_count, rest_param != None, this_t
      | Ty.ThisClassT (Ty.EmptyT r)
          when (Reason.desc_of_reason r) = Reason.RCustom "Array" ->
        0, true, mixedT
      | Ty.InstanceT (r, _, _, _, _) ->
        (match Reason.desc_of_reason r with
        | Reason.RCustom "Array"
        | Reason.RCustom "Function" ->
          0, true, mixedT
        | Reason.RCustom "Date" ->
          7, false, mixedT
        | Reason.RCustom "RegExp" ->
          2, false, mixedT
        | Reason.RCustom name when is_builtin_cons name ->
          1, false, mixedT
        | _ ->
          max (List.length arguments) 1, false, callee_t)
      | Ty.ThisClassT instance_t ->
        (match (Reason.desc_of_reason (Ty.reason_of_t callee_t)) with
        | Reason.RClassType d ->
          (match d with
          | Reason.RCustom name when is_builtin_cons name ->
            max (List.length arguments) 1, false, instance_t
          | _ ->
            0, false, mixedT
          )
        | _ ->
          0, false, mixedT
        )
      | Ty.CustomFunT (_, Ty.ObjectGetPrototypeOf) ->
        1, false, mixedT
      | Ty.FunProtoApplyT _ ->
        2, false, mixedT
      | Ty.FunProtoBindT _
      | Ty.FunProtoCallT _ ->
        1, true, mixedT
      | _ ->
        List.length arguments, false, mixedT in

      (* Dump the this argument. *)
      if invoke_name = "call" then
        s#dump_cnv_temp_from_to "this" this_arg_t this_t;
      (* Dump the fixed arguments. *)
      for i = 1 to param_count do
        s#add (" arg_" ^ string_of_int i)
      done;

      (* Dump the rest arguments. *)
      if has_rest then begin
        let rest_count = (List.length arguments) - param_count in
        s#add " [|";
        for i = 1 to rest_count do
          s#add ("arg_" ^ string_of_int (i + param_count) ^ "; ");
        done;
        s#add "|]"
      end;
    in let dump_call callee_t this_t t pt =
      match pt with
      | Ty.FunT _
      | Ty.FunProtoApplyT _
      | Ty.FunProtoBindT _
      | Ty.FunProtoCallT _
      | Ty.CustomFunT (_, Ty.ObjectGetPrototypeOf)
      | Ty.InstanceT _ ->
        s#add "callee";
        s#add "#"; s#add invoke_name; s#add " ";
        dump_args_ref this_t pt;
        s#add ")";
        let rt =
          match pt with
          | Ty.FunProtoBindT _
          | Ty.FunProtoApplyT _
          | Ty.FunProtoCallT _ -> mixedT
          | Ty.InstanceT (r, _, _, _, _) ->
            (match Reason.desc_of_reason r with
            | Reason.RCustom "Array" -> arrT
            | Reason.RCustom "Boolean" -> boolT
            | Reason.RCustom "Number" -> numT
            | Reason.RCustom "String" -> stringT
            | Reason.RCustom n when is_builtin_cons n ->
              if invoke_name = "call" then pt else this_t
            | _ -> this_t)
          | _ -> s#get_return_type_for (Some callee_t) in
        if invoke_name = "call" then
          rt
        else
          (match TypeMap.get callee_t instance_type_for_constructor with
          | Some t -> t
          | None ->
            (* builtin function *)
            if s#is_unboxed_object (s#classify_type this_t) then
              this_t
            else
              objT)
      | Ty.ThisClassT (Ty.EmptyT r)
          when (Reason.desc_of_reason r) = Reason.RCustom "Array" ->
        s#add "callee";
        s#add "#"; s#add invoke_name;
        dump_args_ref this_t pt;
        s#add ")";
        this_t
      | _ ->
        s#add ("Js_runtime.dynamic_" ^ invoke_name);
        s#add " ";
        s#dump_cnv_temp_from_to "callee" t mixedT;
        if invoke_name = "call" then begin
          s#add " ";
          s#dump_cnv_temp_from_to "this" this_t mixedT;
        end;
        s#add " [|";
        for i = 1 to List.length arguments do
          s#add ("arg_" ^ string_of_int i ^ "; ")
        done;
        s#add "|])";
        mixedT in
    let saved_bound_this_t = current_bound_this_t in
    let return_type =
      match callee with
        | _, Super ->
          if invoke_name = "construct" then failwith "can't do new super";
          s#add "(let this = Obj.magic _this in "; (* co-variant upcast *)
          let _, t, binding = s#get_binding "super" (Some current_scope) 0 in
          (match t with
          | Ty.ThisClassT t ->
            current_bound_this_t <- t;
            let class_name = s#get_class_name_for t in
            let constructor_name = SMap.find class_name constructor_name_for in
            let cons_t = SMap.find constructor_name constructor_type_for in
            dump_args_def cons_t;
            (match binding with
            | FuncBinding _ -> s#add "!id";
            | _ -> failwith "did not bind to a constructor function"
            );
            s#add_escaped constructor_name;
            s#add "#get_typed_prototype#get_constructor";
            dump_args_ref t cons_t;
            s#add ")";
            mixedT
          | _ -> failwith "not implemented"
          )
        | _, (Member {Member._object; property; _}) ->
          s#add "(";
          let ot = s#dump_as_temp _object "this" in
          let ot =
            match ot with
            | Ty.MaybeT (_, t) ->
              s#add "let this = ";
              s#dump_cnv_temp_from_to "this" ot t;
              s#add " in ";
              t
            | t -> t in
          current_bound_this_t <-
            (match ot with
            | Ty.InstanceT (r, _, _, _, _) ->
              (match Reason.desc_of_reason r with
              | Reason.RStatics _ -> mixedT
              | Reason.RCustom n when is_builtin_name n -> mixedT
              | _ -> ot)
            | _ -> mixedT);
          let loc, _ = _object in
          let this_id = loc, "this" in
          let this_expr = loc, (Expression.Identifier this_id) in
          type_for_temp <- LocMap.add loc ot type_for_temp;
          let pt = (match property with
          | Expression.Member.PropertyIdentifier (_, name) ->
            s#add "(let callee = ";
            let pt = s#dump_eval_named_property this_expr name in
            s#add " in ";
            pt
          | Expression.Member.PropertyExpression expr ->
            s#add "(let callee = ";
            let pt = s#dump_eval_computed_property this_expr expr in
            s#add " in ";
            pt
          ) in
          let callee_t =
            match pt with
            | Ty.FunT _
            | Ty.CustomFunT _
            | Ty.FunProtoApplyT _
            | Ty.FunProtoBindT _
            | Ty.FunProtoCallT _ -> pt
            | Ty.InstanceT _ -> TypeMap.find pt constructor_type_for_instance
            | _ -> mixedT in (* todo: implement this case *)
          dump_args_def pt;
          let rt = dump_call callee_t ot pt pt in
          s#add ")";
          rt
        | _ ->
          s#add "(";
          let t = s#dump_as_temp callee "callee" in
          let callee_t, this_t =
            match t with
            | Ty.FunT (_, _, _, {Ty.this_t; _}) ->
              t, s#merge_possible_types this_t
            | Ty.AnyT r when (Reason.desc_of_reason r) = Reason.RDummyThis ->
              failwith "callee type is bound this"
            | Ty.AnyT _ ->
              mixedT, mixedT
            | Ty.ThisClassT (Ty.EmptyT r)
                when (Reason.desc_of_reason r) = Reason.RCustom "Array" ->
              t, arrT
            | Ty.InstanceT (r, _, _, _, _) ->
              (match Reason.desc_of_reason r with
              | Reason.RCustom name when is_builtin_cons name ->
                t, mixedT
              | _ ->
                (match TypeMap.get t constructor_type_for_instance with
                | Some ((Ty.FunT (_, _, _, {Ty.this_t; _})) as ft) ->
                  ft, s#merge_possible_types this_t
                | _ ->
                  mixedT, mixedT))
            | _ ->
              mixedT, mixedT in
          current_bound_this_t <- this_t;
          if invoke_name = "call" then
            (s#add "let this = ";
            s#dump_default_value_for_some this_t;
            s#add " in ");
          dump_args_def callee_t;
          dump_call callee_t this_t t callee_t;
    in
    current_bound_this_t <- saved_bound_this_t;
    return_type

  method get_type_of_property prop =
    match prop with
    | Ty.Field (t, _)
    | Ty.Method t -> s#merge_possible_types t
    | Ty.Set _ -> mixedT
    | Ty.Get t
    | Ty.GetSet (t, _) ->
      match t with
      | Ty.FunT (_, _, _, {Ty.return_t; _}) ->
        s#merge_possible_types return_t
      | _ -> failwith (Debug_js.dump_t context t)

  method get_property_depth_and_type name object_t =
    let rec search_type_and_prototype index ty =
      let search_type_and_proto_t ty proto_t =
        let d, t = s#lookup_property_type index name ty in
        if d >= 0 || name = "prototype" then d, t
        else
          search_type_and_prototype (index+1) proto_t in
      match ty with
      | Ty.ObjT _ ->
        search_type_and_proto_t ty (s#get_prototype_for ty)
      | Ty.ThisClassT instance_t ->
        search_type_and_proto_t instance_t (s#get_prototype_for instance_t)
      | Ty.InstanceT _ ->
        search_type_and_proto_t ty (s#get_prototype_for ty)
      | Ty.FunT (_, statics, _, _) ->
        search_type_and_proto_t statics funProtoT
      | Ty.FunProtoT _ ->
        search_type_and_proto_t ty objProtoT
      | Ty.MaybeT (_, t) ->
        search_type_and_prototype index t
      | _ ->
        (-1), mixedT in
    search_type_and_prototype 0 object_t

  method lookup_property_type index name t =
    match name with
    | "prototype" ->
      (match t with
      | Ty.AnyFunT _ -> (-1), emptyT
      | _ -> index, (s#get_prototype_for t))
    | _ ->
      match t with
      | Ty.ObjT (_, {Ty.props_tmap; _}) ->
        let prop_types = Context.find_props context props_tmap in
        (match SMap.get name prop_types with
        | Some p -> index, (s#get_type_of_property p)
        | None -> (-1), emptyT)
      | Ty.InstanceT (r, _, super_t, _, {Ty.methods_tmap; fields_tmap; _}) ->
        let meth_map = Context.find_props context methods_tmap in
        (match SMap.get name meth_map with
        | Some p when name <> "constructor" ->
          let index' =
            match Reason.desc_of_reason r with
            | Reason.RStatics (Reason.RCustom cname)
                when is_builtin_name cname -> index
            | _ ->
              (* Unless t is a builtin constructor, instance methods
              will be found via the prototype. *)
              index + 1 in
            index', (s#get_type_of_property p)
        | _ ->
          let field_map = Context.find_props context fields_tmap in
          match SMap.get name field_map with
          | Some p ->
            (* Flow models the Function.prototype methods as
            properties of the Function constructor. The runtime is
            as per the specification. Make amends here. *)
            let index' =
              match Reason.desc_of_reason r with
              | Reason.RCustom "Function" ->
                (match name with
                | "apply" | "bind" | "call" -> index + 1
                | _ -> index)
              | _ -> index in
            index', (s#get_type_of_property p)
          | None ->
            (* No instance match, try static match *)
            let static_t = s#get_static_type_for t in
            let index' =
              match static_t with
              | Ty.InstanceT (r, _, _, _, _) ->
                (match Reason.desc_of_reason r with
                | Reason.RStatics (Reason.RCustom cname)
                    when is_builtin_name cname -> index
                | _ ->
                  (* Unless t is a builtin constructor, static methods
                  will be found via the prototype. *)
                  index + 1)
              | _ -> index + 1 in
            (match s#lookup_property_type index' name static_t with
            | _, Ty.EmptyT _ -> (* todo: is this still needed? *)
              (match super_t with
              | Ty.ThisTypeAppT (t, _, _) ->
                (match s#merge_possible_types t with
                | Ty.ThisClassT (Ty.InstanceT (_, _, super, _, _)) ->
                  s#lookup_property_type
                    index' name (s#merge_possible_types super);
                | _ -> (-1), emptyT);
              | _ -> (-1), emptyT);
            | (i, t) -> i, t));
      | _ -> (-1), emptyT

  method dump_member ?(hint=None) _object property _computed =
    match property with
    | Expression.Member.PropertyIdentifier (_, name) ->
      s#dump_eval_named_property _object name
    | Expression.Member.PropertyExpression prop_expr ->
      s#dump_eval_computed_property ~hint:hint _object prop_expr

  method dump_eval_named_property _object name =
    s#add "(";
    let ot = s#dump_as_temp _object "obj" in
    let pd, pt =
      (* Wonderful hack because Flow types iterators as strings. *)
      (match (Reason.desc_of_reason (Ty.reason_of_t ot)) with
      | Reason.RCustom "Symbol" when name = "iterator" ->
        0, mixedT
      (* | Reason.RStatics (Reason.RCustom "Symbol") when name = "iterator" ->
        0, mixedT *)
      | _ ->
        s#get_property_depth_and_type name ot
      ) in
    if pd < 0 then begin
      match ot with
      | Ty.AbstractT _
      | Ty.ArrT _ ->
        (match name with
        | "length" ->
          s#add "(float_of_int obj#length))";
          numT
        | _ ->
          s#add ("(Js_runtime.lookup_property " ^ s#symbol_name name);
          s#dump_cnv_temp_from_to "obj" ot mixedT;
          s#add "))";
          mixedT
        )
      | _ ->
        s#add (spf "(Js_runtime.lookup_property %s " (s#symbol_name name));
        s#dump_cnv_temp_from_to "obj" ot mixedT;
        s#add "))";
        mixedT
    end else begin
      for i = 0 to pd do s#add "("; done;
      (match ot with
      | Ty.MaybeT (_, t) ->
        s#dump_cnv_temp_from_to "obj" ot t
      | _ ->
        s#add "obj");
      for i = 1 to pd do
        s#add "#get_typed_prototype)";
      done;
      s#add "#get"; s#add_escaped name; s#add "))";
      pt
    end

  method dump_eval_computed_property ?(hint=None) _object prop_expr =
    let saved_buffer = buffer in
    buffer <- Buffer.create 64;
    let ot = s#dump_as_temp _object "obj" in
    let pt = s#dump_as_temp prop_expr "index" in
    let obj_index_buffer = buffer in
    buffer <- saved_buffer;
    match ot, pt with
    | Ty.ObjT _, Ty.SingletonStrT (_, str)
    | Ty.ObjT _, Ty.StrT (_, Ty.Literal str) ->
      s#dump_eval_named_property _object str
    | Ty.ArrT (_, (Ty.ArrayAT (elemt, _)
                 | Ty.TupleAT (elemt, _)
                 | Ty.ROArrayAT (elemt))),
      (Ty.NumT _ | Ty.SingletonNumT _) ->
      (match hint with
      | Some ((Ty.MixedT _) as h) ->
        s#add "(";
        s#add_buffer obj_index_buffer;
        s#add "(obj#get_element (int_of_float index)))";
        h
      | _ ->
        s#add "(";
        s#add_buffer obj_index_buffer;
        s#add "(obj#get_typed_element (int_of_float index)))";
        elemt
      )
    | _ ->
      s#add "(";
      s#add_buffer obj_index_buffer;
      s#add "(Js_runtime.lookup_property_expression ";
      s#dump_cnv_temp_from_to "index" pt mixedT;
      s#add " ";
      s#dump_cnv_temp_from_to "obj" ot mixedT;
      s#add "))";
      mixedT

  method dump_yield _argument _delegate =
    s#add "()";
    mixedT

  method dump_comprehension _blocks _filter =
    s#add "()";
    mixedT

  method dump_generator _blocks _filter =
    s#add "()";
    mixedT

  method get_binding name scope_opt with_count =
    match scope_opt with
    | None -> with_count, mixedT, UnknownBinding
    | Some {kind; parent_scope; constants; functions; variables} ->
      let id_opt =
        match SMap.get name constants with
        | Some id -> Some id
        | None ->
          match SMap.get name variables with
          | Some id -> Some id
          | None -> SMap.get name functions in
      let loc, t =
        match id_opt with
        | Some id ->
          let loc, _ = id in
          loc, (s#get_expression_type (loc, (Expression.Identifier id)))
        | None -> Loc.none, mixedT in
      match kind with
      | GlobalScope
      | BlockScope
      | FunctionScope ->
        if SMap.mem name constants then with_count, t, ConstBinding
        else if SMap.mem name variables then with_count, t, VarBinding
        else if SMap.mem name functions then with_count, t, FuncBinding loc
        else s#get_binding name parent_scope with_count
      | WithScope _ ->
        (* todo: look at type of with object *)
        s#get_binding name parent_scope (with_count+1)

  method dump_identifier id_loc name =
    match LocMap.get id_loc type_for_temp with
    | Some t ->
      s#add name; (* compiler temp *)
      t
    | _ ->
      let with_count, t, binding = s#get_binding name (Some current_scope) 0 in
      match with_count, binding with
      | 0, FuncBinding _
      | 0, VarBinding ->
        s#add "!id"; s#add_escaped name; t
      | 0, ConstBinding ->
        s#add "id"; s#add_escaped name; t
      | n, FuncBinding _
      | n, VarBinding ->
        s#add (spf "(Js_runtime.check_with_scopes %s %n "
          (s#symbol_name name) n);
        s#dump_cnv_to_mixed (fun () -> (s#add "!id"; s#add_escaped name)) t;
        s#add ")"; mixedT
      | n, ConstBinding ->
        s#add (spf "(Js_runtime.check_with_scopes %s %n "
          (s#symbol_name name) n);
        s#dump_cnv_to_mixed (fun () -> (s#add "id"; s#add_escaped name)) t;
        s#add ")"; mixedT
      | _, UnknownBinding ->
        let runtime_lookup () =
          s#add (spf "(Js_runtime.lookup_identifier %s)"
            (s#symbol_name name));
          mixedT in
        let rec dump_unknown_binding ty =
          match ty with
          | Ty.UnionT (_, rep) ->
            (* Properties of the Global object can have an unknown type
            if they are assigned to. This possibility causes Flow to model
            global properties with an irregular miscellany of UnionT types.
            We'll assume that assignments do not change the types of
            Global object properties and proceed with the non empty type. *)
            let get_global_prop_type = function
              | ((Ty.FunT _) as t)::[Ty.EmptyT _]
              | ((Ty.ObjT _) as t)::[Ty.EmptyT _]
              | ((Ty.ThisClassT _) as t)::[Ty.EmptyT _]
              | ((Ty.ThisClassT _) as t)::[Ty.ClassT _]
              | (Ty.EmptyT _)::[(Ty.NumT _) as t]
              | (Ty.MixedT _)::[(Ty.NumT _) as t]
              | ((Ty.VoidT _) as t)::[Ty.NumT _]
              | ((Ty.VoidT _) as t)::[Ty.EmptyT _]
              | (Ty.MixedT _)::[(Ty.ClassT _) as t]
              | (Ty.ClassT _)::[(Ty.ClassT _) as t] ->
                Some t
              | _ -> None in
            (match get_global_prop_type (Ty.UnionRep.members rep) with
            | Some t -> dump_unknown_binding t
            | None -> runtime_lookup())
          | Ty.ThisClassT instance_t ->
            let instance_t = s#merge_possible_types instance_t in
            (match (Reason.desc_of_reason (Ty.reason_of_t ty)) with
            | Reason.RClassType d ->
              (match d with
              | Reason.RCustom name when is_builtin_name name ->
                s#add (builtin_name name);
                let static_t = s#get_static_type_for instance_t in
                let proto_t = s#get_prototype_for instance_t in
                prototype_for <- TypeMap.add static_t proto_t prototype_for;
                (match TypeMap.get instance_t constructor_type_for_instance with
                | Some fty ->
                  constructor_type_for <- SMap.add name fty constructor_type_for
                | None -> ());
                instance_t
              | _ ->
                runtime_lookup ())
            | _ ->
              runtime_lookup ())
          | Ty.PolyT (_, t) ->
            dump_unknown_binding t
          | Ty.MixedT _
          | Ty.EmptyT _ ->
            (match name with
            | "Array" ->
              s#add "Js_runtime.Array.constructor";
              Ty.ThisClassT (Ty.EmptyT
                (Reason.locationless_reason (Reason.RCustom "Array")))
            | _ -> runtime_lookup ())
          | _ when is_global_name name ->
            s#add "(Js_runtime.Global.instance#get";
            s#add_escaped name;
            s#add ")";
            ty
          | _ ->
            runtime_lookup ()
          in dump_unknown_binding (s#get_some_type_for id_loc)

  method dump_literal value raw : Ty.t =
    let open Literal in
    match value with
    | String str ->
      s#add (spf "\"%s\"" (String.escaped str));
      stringT
    | Boolean b ->
      s#add (if b then "true" else "false");
      boolT
    | Null ->
      s#add "(Js_runtime.Null)";
      nullT
    | Number n ->
      (* todo: if raw is a valid OCaml number, just dump raw *)
      s#add (string_of_float n);
      Ty.NumT (inference, Ty.Literal (n, raw))
    | RegExp {RegExp.pattern; flags} ->
      s#add (
        "(Js_runtime.Object (" ^
          "(new Js_runtime.regExpInstance " ^
            "Js_runtime.RegExp.prototype " ^
            "\"" ^ (String.escaped pattern) ^ "\" " ^
            "\"" ^ (String.escaped flags) ^ "\"" ^
          ")" ^
          ":> Js_runtime.baseObject" ^
        "))"
      );
      mixedT

  method dump_templateLiteral quasis expressions =
    let open Expression.TemplateLiteral.Element in
    (* Buffer size hint: 4/3 of the size of the quasis *)
    let total_length = List.fold_left (fun acc (_, q) ->
        acc + (String.length q.value.cooked)
    ) 0 quasis in
    let length_hint = total_length + total_length / 3 in
    (* Helper to escape a string *)
    let escape str = "\"" ^ String.escaped str ^ "\"" in
    (* Combine expressions & strings. *)
    s#add ("(let buffer = Buffer.create " ^ string_of_int length_hint ^ " in ");
    let rec dump_template i qs exps = match qs, exps with
      | (_, q)::qs, e::exps ->
        s#add ("Buffer.add_string buffer " ^ escape q.value.cooked ^ ";");
        s#add "Buffer.add_string buffer ";
        s#dump_conversion e stringT;
        s#add ";";
        dump_template (i + 1) qs exps;
      | (_, q)::qs, [] ->
        s#add ("Buffer.add_string buffer " ^ escape q.value.cooked ^ ";");
        dump_template i qs [];
      | [], e::exps ->
        s#add "Buffer.add_string buffer ";
        s#dump_conversion e stringT;
        s#add ";";
        dump_template (i + 1) [] exps;
      | [], [] -> ();
    in dump_template 0 quasis expressions;
    s#add "Buffer.contents buffer)";
    stringT

  method dump_taggedTemplate tag (_, template) =
    let open Expression.TemplateLiteral in
    let open Expression.TemplateLiteral.Element in
    (match tag with
    | _, Expression.Super ->
      failwith "not implemented";
    | _, (Expression.Member {Expression.Member._object; property; _}) ->
      s#add "(";
      let ot = s#dump_as_temp _object "tag_object" in
      let loc, _ = _object in
      let this_id = loc, "tag_object" in
      let this_expr = loc, (Expression.Identifier this_id) in
      type_for_temp <- LocMap.add loc ot type_for_temp;
      let pt = (match property with
      | Expression.Member.PropertyIdentifier (_, name) ->
        s#add "let tag_method = ";
        let pt = s#dump_eval_named_property this_expr name in
        s#add " in ";
        pt
      | Expression.Member.PropertyExpression expr ->
        s#add "let tag_method = ";
        let pt = s#dump_eval_computed_property this_expr expr in
        s#add " in ";
        pt
      ) in
      s#add " let tag_func = ";
      s#dump_cnv_temp_from_to "tag_method" pt mixedT;
      s#add " in let this = ";
      s#dump_cnv_temp_from_to "tag_object" ot mixedT;
    | _ ->
      s#add "(let tag_func = ";
      s#dump_conversion tag mixedT;
      s#add " in let this = ";
      s#add "(Js_runtime.Object (_this :> Js_runtime.js_object_instance))";
    );
    (* Dynamic call to the tag function *)
    s#add " in Js_runtime.dynamic_call tag_func this ";
    (* Evaluate all template arguments. *)
    template.expressions |> List.iteri (fun i e ->
      s#add ("(let arg_" ^ string_of_int i ^ " = ");
      s#dump_conversion e mixedT;
      s#add " in ";
    );
    (* Fetch the template ID from the cache. *)
    let raw = List.map (fun (_, q) -> q.value.raw) template.quasis in
    let cooked = List.map (fun (_, q) -> q.value.cooked) template.quasis in
    let key = Array.of_list raw in
    let id =
      try let (id, _) = TmplMap.find key templates in id
      with Not_found ->
        let id = TmplMap.cardinal templates in
        templates <- TmplMap.add key (id, cooked) templates;
        id
    in
    (* Pass dynamic arguments *)
    s#add "[|";
    s#add ("Js_runtime.get_template tmpl_" ^ string_of_int id ^ ";");
    for i = 1 to List.length template.expressions do
      s#add ("arg_" ^ string_of_int (i - 1) ^ ";");
    done;
    s#add "|]";
    for i = 1 to List.length template.expressions do
      s#add ")";
    done;
    s#add ")";
    mixedT

  method dump_jsxElement _openingElement _closingElement _children =
    s#add "()";
    mixedT

  method dump_class _id _body _superClass _typeParameters
       _superTypeParameters _implements _classDecorators =
    s#add "()";
    mixedT

  method dump_typeCast _expression _typeAnnotation =
    s#add "()";
    mixedT

  method dump_meta_property _meta _property =
    s#add "()";
    mixedT

  method dump_cnv_to_mixed f t =
    let jst = s#classify_type t in
    match jst with
    | Tagged_t
    | Void_t ->
      f()
    | _ ->
      let dump_cnv t =
        s#add ("(Js_runtime." ^ t ^ " ");
        f();
        s#add ")";
      in match jst with
        | Boolean_t  -> dump_cnv "Boolean"
        | Number_t   -> dump_cnv "Number"
        | String_t   -> dump_cnv "String"
        | Symbol_t   -> dump_cnv "Symbol"
        | Array_t
        | Function_t
        | Object_t ->
          s#add "(Js_runtime.Object (";
          f();
          s#add " :> Js_runtime.baseObject))";
        | Tagged_t | Void_t ->
          failwith "this case should already have been handled";

  method get_pattern_type (loc, pattern) : Ty.t =
    match pattern with
    | Pattern.Identifier _ ->
        s#get_some_type_for loc
    | _ -> failwith "not yet implemented"

  method get_expression_type (loc, _) : Ty.t =
    s#merge_possible_types (s#get_open_expression_type loc)

  method merge_possible_types (t : Ty.t) : Ty.t =
    let type_cache = Hashtbl.create 10 in
    let rec merge_flags fl1 fl2 =
      let sealed = match fl1.Ty.sealed, fl2.Ty.sealed with
      | Ty.Sealed, Ty.Sealed -> Ty.Sealed
      | Ty.UnsealedInFile s1, Ty.UnsealedInFile s2 when s1 = s2 ->
        Ty.UnsealedInFile s1
      | _ -> Ty.UnsealedInFile None in
      { Ty.sealed;
        exact = fl1.Ty.exact && fl2.Ty.exact;
        frozen = fl1.Ty.frozen && fl2.Ty.frozen }
    and merge_dicts d1_opt d2_opt =
      match d1_opt, d2_opt with
      | None, None -> None
      | None, Some d | Some d, None -> Some d
      | Some d1, Some d2 ->
        let dict_name = match d1.Ty.dict_name, d2.Ty.dict_name with
        | None, None -> None
        | Some n, _
        | _, Some n -> Some n in
        let mk1 = merge_with_cache d1.Ty.key in
        let mk2 = merge_with_cache d2.Ty.key in
        let key = merge inference [mk1; mk2] in
        let mv1 = merge_with_cache d1.Ty.value in
        let mv2 = merge_with_cache d2.Ty.value in
        let value = merge inference [mv1; mv2] in
        let dict_polarity =
          match d1.Ty.dict_polarity, d2.Ty.dict_polarity with
          | Ty.Neutral, Ty.Neutral -> Ty.Neutral
          | _ -> failwith "TODO: dictionary variance"
        in
        Some { Ty.dict_name; key; value; dict_polarity }
    and merge_props p1 p2 =
      let map1 = Context.find_props context p1 in
      let map2 = Context.find_props context p2 in
      let map12 = SMap.merge
        (fun _k v1_opt v2_opt ->
          (* TODO: Property maps values used to be Ty.t, but are now
           * Ty.property. Properties can be fields with a given variance or
           * getters/setters. The dumper has not been properly updated to
           * support property variance. cc samgoldman *)
          let v1_opt = match v1_opt with
          | Some (Ty.Field (t, _)) -> Some t
          | Some _ -> failwith "TODO: Unsupported property"
          | None -> None
          in
          let v2_opt = match v2_opt with
          | Some (Ty.Field (t, _)) -> Some t
          | Some _ -> failwith "TODO: Unsupported property"
          | None -> None
          in
           match v1_opt, v2_opt with
           | None, None -> None
           | None, Some v
           | Some v, None ->
             Some (Ty.Field (v, Ty.Neutral))
           | Some v1, Some v2 ->
             let mv1 = merge_with_cache v1 in
             let mv2 = merge_with_cache v2 in
             Some (Ty.Field (merge inference [mv1; mv2], Ty.Neutral)))
         map1 map2 in
      Context.make_property_map context map12
    and merge_objects r o1 o2 =
      let flags = merge_flags o1.Ty.flags o2.Ty.flags in
      let dict_t = merge_dicts o1.Ty.dict_t o2.Ty.dict_t in
      let props_tmap = merge_props o1.Ty.props_tmap o2.Ty.props_tmap in
      let mp1 = merge_with_cache o1.Ty.proto_t in
      let mp2 = merge_with_cache o2.Ty.proto_t in
      let proto_t =  merge inference [mp1; mp2] in
      Ty.ObjT (r, {Ty.flags; dict_t; props_tmap; proto_t})
    and builtin_name r =
      (match (Reason.desc_of_reason r) with
      | Reason.RCustom name when is_builtin_name name -> name
      | _ -> "")
    and is_builtin r =
      (builtin_name r ) <> ""
    and merge r = function
      | [] -> Ty.EmptyT r
      | [t] ->
        merge_with_cache t
      | [t; u] when t == u ->
         t
      | [t; u] ->
        let mt = merge_with_cache t in
        let mu = merge_with_cache u in
        let mut = Flow_js.merge_type context (mt, mu) in
        (match mt, mu, mut with
        | Ty.ObjT (_, o1), Ty.ObjT (r, o2), _ ->
          merge_objects r o1 o2
        | Ty.ClassT (Ty.InstanceT (r, _, _, _, _)), Ty.ThisClassT tc, _ ->
          if (Ty.reason_of_t tc) = r && (is_builtin r) then u else mut
        | Ty.InstanceT _, Ty.InstanceT _, Ty.InstanceT _  ->
          mut
        | Ty.InstanceT (_, st1, su1, _, in1),
            Ty.InstanceT (_, st2, su2, _, in2), _ ->
          if st1 = st2 && su1 = su2 && in1 = in2 then mt else mut
        | Ty.InstanceT _, Ty.EmptyT _, _ ->
          let reason = Reason.replace_reason (fun desc -> Reason.RMaybe desc)
            (Ty.reason_of_t mt) in
          Ty.MaybeT (reason, mt)
        | Ty.EmptyT _, Ty.InstanceT _, _ ->
          let reason = Reason.replace_reason (fun desc -> Reason.RMaybe desc)
            (Ty.reason_of_t mu) in
          Ty.MaybeT (reason, mu)
        | (Ty.EmptyT _), (Ty.MaybeT _), _ ->
          mu
        | (Ty.MaybeT _), (Ty.EmptyT _), _ ->
          mt
        | (Ty.MaybeT (r1, t1)), (Ty.MaybeT (_, t2)), _ ->
          Ty.MaybeT (r1, merge r [t1; t2])
        | Ty.ThisClassT tc, Ty.ClassT (Ty.InstanceT (r, _, _, _, _)), _ ->
          if (Ty.reason_of_t tc) = r && (is_builtin r) then t else mut
        | Ty.ThisClassT tc1, Ty.ThisClassT tc2, _ ->
          let r1 = Ty.reason_of_t tc1 and r2 = Ty.reason_of_t tc2 in
          let bn1 = builtin_name r1 and bn2 = builtin_name r2 in
          if (r1 = r2 || (bn1 = bn2 && bn1 <> "")) then mt else mut
        | Ty.PolyT (_, t), _, _ -> merge r [t; u]
        | _, Ty.PolyT (_, u), _ -> merge r [t; u]
        | (Ty.FunT (r1, st1, pt1, functype)),
          (Ty.FunT (_, st2, pt2, {Ty.return_t  = rt2; _})), _ ->
          let st12 = merge r1 [st1; st2] in
          let pt12 = merge r1 [pt1; pt2] in
          let rt1 = functype.Ty.return_t in
          let rt12 = merge r1 [rt1; rt2] in
          Ty.FunT (r1, st12, pt12, {functype with Ty.return_t = rt12})
        | Ty.AnyT _, (Ty.AnyT _), _
        | Ty.AnyT _, (Ty.EmptyT _), _
        | Ty.AnyT _, (Ty.MixedT _), _
        | Ty.EmptyT _, (Ty.AnyT _), _
        | Ty.EmptyT _, (Ty.EmptyT _), _
        | Ty.EmptyT _, (Ty.MixedT _), _
        | Ty.MixedT _, (Ty.AnyT _), _
        | Ty.MixedT _, (Ty.EmptyT _), _
        | Ty.MixedT _, (Ty.MixedT _), _ ->
          mt
        | (Ty.AnyT _), _, _
        | (Ty.EmptyT _), _, _
        | (Ty.MixedT _), _, _
        | _, (Ty.AnyT _), _
        | _, (Ty.EmptyT _), _
        | _, (Ty.MixedT _), _ ->
          Ty.UnionT (inference, Ty.UnionRep.make mt mu [])
        | _ -> mut)
      | t::ts ->
        List.fold_left (fun t u -> merge r [t; u]) t ts
    and merge_unless_cached t f =
      try Hashtbl.find type_cache t
      with Not_found ->
        Hashtbl.add type_cache t t;
        let computed_type = f () in
        Hashtbl.replace type_cache t computed_type;
        computed_type
    and merge_with_cache ot =
      match ot with
      | Ty.AnnotT t ->
        merge_unless_cached ot (fun () -> merge_with_cache t)
      | Ty.OpenT (r, id) ->
        merge_unless_cached ot (fun () ->
          merge r (Flow_js.possible_types context id))
      | Ty.IntersectionT (r, rep) ->
        merge_unless_cached ot (fun () ->
          match merge r (Ty.InterRep.members rep) with
          | Ty.IntersectionT _ -> mixedT
          | t -> t)
      | Ty.UnionT (r, rep) ->
        merge_unless_cached ot (fun () ->
          match merge r (Ty.UnionRep.members rep) with
          | Ty.UnionT _ -> mixedT
          | t -> t)
      | Ty.ArrT (r, Ty.TupleAT(e, els)) ->
        merge_unless_cached ot (fun () ->
          Ty.ArrT (r, Ty.TupleAT (
            merge_with_cache e,
            List.map merge_with_cache els
          ))
        )
      | Ty.ArrT (r, Ty.ROArrayAT(e)) ->
        merge_unless_cached ot (fun () ->
          Ty.ArrT (r, Ty.ROArrayAT (
            merge_with_cache e
          ))
        )
      | Ty.ArrT (r, Ty.ArrayAT(e, els)) ->
        merge_unless_cached ot (fun () ->
          Ty.ArrT (r, Ty.ArrayAT (
            merge_with_cache e,
            match els with
            | None -> None
            | Some els -> Some (List.map merge_with_cache els)
          ))
        )
      | Ty.MaybeT (r, t) ->
        Ty.MaybeT (r, merge_with_cache t)
      | t -> t in
    merge_with_cache t

  method get_some_type_for loc : Ty.t =
    match s#get_type_for loc with
    | None -> mixedT
    | Some t -> t

  method get_type_for loc : Ty.t option =
    try
      let ot = Hashtbl.find (Context.type_table context) loc in
      Some (s#merge_possible_types ot)
    with Not_found -> None

  method get_open_expression_type loc : Ty.t =
    try Hashtbl.find (Context.type_table context) loc
    with Not_found -> emptyT

  method get_prototype_for (t: Ty.t) : Ty.t =
    match TypeMap.get t prototype_for with
    | Some pt -> pt
    | None ->
      match t with
      | Ty.FunT (_, _, proto_t, _) ->
        let pt = s#merge_possible_types proto_t in
        prototype_for <- TypeMap.add t pt prototype_for;
        pt
      | Ty.ObjT (_, {Ty.proto_t; _}) ->
        s#merge_possible_types proto_t
      | Ty.InstanceT (r, _, super, _, {Ty.methods_tmap; _ }) ->
        let super_t =
          match super with
          | Ty.ThisTypeAppT (t, _, _) ->
            (match s#merge_possible_types t with
            | Ty.ThisClassT inst_t -> inst_t
            | mt -> mt)
          | _ -> super in
        let super_proto_t = s#get_prototype_for super_t in
        let proto_t = Ty.ObjT (proto_reason, {
          Ty.flags = {Ty.frozen = true; Ty.sealed = Ty.Sealed; Ty.exact = true};
          Ty.dict_t = None;
          Ty.props_tmap = methods_tmap;
          Ty.proto_t = super_proto_t }) in
        prototype_for <- TypeMap.add t proto_t prototype_for;
        (match (Reason.desc_of_reason r) with
        | Reason.RCustom name when is_builtin_name name ->
          object_type_name_map <- TypeMap.add proto_t
            ("Js_runtime." ^ (String.uncapitalize_ascii name) ^
              "Prototype") object_type_name_map;
          builtin_prototype_name_map <- TypeMap.add proto_t
            ("Js_runtime." ^ name ^ ".prototype") builtin_prototype_name_map;
        | _ ->
          let saved_bound_this_t = current_bound_this_t in
          current_bound_this_t <- t;
          ignore (s#get_class_name_for proto_t);
          current_bound_this_t <- saved_bound_this_t);
        proto_t
      | _ -> objProtoT

  method get_static_type_for (t: Ty.t) : Ty.t =
    match TypeMap.get t static_type_for with
    | Some st -> st
    | None ->
      match t with
      | Ty.InstanceT (r, static, super, _, {Ty.class_id; _}) ->
        let static_t =
          match s#merge_possible_types static with
          | Ty.InstanceT (r, st, su, impl, inst) ->
            Ty.InstanceT (r, st, su, impl, {inst with Ty.class_id = class_id})
          | t -> t in
        let rec find_call stat sup =
          match stat with
          | Ty.InstanceT (_, _, _, _, {Ty.methods_tmap; _}) ->
            let meth_map = Context.find_props context methods_tmap in
            (match SMap.get "$call" meth_map with
            | None ->
              (match sup with
              | Ty.ThisTypeAppT (t, _, _) ->
                (match s#merge_possible_types t with
                | Ty.ThisClassT (Ty.InstanceT (_, static, super, _, _)) ->
                  find_call
                    (s#merge_possible_types static)
                    (s#merge_possible_types super);
                | _ -> ());
              | _ -> ());
            | Some prop ->
              match prop with
              | Ty.Field (ft, _) ->
                let fty = s#merge_possible_types ft in
                (match Reason.desc_of_reason r with
                | Reason.RCustom n when is_builtin_cons n -> ()
                | _ ->
                  constructor_type_for_instance <-
                    TypeMap.add t fty constructor_type_for_instance);
                instance_type_for_constructor <-
                  TypeMap.add fty t instance_type_for_constructor;
              | _ -> ());
          | _ -> () in
        find_call static_t (s#merge_possible_types super);
        static_type_for <- TypeMap.add t static_t static_type_for;
        static_t
      | _ -> mixedT

  method infer_assignment operator left right =
    match s#infer_pattern_type_opt left with
    | Some t ->
      (* If the pattern determines a type, that is the result
         of the assignment *)
       t
    | None when operator = Expression.Assignment.Assign ->
      (* If the pattern has no opinion, use the rh type *)
      s#get_expression_type right
    | _ ->
      (* Can't do a thing without the left hand type *)
      failwith "Found operator assignment to untyped pattern"

  method infer_pattern_type_opt pattern =
    match pattern with
    | (loc, Pattern.Identifier _) ->
      s#get_type_for loc
    | (_, Pattern.Assignment { Pattern.Assignment.left; right; }) ->
      Some (s#infer_assignment Expression.Assignment.Assign left right)
    (* more cases to come in the future *)
    | _ ->
      None

  method infer_pattern_type pattern default_t =
    match s#infer_pattern_type_opt pattern with
    | Some t -> t
    | None -> default_t

  method infer_literal value raw =
    let open Literal in
    match value with
    | String str -> Ty.StrT (inference, Ty.Literal str)
    | Boolean b -> Ty.BoolT (inference, Some b)
    | Null -> Ty.NullT (inference)
    | Number n -> Ty.NumT (inference, Ty.Literal (n, raw))
    | RegExp _ -> Ty.MixedT (inference, Ty.Mixed_non_null)

end
