(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)
open Utils_js

(* init is called from the master process at the end of the
   initialization phase of Flow. It dispatches worker processes
   to concurrently process buckets of files. The AST for each
   file, along with type information is stored in shared memory
   accessible to all workers, who only read from it. *)

let rec init merge genv (profiling, env) =
  try
    let path = Sys.getenv "js2z" in
    if Filename.is_relative path then begin
      (* Place output in a folder relative to the source root *)
      let source_root = Options.root genv.ServerEnv.options in
      let target_root = Path.concat source_root ".out" in
      Sys_utils.mkdir_no_fail (Path.to_string target_root);
      run_dumper merge genv profiling env target_root
    end else begin
      (* Place output in the folder passed to the dumper. *)
      run_dumper merge genv profiling env (Path.make path)
    end
  with Not_found -> profiling, env

and run_dumper merge genv profiling env target_root =
  print_endline "Running dumper";
  let open ServerEnv in
  let source_root = Options.root genv.ServerEnv.options in
  let profiling = Profiling_js.start_timer ~timer:"Dumper" profiling in
  let next = MultiWorker.next genv.workers (FilenameSet.elements env.files) in
  MultiWorker.call
    genv.workers
    ~job: (dump_file merge genv.options source_root target_root)
    ~neutral: ()
    ~merge: (fun _ _ -> ())
    ~next: next;
  let profiling = Profiling_js.stop_timer ~timer:"Dumper" profiling in
  (profiling, env)

and dump_file merge options source_root target_root _ file_names =
  let srlen = (String.length (Path.to_string source_root)) + 1 in
  let open Loc in
  file_names |> List.iter
    (fun filename ->
      match filename with
      | Builtins
      | ResourceFile _
      | JsonFile _ -> ()
      | LibFile fn
      | SourceFile fn ->
        let len = (String.length fn) - srlen in
        let rel_path = String.sub fn srlen len in
        let fn = Path.to_string (Path.concat target_root (rel_path ^ ".ml")) in
        let file_ast =
          try
            Parsing_service_js.get_ast_unsafe filename
          with Not_found ->
            failwith "no AST for env.files element" in
        let metadata = { (Context.metadata_of_options options) with
          Context.checked = true;
        } in
        let ctx = Type_inference_js.infer_ast ~metadata ~filename file_ast in
        merge options ctx; (* combine with dependencies *)
        (new File_dumper.file_dumper ctx)#dump fn rel_path file_ast)
