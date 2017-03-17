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
open Js_rtti
open Js_builtin

class consoleInstance = object
  inherit [objectPrototype] builtinInstance Object.prototype tag_console

  val log = define_function_0_rest "log"
    ret_dyn arg_rest_dyn
    (fun _ args ->
      args |> Array.iter (fun v -> prerr_string (!jsc_ToString v));
      prerr_char '\n';
      Undefined
    )
  method get_log = log
end

module Console = struct
  let instance = new consoleInstance
end
