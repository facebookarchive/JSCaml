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
open Js_util
open Js_rtti
open Js_builtin
open Js_syms
open Js_prop

let e       = 2.7182818284590452353602874713526;;
let ln10    = 2.3025850929940456840179914546843;;
let ln2     = 0.6931471805599453094172321214581;;
let log10e  = 0.4342944819032518276511289189166;;
let log2e   = 1.4426950408889634073599246810018;;
let pi      = 3.1415926535897932384626433832795;;
let sqrt1_2 = 0.7071067811865475244008443621048;;
let sqrt2   = 1.4142135623730950488016887242096;;

class mathInstance ~prototype = object
  inherit [objectPrototype] builtinInstance prototype tag_Object as super

  val m_abs = define_function_1 "abs"
    ret_number arg_number
    (fun (_ : js_value) -> abs_float)
  method get_abs = m_abs

  val m_acos = define_function_1 "acos"
    ret_number arg_number
    (fun (_ : js_value) -> acos)
  method get_acos = m_acos

  val m_acosh = define_function_1 "acosh"
    ret_number arg_number
    (fun (_ : js_value) x -> match x with
    | x when x <> x -> nan
    | x when x < 1. -> nan
    | 1. -> 0.
    | x when x = infinity -> infinity
    | x -> log (x +. sqrt (x *. x -. 1.))
    )
  method get_acosh = m_acosh

  val m_asin = define_function_1 "asin"
    ret_number arg_number
    (fun (_ : js_value) -> asin)
  method get_asin = m_asin

  val m_asinh = define_function_1 "asinh"
    ret_number arg_number
    (fun (_ : js_value) x -> match x with
    | 0. -> x
    | x when x <> x -> nan
    | x when x = infinity -> infinity
    | x when x = neg_infinity -> neg_infinity
    | x -> log (x +. sqrt (x *. x +. 1.))
    )
  method get_asinh = m_asinh

  val m_atan = define_function_1 "atan"
    ret_number arg_number
    (fun (_ : js_value) -> atan)
  method get_atan = m_atan

  val m_atan2 = define_function_2 "atan2"
    ret_number arg_number arg_number
    (fun (_ : js_value) -> atan2)
  method get_atan2 = m_atan2

  val m_atanh = define_function_1 "atanh"
    ret_number arg_number
    (fun (_ : js_value) x -> match x with
    | x when x <> x -> nan
    | x when x = infinity -> nan
    | x when x = neg_infinity -> nan
    | 0. -> x
    | x -> (log (1. +. x) -. log(1. -. x)) /. 2.
    )
  method get_atanh = m_atanh

  val m_ceil = define_function_1 "ceil"
    ret_number arg_number
    (fun (_ : js_value) -> ceil)
  method get_ceil = m_ceil

  val m_clz32 = define_function_1 "clz32"
    ret_number arg_number
    (fun (_ : js_value) x ->
    let num = to_uint32 x in
    let rec clz = function
      | 0 when num land 1 = 1-> 31
      | 0 -> 32
      | i when ((1 lsl i) land num) <> 0 -> 31 - i
      | i -> clz (i - 1)
    in float_of_int (clz 31)
  )
  method get_clz32 = m_clz32

  val m_cos = define_function_1 "cos"
    ret_number arg_number
    (fun (_ : js_value) -> cos)
  method get_cos = m_cos

  val m_cosh = define_function_1 "cosh"
    ret_number arg_number
    (fun (_ : js_value) -> cosh)
  method get_cosh = m_cosh

  val m_exp = define_function_1 "exp"
    ret_number arg_number
    (fun (_ : js_value) x -> e ** x)
  method get_exp = m_exp

  val m_expm1 = define_function_1 "expm1"
    ret_number arg_number
    (fun (_ : js_value) x -> match x with
    | x when x <> x -> nan
    | x when x = infinity -> infinity
    | x when x = neg_infinity -> -1.
    | 0. -> x
    | x -> e ** x -. 1.
    )
  method get_expm1 = m_expm1

  val m_floor = define_function_1 "floor"
    ret_number arg_number
    (fun (_ : js_value) -> floor)
  method get_floor = m_floor

  val m_log = define_function_1 "log"
    ret_number arg_number
    (fun (_ : js_value) -> log)
  method get_log = m_log

  val m_log10 = define_function_1 "log10"
    ret_number arg_number
    (fun (_ : js_value) -> log10)
  method get_log10 = m_log10

  val m_log1p = define_function_1 "log1p"
    ret_number arg_number
    (fun (_ : js_value) x -> match x with
    | 0. -> x
    | x -> log (1. +. x)
    )
  method get_log1p = m_log1p

  val m_log2 = define_function_1 "log2"
    ret_number arg_number
    (fun (_ : js_value) x ->
    log x /. log 2.0
  )
  method get_log2 = m_log2

  val m_pow = define_function_2 "pow"
    ret_number arg_number arg_number
    (fun (_ : js_value) x y ->
    if abs_float x = 1. then
      match y with
      | _ when x = -1. -> nan
      | y when y = infinity ->  nan
      | y when y = neg_infinity -> nan
      | _ -> 1.
    else
      x ** y
    )
  method get_pow = m_pow

  val m_random = define_function_0 "random"
    ret_number
    (fun (_ : js_value) -> Random.float 1.)
  method get_random = m_random

  val m_round = define_function_1 "round"
    ret_number arg_number
    (fun (_ : js_value)x -> match x with
    | x when x <> x -> nan
    | x when x = infinity -> infinity
    | x when x = neg_infinity -> neg_infinity
    | x when x > 0. || x < -0.5 -> floor (x +. 0.5)
    | 0. -> x
    | _ -> -0.
    )
  method get_round = m_round

  val m_sign = define_function_1 "sign"
    ret_number arg_number
    (fun (_ : js_value) x -> match x with
    | x when x <> x -> nan
    | x when x > 0. -> +1.
    | x when x < 0. -> -1.
    | _ -> x
    )
  method get_sign = m_sign

  val m_sin = define_function_1 "sin"
    ret_number arg_number
    (fun (_ : js_value) -> sin)
  method get_sin = m_sin

  val m_sinh = define_function_1 "sinh"
    ret_number arg_number
    (fun (_ : js_value) -> sinh)
  method get_sinh = m_sinh

  val m_sqrt = define_function_1 "sqrt"
    ret_number arg_number
    (fun (_ : js_value) -> sqrt)
  method get_sqrt = m_sqrt

  val m_tan = define_function_1 "tan"
    ret_number arg_number
    (fun (_ : js_value) -> tan)
  method get_tan = m_tan

  val m_tanh = define_function_1 "tanh"
    ret_number arg_number
    (fun (_ : js_value) -> tanh)
  method get_tanh = m_tanh

  val m_trunc = define_function_1 "trunc"
    ret_number arg_number
    (fun (_ : js_value) x -> match x with
    | x when x <> x -> nan
    | x when x = infinity -> infinity
    | x when x = neg_infinity -> neg_infinity
    | x when 0. < x && x < 1. -> 0.
    | x when -1. < x && x < 0. -> -0.
    | 0. -> x
    | x -> float_of_int (int_of_float x)
    )
  method get_trunc = m_trunc

  method get_E       = e
  method get_LN10    = ln10
  method get_LN2     = ln2
  method get_LOG10E  = log10e
  method get_LOG2E   = log2e
  method get_PI      = pi
  method get_SQRT1_2 = sqrt1_2
  method get_SQRT2   = sqrt2

  method! setup_props = setup_props (super#setup_props) [
    (sym_abs,         new builtinObjectProperty m_abs);
    (sym_acos,        new builtinObjectProperty m_acos);
    (sym_acosh,       new builtinObjectProperty m_acos);
    (sym_asin,        new builtinObjectProperty m_asin);
    (sym_asinh,       new builtinObjectProperty m_asin);
    (sym_atan,        new builtinObjectProperty m_atan);
    (sym_atanh,       new builtinObjectProperty m_atan);
    (sym_atan,        new builtinObjectProperty m_atan);
    (sym_ceil,        new builtinObjectProperty m_ceil);
    (sym_clz32,       new builtinObjectProperty m_clz32);
    (sym_cos,         new builtinObjectProperty m_cos);
    (sym_cosh,        new builtinObjectProperty m_cosh);
    (sym_exp,         new builtinObjectProperty m_exp);
    (sym_expm1,       new builtinObjectProperty m_expm1);
    (sym_floor,       new builtinObjectProperty m_floor);
    (sym_log,         new builtinObjectProperty m_log);
    (sym_log1p,       new builtinObjectProperty m_log1p);
    (sym_log10,       new builtinObjectProperty m_log10);
    (sym_log2,        new builtinObjectProperty m_log2);
    (sym_pow,         new builtinObjectProperty m_pow);
    (sym_random,      new builtinObjectProperty m_random);
    (sym_round,       new builtinObjectProperty m_round);
    (sym_sign,        new builtinObjectProperty m_sign);
    (sym_sin,         new builtinObjectProperty m_sin);
    (sym_sinh,        new builtinObjectProperty m_sinh);
    (sym_sqrt,        new builtinObjectProperty m_sqrt);
    (sym_tan,         new builtinObjectProperty m_tan);
    (sym_tanh,        new builtinObjectProperty m_tanh);
    (sym_trunc,       new builtinObjectProperty m_trunc);
    (sym_E,           new builtinNumberProperty e);
    (sym_LN10,        new builtinNumberProperty ln10);
    (sym_LN2,         new builtinNumberProperty ln2);
    (sym_LOG10E,      new builtinNumberProperty log10e);
    (sym_LOG2E,       new builtinNumberProperty log2e);
    (sym_PI,          new builtinNumberProperty pi);
    (sym_SQRT1_2,     new builtinNumberProperty sqrt1_2);
    (sym_SQRT2,       new builtinNumberProperty sqrt2);
    (sym_toStringTag, new constStringProperty "Math")
  ]
end

module Math = struct
  let instance = new mathInstance Object.prototype
end
