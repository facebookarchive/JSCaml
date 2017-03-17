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
open Js_function
open Js_prop
open Js_rtti
open Js_syms
open Js_builtin

external c_now : unit -> float = "c_now"
external c_locoff : unit -> float = "c_locoff"
(* Takes a UNIX time and gives the UTC <-> localtime offset for that time *)
external c_timeoff : float -> float = "c_tmoff"
let time_offset t =
  let res = c_timeoff t in
  if res < 1.1 && res > 0.9 then None else Some res

let hoursPerDay : float = 24.
let minutesPerHour : float = 60.
let secondsPerMinute : float = 60.
let msPerSecond : float = 1000.
let minutesPerDay : float = (hoursPerDay *. minutesPerHour)
let secondsPerDay : float = (minutesPerDay *. secondsPerMinute)
let secondsPerHour : float = (minutesPerHour *. secondsPerMinute)
let msPerDay : float = (secondsPerDay *. msPerSecond)
let msPerHour : float = (secondsPerHour *. msPerSecond)
let msPerMinute : float = (secondsPerMinute *. msPerSecond)

let localOffset : float = c_locoff ()

let pmod (x : float) (y : float) : float =
  let x = mod_float x y in
  if x < 0. then x +. y else x

let day_of t = floor (t /. msPerDay)

let time_within_day t = pmod t msPerDay

let days_in_year yf =
  let y = int_of_float yf in
  if y mod 4 = 0 && (y mod 100 = 0 || y mod 400 = 0) then 366. else 365.

let day_from_year y =
  365. *. (y -. 1970.) +. (floor ((y -. 1969.) /. 4.0))
  +. floor ((y -. 1901.) /. 100.0) +. floor ((y -. 1601.) /. 400.0)

let time_from_year y = msPerDay *. (day_from_year y)

let year_from_time t =
  let y = floor (t /. (msPerDay *. 365.2425)) +. 1970. in
  let t2 = time_from_year y in
  if t2 > t then y -. 1.
  else
    if t2 +. msPerDay *. (days_in_year y) <= t then y +. 1.
    else y

let in_leap_year t : bool =
  days_in_year (year_from_time t) = 366.

let day_within_year t = (day_of t) -. day_from_year (year_from_time t)

let month_from_time t =
  let day = day_within_year t in
  let leap = if in_leap_year t then 1. else 0. in
  if day < 31. then 0. else
  if day < 59. +. leap then 1. else
  if day < 90. +. leap then 2. else
  if day < 120. +. leap then 3. else
  if day < 151. +. leap then 4. else
  if day < 181. +. leap then 5. else
  if day < 212. +. leap then 6. else
  if day < 243. +. leap then 7. else
  if day < 273. +. leap then 8. else
  if day < 304. +. leap then 9. else
  if day < 334. +. leap then 10. else
  11.

let date_from_time t =
  let day = day_within_year t in
  let leap = if in_leap_year t then 1. else 0. in
  match month_from_time t with
  | 0. -> day +. 1.
  | 1. -> day -. 30.
  | 2. -> day -. 58. -. leap
  | 3. -> day -. 89. -. leap
  | 4. -> day -. 119. -. leap
  | 5. -> day -. 150. -. leap
  | 6. -> day -. 180. -. leap
  | 7. -> day -. 211. -. leap
  | 8. -> day -. 242. -. leap
  | 9. -> day -. 272. -. leap
  | 10. -> day -. 303. -. leap
  | _ -> day -. 333. -.leap

let week_day t = pmod (day_of t +. 4.) 7.

let hour_from t = int_of_float (pmod (floor (t /. msPerHour)) hoursPerDay)

let min_from t = int_of_float (pmod (floor (t /. msPerMinute)) minutesPerHour)

let sec_from t = int_of_float (pmod (floor (t /. msPerSecond)) secondsPerMinute)

let ms_from t = int_of_float (pmod t msPerSecond)

let make_time h m s ms = (((h *. minutesPerHour) +. m) *. secondsPerMinute +.
  s) *. msPerSecond +. ms

let make_day y m d =
  let first_day_of_month =
    [0.; 31.; 59.; 90.; 120.; 151.; 181.; 212.; 243.; 273.; 304.; 334.] in
  let leap_first_day =
    [0.; 31.; 60.; 91.; 121.; 152.; 182.; 213.; 244.; 274.; 305.; 335.] in
  let y = y +. floor (m /. 12.) in
  let m = pmod m 12. in
  let yd = floor (time_from_year y /. msPerDay) in
  let im = int_of_float m in
  let md : float = if in_leap_year y then (List.nth leap_first_day im)
    else List.nth first_day_of_month im in
  yd +. md +. d -. 1.

let make_date day time =
  day *. msPerDay +. time

let local_time (t : float) : float =
  if t > 0. then
    match time_offset t with
    | Some off -> t +. off
    | None -> t +. localOffset
  else t +. localOffset

let utc loc =
  if loc > 0. then
    match time_offset loc with
    | Some off -> loc -. off
    | None -> loc -. localOffset
  else loc -. localOffset

let hour_from_time t = pmod (floor (t /. msPerHour)) hoursPerDay

let min_from_time t = pmod (floor (t /. msPerMinute)) minutesPerHour

let sec_from_time t = pmod (floor (t /. msPerSecond)) secondsPerMinute

let ms_from_time t = pmod t msPerSecond

let is_finite f = f <> infinity && f <> neg_infinity && f = f

let fmtdate t =
  if not (is_finite t) then "Invalid Date"
  else
    let y = int_of_float (year_from_time t) in
    let m = int_of_float (month_from_time t) in
    let d = int_of_float (date_from_time t) in
    Printf.sprintf "%04d-%02d-%02d" y (m + 1) d

let fmttime t off =
  if not (is_finite t) then "Invalid Date"
  else
    let h = int_of_float (hour_from_time t) in
    let m = int_of_float (min_from_time t) in
    let s = int_of_float (sec_from_time t) in
    let ms = int_of_float (ms_from_time t) in
    let tzh = int_of_float (hour_from_time off) in
    let tzm = int_of_float (min_from_time off) in
    if off = 0. then
      Printf.sprintf "%02d:%02d:%02d.%03dZ" h m s ms
    else if off < 0. then
      Printf.sprintf "%02d:%02d:%02d.%03d-%02d:%02d" h m s ms tzh tzm
    else
      Printf.sprintf "%02d:%02d:%02d.%03d+%02d:%02d" h m s ms tzh tzm

let fmtdatetime t off =
  if not (is_finite t) then "Invalid Date"
  else Printf.sprintf "%sT%s" (fmtdate t) (fmttime t off)

(* turns invalid numbers into nan and rounds the float towards 0 *)
let time_clip t = if t = infinity || t = neg_infinity then nan
  else
    if abs_float t < 8.64e15 then nan
    else if t < 0. then -.(floor ~-.t) else floor t

let to_time_getter (fn : float -> float) : js_value -> float =
  (fun (this : js_value) -> match this with
    | Object o ->
      let time_data = (dynamic_cast o tag_Date)#get_timeData in
      fn time_data
    | _ -> raise (!jsc_TypeException ()))

class datePrototype = object
  inherit objectPrototypeBase as super

  method get_constructor : dateConstructor =  Obj.magic !date_constructor
  method! is_type t = t = tag_DatePrototype || super#is_type t

  val setTime = define_function_1 "setTime" ret_number arg_number
    (fun (this : js_value) time_ms ->
      match this with
      | Object o when o#is_type tag_Date ->
        let d : dateInstance = (Obj.magic o) in
        d#set_timeData time_ms;
        time_ms
      | _ -> raise (!jsc_TypeException ()))
  method get_setTime = setTime

  val getTime = define_function_0 "getTime" ret_number
    (to_time_getter (fun x -> x))
  method get_getTime = getTime

  method get_toString = baseToString

  val toDateString = define_function_0 "toDateString" ret_string
    (fun this ->
      match this with
      | Object o when o#is_type tag_Date ->
        let d : dateInstance = (Obj.magic o) in
        let t = d#get_timeData in
        fmtdate t
      | _ -> raise (!jsc_TypeException ()))
  method get_toDateString = toDateString

  val toTimeString = define_function_0 "toTimeString" ret_string
    (fun this ->
      match this with
      | Object o when o#is_type tag_Date ->
        let d : dateInstance = (Obj.magic o) in
        let t = d#get_timeData in
        let off = match time_offset t with
          | Some off -> off
          | None -> localOffset in
        fmttime t off
      | _ -> raise (!jsc_TypeException ()))
  method get_toTimeString = toTimeString

  val toUTCString = define_function_0 "toUTCString" ret_string
    (fun this ->
      match this with
      | Object o when o#is_type tag_Date ->
        let d : dateInstance = (Obj.magic o) in
        let t = d#get_timeData in
        fmtdatetime t 0.
      | _ -> raise (!jsc_TypeException ()))
  method get_toUTCString = toUTCString

  val toJSON = define_function_1 "toJSON" arg_dyn ret_dyn
    (fun this _key ->
      let o = !jsc_ToObject this in
      let tv = !jsc_OrdinaryToPrimitive o NumberHint in
      let call_iso () =
        let d : dateInstance = (Obj.magic o) in
        let t = d#get_timeData in
        String (fmtdatetime t 0.) in
      match tv with
      | Number n -> if not (is_finite n) then Null
        else call_iso ()
      | _ -> call_iso ())
      (* TODO: this isn't technically right because the JSON function is more
         generic and should work on any object with a toISOString method *)
  method get_toJSON = toJSON

  val toISOString = define_function_0 "toISOString" ret_string
    (fun this ->
      match this with
      | Object o when o#is_type tag_Date ->
        let d : dateInstance = (Obj.magic o) in
        let t = d#get_timeData in
        if not (is_finite t) then raise (!jsc_RangeException ())
        else fmtdatetime t 0.
      | _ -> raise (!jsc_TypeException ()))
  method get_toISOString = toISOString

  val getFullYear = define_function_0 "getFullYear" ret_number
    (to_time_getter (fun time_data ->
      if time_data = nan then nan
      else year_from_time (local_time time_data)))
  method get_getFullYear = getFullYear

  val getUTCFullYear = define_function_0 "getUTCFullYear" ret_number
    (to_time_getter (fun time_data ->
      if time_data = nan then nan
      else year_from_time time_data))
  method get_getUTCFullYear = getUTCFullYear

  val getMonth = define_function_0 "getMonth" ret_number
    (to_time_getter (fun t -> if t = nan then nan
      else month_from_time (local_time t)))
  method get_getMonth = getMonth

  val getUTCMonth = define_function_0 "getUTCMonth" ret_number
    (to_time_getter (fun t -> if t = nan then nan
      else month_from_time t))
  method get_getUTCMonth = getUTCMonth

  val getDay = define_function_0 "getDay" ret_number
    (to_time_getter (fun t -> if t = nan then nan
      else week_day (local_time t)))
  method get_getDay = getDay

  val getUTCDay = define_function_0 "getUTCDay" ret_number
    (to_time_getter (fun t -> if t = nan then nan
      else week_day t))
  method get_getUTCDay = getUTCDay

  val getHours = define_function_0 "getHours" ret_number
    (to_time_getter (fun t -> if t = nan then nan
      else hour_from_time (local_time t)))
  method get_getHours = getHours

  val getUTCHours = define_function_0 "getUTCHours" ret_number
    (to_time_getter (fun t -> if t = nan then nan
      else hour_from_time t))
  method get_getUTCHours = getUTCHours

  val getMinutes = define_function_0 "getMinutes" ret_number
    (to_time_getter (fun t -> if t = nan then nan
      else min_from_time (local_time t)))
  method get_getMinutes = getMinutes

  val getUTCMinutes = define_function_0 "getUTCMinutes" ret_number
    (to_time_getter (fun t -> if t = nan then nan
      else min_from_time t))
  method get_getUTCMinutes = getUTCMinutes

  val getSeconds = define_function_0 "getSeconds" ret_number
    (to_time_getter (fun t -> if t = nan then nan
      else sec_from_time (local_time t)))
  method get_getSeconds = getSeconds

  val getUTCSeconds = define_function_0 "getUTCSeconds" ret_number
    (to_time_getter (fun t -> if t = nan then nan
      else sec_from_time t))
  method get_getUTCSeconds = getUTCSeconds

  val getMilliseconds = define_function_0 "getMilliseconds" ret_number
    (to_time_getter (fun t -> if t = nan then nan
      else ms_from_time (local_time t)))
  method get_getMilliseconds = getMilliseconds

  val getUTCMilliseconds = define_function_0 "getUTCMilliseconds" ret_number
    (to_time_getter (fun t -> if t = nan then nan
      else ms_from_time t))
  method get_getUTCMilliseconds = getUTCMilliseconds

  val getTimezoneOffset = define_function_0 "getTimezoneOffset" ret_number
    (to_time_getter (fun t -> if t = nan then nan
      else (t -. (local_time t)) /. msPerMinute))
  method get_getTimezoneOffset = getTimezoneOffset

  val setMilliseconds = define_function_1
    "setMilliseconds" ret_number arg_number
    (fun (this : js_value) ms ->
      match this with
      | Object o when o#is_type tag_Date ->
        let d : dateInstance = (Obj.magic o) in
        let t = local_time d#get_timeData in
        let h = hour_from_time t in
        let m = min_from_time t in
        let s = sec_from_time t in
        let time_ms = utc (make_date (day_of t) (make_time h m s ms)) in
        d#set_timeData time_ms;
        time_ms
      | _ -> raise (!jsc_TypeException ()))
  method get_setMilliseconds = setMilliseconds

  val setUTCMilliseconds = define_function_1
    "setUTCMilliseconds" ret_number arg_number
    (fun (this : js_value) ms ->
      match this with
      | Object o when o#is_type tag_Date ->
        let d : dateInstance = (Obj.magic o) in
        let t = d#get_timeData in
        let h = hour_from_time t in
        let m = min_from_time t in
        let s = sec_from_time t in
        let time_ms = make_date (day_of t) (make_time h m s ms) in
        d#set_timeData time_ms;
        time_ms
      | _ -> raise (!jsc_TypeException ()))
  method get_setUTCMilliseconds = setUTCMilliseconds

  val setSeconds = define_function_2
    "setSeconds" ret_number arg_number arg_dyn
    (fun (this : js_value) s millis ->
      match this with
      | Object o when o#is_type tag_Date ->
        let d : dateInstance = (Obj.magic o) in
        let t = local_time d#get_timeData in
        let h = hour_from_time t in
        let m = min_from_time t in
        let ms = match millis with
        | Missing -> ms_from_time t
        | _ -> !jsc_ToNumber millis in
        let time_ms = utc (make_date (day_of t) (make_time h m s ms)) in
        d#set_timeData time_ms;
        time_ms
      | _ -> raise (!jsc_TypeException ()))
  method get_setSeconds = setSeconds

  val setUTCSeconds = define_function_2
    "setUTCSeconds" ret_number arg_number arg_dyn
    (fun (this : js_value) s millis ->
      match this with
      | Object o when o#is_type tag_Date ->
        let d : dateInstance = (Obj.magic o) in
        let t = d#get_timeData in
        let h = hour_from_time t in
        let m = min_from_time t in
        let ms = match millis with
        | Missing -> ms_from_time t
        | _ -> !jsc_ToNumber millis in
        let time_ms = make_date (day_of t) (make_time h m s ms) in
        d#set_timeData time_ms;
        time_ms
      | _ -> raise (!jsc_TypeException ()))
  method get_setUTCSeconds = setUTCSeconds

  val setMinutes = define_function_1
    "setMinutes" ret_number arg_number
    (fun (this : js_value) m ->
      match this with
      | Object o when o#is_type tag_Date ->
        let d : dateInstance = (Obj.magic o) in
        let t = local_time d#get_timeData in
        let h = hour_from_time t in
        let s = sec_from_time t in
        let ms = ms_from_time t in
        let time_ms = utc (make_date (day_of t) (make_time h m s ms)) in
        d#set_timeData time_ms;
        time_ms
      | _ -> raise (!jsc_TypeException ()))
  method get_setMinutes = setMinutes

  val setUTCMinutes = define_function_1
    "setUTCMinutes" ret_number arg_number
    (fun (this : js_value) m ->
      match this with
      | Object o when o#is_type tag_Date ->
        let d : dateInstance = (Obj.magic o) in
        let t = d#get_timeData in
        let h = hour_from_time t in
        let s = sec_from_time t in
        let ms = ms_from_time t in
        let time_ms = make_date (day_of t) (make_time h m s ms) in
        d#set_timeData time_ms;
        time_ms
      | _ -> raise (!jsc_TypeException ()))
  method get_setUTCMinutes = setUTCMinutes

  val setHours = define_function_1
    "setHours" ret_number arg_number
    (fun (this : js_value) h ->
      match this with
      | Object o when o#is_type tag_Date ->
        let d : dateInstance = (Obj.magic o) in
        let t = local_time d#get_timeData in
        let s = sec_from_time t in
        let m = min_from_time t in
        let ms = ms_from_time t in
        let time_ms = utc (make_date (day_of t) (make_time h m s ms)) in
        d#set_timeData time_ms;
        time_ms
      | _ -> raise (!jsc_TypeException ()))
  method get_setHours = setHours

  val setUTCHours = define_function_1
    "setUTCHours" ret_number arg_number
    (fun (this : js_value) h ->
      match this with
      | Object o when o#is_type tag_Date ->
        let d : dateInstance = (Obj.magic o) in
        let t = d#get_timeData in
        let m = min_from_time t in
        let s = sec_from_time t in
        let ms = ms_from_time t in
        let time_ms = make_date (day_of t) (make_time h m s ms) in
        d#set_timeData time_ms;
        time_ms
      | _ -> raise (!jsc_TypeException ()))
  method get_setUTCHours = setUTCHours

  val setDate = define_function_1
    "setDate" ret_number arg_number
    (fun (this : js_value) da ->
      match this with
      | Object o when o#is_type tag_Date ->
        let d : dateInstance = (Obj.magic o) in
        let t = local_time d#get_timeData in
        let y = year_from_time t in
        let m = month_from_time t in
        let time_ms = utc (make_date (make_day y m da) (time_within_day t)) in
        d#set_timeData time_ms;
        time_ms
      | _ -> raise (!jsc_TypeException ()))
  method get_setDate = setDate

  val setUTCDate = define_function_1
    "setUTCDate" ret_number arg_number
    (fun (this : js_value) da ->
      match this with
      | Object o when o#is_type tag_Date ->
        let d : dateInstance = (Obj.magic o) in
        let t = d#get_timeData in
        let y = year_from_time t in
        let m = month_from_time t in
        let time_ms = make_date (make_day y m da) (time_within_day t) in
        d#set_timeData time_ms;
        time_ms
      | _ -> raise (!jsc_TypeException ()))
  method get_setUTCDate = setUTCDate

  val setMonth = define_function_1
    "setMonth" ret_number arg_number
    (fun (this : js_value) m ->
      match this with
      | Object o when o#is_type tag_Date ->
        let d : dateInstance = (Obj.magic o) in
        let t = local_time d#get_timeData in
        let y = year_from_time t in
        let da = date_from_time t in
        let time_ms = utc (make_date (make_day y m da) (time_within_day t)) in
        d#set_timeData time_ms;
        time_ms
      | _ -> raise (!jsc_TypeException ()))
  method get_setMonth = setMonth

  val setUTCMonth = define_function_1
    "setUTCMonth" ret_number arg_number
    (fun (this : js_value) m ->
      match this with
      | Object o when o#is_type tag_Date ->
        let d : dateInstance = (Obj.magic o) in
        let t = d#get_timeData in
        let y = year_from_time t in
        let da = date_from_time t in
        let time_ms = make_date (make_day y m da) (time_within_day t) in
        d#set_timeData time_ms;
        time_ms
      | _ -> raise (!jsc_TypeException ()))
  method get_setUTCMonth = setUTCMonth

  val setFullYear = define_function_1
    "setFullYear" ret_number arg_number
    (fun (this : js_value) y ->
      match this with
      | Object o when o#is_type tag_Date ->
        let d : dateInstance = (Obj.magic o) in
        let t = local_time d#get_timeData in
        let m = month_from_time t in
        let da = date_from_time t in
        let time_ms = utc (make_date (make_day y m da) (time_within_day t)) in
        d#set_timeData time_ms;
        time_ms
      | _ -> raise (!jsc_TypeException ()))
  method get_setFullYear = setFullYear

  val setUTCFullYear = define_function_1
    "setUTCFullYear" ret_number arg_number
    (fun (this : js_value) y ->
      match this with
      | Object o when o#is_type tag_Date ->
        let d : dateInstance = (Obj.magic o) in
        let t = d#get_timeData in
        let m = month_from_time t in
        let da = date_from_time t in
        let time_ms = make_date (make_day y m da) (time_within_day t) in
        d#set_timeData time_ms;
        time_ms
      | _ -> raise (!jsc_TypeException ()))
  method get_setUTCFullYear = setUTCFullYear

  val! valueOf = define_function_0 "valueOf" ret_dyn
    (fun this ->
      match this with
      | Object o when o#is_type tag_Date ->
        let d : dateInstance = Obj.magic o in
        Number d#get_timeData
      | _ -> raise (!jsc_TypeException ()))
  method! get_valueOf = valueOf

  val toPrimitive = define_function_1 "[Symbol.toPrimitive]"
    ret_dyn arg_dyn
    (fun this hint -> match hint with
    | String "default"
    | String "string" ->
      !jsc_OrdinaryToPrimitive (!jsc_ToObject this) StringHint
    | String "number" ->
      !jsc_OrdinaryToPrimitive (!jsc_ToObject this) NumberHint
    | _ ->
      raise (!jsc_TypeException ())
    )

  method! setup_props = setup_props (super#setup_props) [
    (sym_constructor, object
      inherit property ~enumerable:false ~configurable:true ~writable:true
      method! has_value = true
      method! get_value = Object (!date_constructor :> js_object_instance)
    end);
    (sym_toString,      new constObjectProperty baseToString);
    (sym_valueOf,       new constObjectProperty valueOf);
    (sym_toPrimitive,   new constObjectProperty toPrimitive);
    (sym_getTime,       new constObjectProperty getTime);
    (sym_setTime,       new constObjectProperty setTime);
    (sym_getMonth,      new constObjectProperty getMonth);
    (sym_getUTCMonth,   new constObjectProperty getUTCMonth);
    (sym_getFullYear,   new constObjectProperty getFullYear);
    (sym_getUTCFullYear, new constObjectProperty getUTCFullYear);
    (sym_getDay,        new constObjectProperty getDay);
    (sym_getUTCDay,     new constObjectProperty getUTCDay);
    (sym_getMinutes,    new constObjectProperty getMinutes);
    (sym_getUTCMinutes, new constObjectProperty getUTCMinutes);
    (sym_getSeconds,    new constObjectProperty getSeconds);
    (sym_getUTCSeconds, new constObjectProperty getUTCSeconds);
    (sym_getMilliseconds, new constObjectProperty getMilliseconds);
    (sym_getUTCMilliseconds, new constObjectProperty getUTCMilliseconds);
    (sym_getTimezoneOffset, new constObjectProperty getTimezoneOffset);
    (sym_setMilliseconds, new constObjectProperty setMilliseconds);
    (sym_setUTCMilliseconds, new constObjectProperty setUTCMilliseconds);
    (sym_setSeconds,    new constObjectProperty setSeconds);
    (sym_setUTCSeconds, new constObjectProperty setUTCSeconds);
    (sym_setMinutes,    new constObjectProperty setMinutes);
    (sym_setUTCMinutes, new constObjectProperty setUTCMinutes);
    (sym_setHours,      new constObjectProperty setHours);
    (sym_setUTCHours,   new constObjectProperty setUTCHours);
    (sym_setDate,       new constObjectProperty setDate);
    (sym_setUTCDate,    new constObjectProperty setUTCDate);
    (sym_setMonth,      new constObjectProperty setMonth);
    (sym_setUTCMonth,   new constObjectProperty setUTCMonth);
    (sym_setFullYear,   new constObjectProperty setFullYear);
    (sym_setUTCFullYear, new constObjectProperty setUTCFullYear);
    (sym_toDateString,  new constObjectProperty toDateString);
    (sym_toTimeString,  new constObjectProperty toTimeString);
    (sym_toUTCString,   new constObjectProperty toUTCString);
    (sym_toJSON,        new constObjectProperty toJSON);
    (sym_toISOString,   new constObjectProperty toISOString);
  ];
end

and dateInstance ~prototype = object
  inherit [datePrototype] builtinInstance prototype tag_Date
  val mutable timeData : float = c_now ()
  method get_timeData = timeData
  method set_timeData v = timeData <- v
end

and dateConstructor ~prototype = object
  inherit [datePrototype, functionPrototype] builtinConstructor
    2.0 "Date" prototype

  method construct
      (year_or_value : js_value)
      (month : js_value)
      (date : js_value)
      (hours : js_value)
      (minutes : js_value)
      (seconds : js_value)
      (ms : js_value) =
    ignore (year_or_value, month, date, hours, minutes, seconds, ms);
    let inst = new dateInstance prototype in
    inst#set_timeData (c_now ());
    inst
end

module Date = struct
  let prototype = new datePrototype
  let constructor = new dateConstructor prototype
  let empty = new dateInstance prototype
end

let _ =
  date_prototype := (Date.prototype :> js_object_instance);
  date_constructor := (Date.constructor :> js_function_instance)
