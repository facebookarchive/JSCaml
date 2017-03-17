/**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>

#include "pcre.h"

// Raises a SyntaxError exception.
static void raise_SyntaxError(const char *msg) {
  // Fetch the OCaml values of the constructor + the exception.
  static value *thrown_value = NULL;
  if (!thrown_value) {
    thrown_value = caml_named_value("thrown_value");
  }
  static value *js_syntax_error = NULL;
  if (!js_syntax_error) {
    js_syntax_error = caml_named_value("js_syntax_error");
  }

  // Create & throw the exception.
  value msg_val = caml_copy_string(msg);
  value err_obj = caml_callback(*js_syntax_error, msg_val);
  caml_raise_with_arg(*thrown_value, err_obj);
}

// Structure describing the custom type.
static struct custom_operations js_reg_exp_ops = {
  "com.facebook.js_reg_exp",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default
};

// Returns the JSRegExp pointer.
static inline JSRegExp *RegExp_val(value v) {
  return *(JSRegExp**)Data_custom_val(v);
}

// Frees heap-allocated memory.
void js_reg_exp_finalize(value v) {
  CAMLparam1(v);
  JSRegExp *reg_exp = RegExp_val(v);
  if (reg_exp != NULL) {
    jsRegExpFree(reg_exp, free);
  }
  CAMLreturn0;
}

// Creates a new regexp & return the OCaml wrapper.
CAMLprim value js_reg_exp_compile(value pattern_val, value flags_val) {
  CAMLparam2(pattern_val, flags_val);
  CAMLlocal1(result);

  // Unbox the arguments.
  const char *pattern = String_val(pattern_val);
  const char *flags = String_val(flags_val);

  // Extract options from the flags array.
  JSRegExpIgnoreCaseOption ignore_case_opt = JSRegExpDoNotIgnoreCase;
  JSRegExpMultilineOption multiline_opt = JSRegExpSingleLine;
  for (const char *flag = flags; *flag; ++flag) {
    switch (*flag) {
      case 'i':
        ignore_case_opt = JSRegExpIgnoreCase;
        break;
      case 'm':
        multiline_opt = JSRegExpMultiline;
        break;
      case 'g':
      case 'u':
      case 'y':
        break;
      default:
        raise_SyntaxError("RegExp: invalid flag");
    }
  }

  // Convert the pattern to 16-bit code points.
  size_t length = strlen(pattern);
  UChar *upattern = (UChar*)malloc(sizeof(UChar) * length);
  if (upattern == NULL) {
    caml_failwith("RegExp: cannot allocate buffer.");
  }
  for (size_t i = 0; i < length; ++i) {
    upattern[i] = pattern[i];
  }

  // Create the regexp.
  const char *err_ptr = NULL;
  JSRegExp *reg_exp = jsRegExpCompile(
      upattern,
      length,
      ignore_case_opt,
      multiline_opt,
      NULL,
      &err_ptr,
      malloc,
      free
  );
  free(upattern);
  if (reg_exp == NULL) {
    raise_SyntaxError(err_ptr);
  }

  // Allocate a pointer to the RegExp object & return it.
  result = caml_alloc_custom(&js_reg_exp_ops, sizeof(JSRegExp*), 0, 1);
  *(JSRegExp**)Data_custom_val(result) = reg_exp;
  CAMLreturn(result);
}

// Executes the regexp.
CAMLprim value js_reg_exp_exec(
    value reg_exp_val,
    value subject_val,
    value start_val)
{
  CAMLparam2(reg_exp_val, subject_val);
  CAMLlocal1(result);

  // Unbox the arguments.
  JSRegExp *reg_exp = RegExp_val(reg_exp_val);
  const char *subject = String_val(subject_val);
  int start = Long_val(start_val);

  // Convert the subject to 16 bit.
  size_t length = strlen(subject);
  UChar *usubject = (UChar*)malloc(sizeof(UChar) * length);
  if (usubject == NULL) {
    caml_failwith("RegExp: cannot allocate buffer.");
  }
  for (size_t i = 0; i < length; ++i) {
    usubject[i] = subject[i];
  }

  // Execute the RegExp. We must find a size for the offset array big
  // enough to store all the offsets of all the resulting patterns.
  int *offset = NULL, offset_length = 10, count = -1;
  do {
    if (offset_length >= (1 << 30)) {
      caml_failwith("RegExp: too many results.");
    }
    offset_length = offset_length * 2;
    offset = realloc(offset, sizeof(int) * offset_length);
    if (offset == NULL) {
      caml_failwith("RegExp: cannot allocate offsets buffer.");
    }
    count = jsRegExpExecute(
        reg_exp,
        usubject,
        length,
        start,
        offset,
        offset_length
    );
    if (count < 0) {
      free(offset);
      switch (count) {
        case JSRegExpErrorNoMatch:
          result = caml_alloc(0, 0);
          CAMLreturn(result);
        case JSRegExpErrorHitLimit: caml_failwith("RegExp: hit limit");
        case JSRegExpErrorNoMemory: caml_failwith("RegExp: no memory");
        case JSRegExpErrorInternal: caml_failwith("RegExp: internal");
        default: caml_failwith("RegExp: unknown error.");
      }
    }
  } while (count == 0);

  // Pass the results back to OCaml.
  result = caml_alloc(count * 2, 0);
  for (int i = 0; i < count * 2; ++i) {
    Store_field(result, i, Val_long(offset[i]));
  }
  free(offset);
  CAMLreturn(result);
}
