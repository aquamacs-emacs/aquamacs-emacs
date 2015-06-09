/*
  emacs_module.h - Module API
  Copyright (C) 2015 Free Software Foundation, Inc.

  This file is part of GNU Emacs.

  GNU Emacs is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  GNU Emacs is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef EMACS_MODULE_H
#define EMACS_MODULE_H

#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

/* Current environment */
typedef struct emacs_env_25 emacs_env;

/* The size of emacs_value must match EMACS_INT:
   32 bit system: 32 bits
   32 bit system with --with-wide-int: 64 bits
   64 bit system: 64 bits.

   When compiling modules, define the macro EMACS_VALUE_TYPE by the
   result of `module-emacs_value-type'. */
typedef EMACS_VALUE_TYPE emacs_value;

/* Struct passed to a module init function (emacs_module_init) */
struct emacs_runtime {
  size_t size;
  emacs_env* (*get_environment)(struct emacs_runtime *ert);
};


/* Function prototype for the module init function */
typedef int (*emacs_init_function)(struct emacs_runtime *ert);

/* Function prototype for the module Lisp functions */
typedef emacs_value (*emacs_subr)(emacs_env *env,
                                  int nargs,
                                  emacs_value args[]);
struct emacs_env_25 {
  /*
   * Structure size (for version checking)
   */

  size_t size;

  /*
   * Constants
   */
  emacs_value Qt_value;
  emacs_value Qnil_value;

  /*
   * Memory management
   */

  emacs_value (*make_global_reference)(emacs_env *env,
                                       emacs_value any_reference);

  void (*free_global_reference)(emacs_env *env,
                                emacs_value global_reference);

  /*
   * Error handling
   */

  bool (*error_check)(emacs_env *env);

  void (*clear_error)(emacs_env *env);

  bool (*get_error)(emacs_env *env,
                    emacs_value *error_symbol_out,
                    emacs_value *error_data_out);

  void (*signal_error)(emacs_env *env,
                       const char* msg,
                       emacs_value error_data);

  /*
   * Function registration
   */

  emacs_value (*make_function)(emacs_env *env,
                               int min_arity,
                               int max_arity,
                               emacs_subr function);

  emacs_value (*funcall)(emacs_env *env,
                         emacs_value function,
                         int nargs,
                         emacs_value args[]);

  emacs_value (*intern)(emacs_env *env,
                        const char *symbol_name);

  emacs_value (*intern_soft)(emacs_env *env,
                             const char *symbol_name);

  void (*bind_function) (emacs_env *env,
                         const char *name,
                         emacs_value definition);

  /*
   * Type conversion
   */

  emacs_value (*type_of)(emacs_env *env,
                         emacs_value value);

  int64_t (*fixnum_to_int)(emacs_env *env,
                           emacs_value value);

  emacs_value (*make_fixnum)(emacs_env *env,
                             int64_t value);

  double (*float_to_c_double)(emacs_env *env,
                              emacs_value value);

  emacs_value (*make_float)(emacs_env *env,
                            double value);

  bool (*copy_string_contents)(emacs_env *env,
                               emacs_value value,
                               char *buffer,
                               size_t* length_inout);

  size_t (*buffer_byte_length)(emacs_env   *env,
                               emacs_value  start,
                               emacs_value  end);
  /* Return the size in bytes of the buffer substring in the current
     buffer from START to END */

  void (*copy_buffer_substring)(emacs_env   *env,
                                emacs_value  start,
                                emacs_value  end,
                                char        *buffer,
                                size_t*      length_inout);
  /* Copy buffer string from current buffer, BEG to END (integers or
     markers), to BUFFER. On call, LENGTH_INOUT is the size in bytes
     of BUFFER; on return, it is the size in bytes of the copied
     string.

     If BUFFER is too small, signals an error. Use buffer_byte_length
     to ensure BUFFER is not too small. */

  emacs_value (*make_string)(emacs_env *env,
                             const char *contents);

  /*
   * miscellaneous
   */

  void (*message)(emacs_env *env,
                  emacs_value msg);
  /* msg must be already formatted */

  emacs_value (*symbol_value)(emacs_env *env,
                              emacs_value symbol);
};

#endif /* EMACS_MODULE_H */
