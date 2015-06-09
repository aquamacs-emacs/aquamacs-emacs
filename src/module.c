/*
  module.c - Module loading and runtime implementation
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

#include <config.h>
#include "lisp.h"
#include "character.h"
#include "buffer.h"

/* see comment in emacs_module.h at emacs_value for this define */
#define EMACS_VALUE_TYPE EMACS_INT
#include "emacs_module.h"

#include <ltdl.h>

/* internal functions */
void                         syms_of_module         (void);
static struct emacs_runtime* module_get_runtime     (void);
static emacs_env*            module_get_environment (struct emacs_runtime *ert);

/* emacs_module.h emacs_env_* functions; same order as there */
/* FIXME: make_global_reference */
/* FIXME: free_global_reference */
/* FIXME: error_check */
/* FIXME: clear_error */
/* FIXME: get_error */
static void                  module_signal_error    (emacs_env *env,
                                                     const char* msg,
                                                     emacs_value error_data);
static emacs_value           module_make_function   (emacs_env *env,
                                                     int min_arity,
                                                     int max_arity,
                                                     emacs_subr subr);
static emacs_value           module_funcall         (emacs_env *env,
                                                     emacs_value fun,
                                                     int nargs,
                                                     emacs_value args[]);
static emacs_value           module_intern          (emacs_env *env,
                                                     const char *name);
static emacs_value           module_intern_soft     (emacs_env *env,
                                                     const char *name);
static void                  module_bind_function   (emacs_env *env,
                                                     const char *name,
                                                     emacs_value definition);
/* FIXME: type_of */
static int64_t               module_fixnum_to_int   (emacs_env *env,
                                                     emacs_value n);
static emacs_value           module_make_fixnum     (emacs_env *env,
                                                     int64_t n);
/* FIXME: float_to_c_double */
/* FIXME: make_float */
/* FIXME: copy_string_contents */
static size_t                module_buffer_byte_length (emacs_env *env,
                                                        emacs_value start,
                                                        emacs_value end);

static void                  module_copy_buffer_substring (emacs_env   *env,
                                                           emacs_value  start,
                                                           emacs_value  end,
                                                           char        *buffer,
                                                           size_t      *length_inout);
static emacs_value           module_make_string     (emacs_env *env,
                                                     const char *contents);
static void                  module_message         (emacs_env *env,
                                                     emacs_value msg);
static emacs_value           module_symbol_value    (emacs_env *env,
                                                     emacs_value symbol);


static struct emacs_runtime* module_get_runtime (void)
{
  /* FIXME: why do we need module_get_runtime, as opposed to just module_get_environment? */
  struct emacs_runtime *ert = xzalloc (sizeof *ert);

  ert->size = sizeof *ert;
  ert->get_environment = module_get_environment;

  return ert;
}

static emacs_env* module_get_environment (struct emacs_runtime *ert)
{
  /* FIXME: error if not on main emacs thread? */

  emacs_env *env = xzalloc (sizeof *env);

  env->size                  = sizeof *env;
  env->Qt_value              = (emacs_value) Qt;
  env->Qnil_value            = (emacs_value) Qnil;
  /* FIXME: make_global_reference */
  /* FIXME: free_global_reference */
  /* FIXME: error_check */
  /* FIXME: clear_error */
  /* FIXME: get_error */
  env->signal_error          = module_signal_error;
  env->make_function         = module_make_function;
  env->funcall               = module_funcall;
  env->intern                = module_intern;
  env->intern_soft           = module_intern_soft;
  env->bind_function         = module_bind_function;
  env->fixnum_to_int         = module_fixnum_to_int;
  env->make_fixnum           = module_make_fixnum;
  /* FIXME: copy_string_contents */
  env->buffer_byte_length    = module_buffer_byte_length;
  env->copy_buffer_substring = module_copy_buffer_substring;
  env->make_string           = module_make_string;
  env->message               = module_message;
  env->symbol_value          = module_symbol_value;

  return env;
}

static emacs_value module_make_fixnum (emacs_env *env, int64_t n)
{
  return (emacs_value) make_number (n);
}

static int64_t module_fixnum_to_int (emacs_env *env, emacs_value n)
{
  return (int64_t) XINT ((Lisp_Object) n);
}

static emacs_value module_intern (emacs_env *env, const char *name)
{
  return (emacs_value) intern (name);
}

static emacs_value module_intern_soft (emacs_env *env, const char *name)
{
  register ptrdiff_t len = strlen (name);
  register Lisp_Object tem = oblookup (Vobarray, name, len, len);

  if (INTEGERP (tem))
    return (emacs_value) Qnil;
  else
    return (emacs_value) tem;
}

static void module_bind_function (emacs_env *env,
                                  const char *name,
                                  emacs_value definition)
{
  Lisp_Object symbol = intern (name);
  set_symbol_function (symbol, (Lisp_Object) definition);
}

static void module_signal_error (emacs_env *env,
                                 const char* msg,
                                 emacs_value error_data)
{
  signal_error (msg, (Lisp_Object) (error_data));
}

static emacs_value module_make_function (emacs_env *env,
                                         int min_arity,
                                         int max_arity,
                                         emacs_subr subr)
{
  /*
    (function
     (lambda
      (&rest arglist)
      (module-call
       envptr
       subrptr
       arglist)))
  */
  /* FIXME: allow for doc string and interactive */
  Lisp_Object Qrest = intern ("&rest");
  Lisp_Object Qarglist = intern ("arglist");
  Lisp_Object Qmodule_call = intern ("module-call");
  Lisp_Object envptr = make_save_ptr ((void*) env);
  Lisp_Object subrptr = make_save_ptr ((void*) subr);

  Lisp_Object form = list2 (Qfunction,
                            list3 (Qlambda,
                                   list2 (Qrest, Qarglist),
                                   list4 (Qmodule_call,
                                          envptr,
                                          subrptr,
                                          Qarglist)));

  struct gcpro gcpro1;
  GCPRO1 (Qform);
  Lisp_Object ret = Feval (form, Qnil);
  UNGCPRO;

  return (emacs_value) ret;
}

static emacs_value module_funcall (emacs_env *env,
                                   emacs_value fun,
                                   int nargs,
                                   emacs_value args[])
{
  /*
   *  Make a new Lisp_Object array starting with the function as the
   *  first arg, because that's what Ffuncall takes
   */
  int i;
  Lisp_Object *newargs = xmalloc ((nargs+1) * sizeof (*newargs));

  newargs[0] = (Lisp_Object) fun;
  for (i = 0; i < nargs; i++)
    newargs[1 + i] = (Lisp_Object) args[i];

  struct gcpro gcpro1;
  GCPRO1 (newargs[0]);
  Lisp_Object ret = Ffuncall (nargs+1, newargs);
  UNGCPRO;

  xfree (newargs);
  return (emacs_value) ret;
}

static size_t module_buffer_byte_length (emacs_env *env,
                                         emacs_value start,
                                         emacs_value end)
{
  Lisp_Object start_1 = (Lisp_Object)start;
  Lisp_Object end_1   = (Lisp_Object)end;

  validate_region (&start_1, &end_1);

  {
    ptrdiff_t start_byte = CHAR_TO_BYTE (XINT (start_1));
    ptrdiff_t end_byte   = CHAR_TO_BYTE (XINT (end_1));

    return (size_t) end_byte - start_byte;
  }
}

static void module_copy_buffer_substring (emacs_env   *env,
                                          emacs_value  start,
                                          emacs_value  end,
                                          char        *buffer,
                                          size_t      *length_inout)
{
  /* Copied from editfns.c "buffer-substring-no-properties" and make_buffer_string_both */
  Lisp_Object start_1 = (Lisp_Object)start;
  Lisp_Object end_1   = (Lisp_Object)end;

  validate_region (&start_1, &end_1);

  {
    ptrdiff_t start      = XINT (start_1);
    ptrdiff_t start_byte = CHAR_TO_BYTE (start);
    ptrdiff_t end        = XINT (end_1);
    ptrdiff_t end_byte   = CHAR_TO_BYTE (end);
    ptrdiff_t beg0, end0, beg1, end1;
    size_t    size;

    if (end_byte - start_byte > *length_inout)
      {
        /* buffer too small */
        /* FIXME: could copy less than requested, but that's
           complicated for multi-byte characters */
        signal_error ("module_copy_buffer_substring: buffer too small", Qnil);
      }

  if (start_byte < GPT_BYTE && GPT_BYTE < end_byte)
    {
      /* Two regions, before and after the gap.  */
      beg0 = start_byte;
      end0 = GPT_BYTE;
      beg1 = GPT_BYTE + GAP_SIZE - BEG_BYTE;
      end1 = end_byte + GAP_SIZE - BEG_BYTE;
    }
  else
    {
      /* One region, before the gap.  */
      beg0 = start_byte;
      end0 = end_byte;
      beg1 = -1;
      end1 = -1;
    }

    size = end0 - beg0;

    /* FIXME: need to decode? See external process stuff. */

    /* BYTE_POS_ADDR handles one region after the gap */
    memcpy (buffer, BYTE_POS_ADDR (beg0), size);
    if (beg1 != -1)
      memcpy (buffer + size, BEG_ADDR + beg1, end1 - beg1);
  }
}

static emacs_value module_make_string (emacs_env *env, const char *contents)
{
  return (emacs_value) make_string (contents, strlen (contents));
}

static void module_message (emacs_env *env,
                            emacs_value msg)
{
  message3 ((Lisp_Object) msg);
}

static emacs_value module_symbol_value (emacs_env *env,
                                        emacs_value symbol)
{
  Lisp_Object val= find_symbol_value ((Lisp_Object) symbol);
  if (!EQ (val, Qunbound))
    return (emacs_value) val;

  xsignal1 (Qvoid_variable, (Lisp_Object) symbol);
}

DEFUN ("module-call", Fmodule_call, Smodule_call, 3, 3, 0,
       doc: "Call a module function")
  (Lisp_Object envptr, Lisp_Object subrptr, Lisp_Object arglist)
{
  int len = XINT (Flength (arglist));
  emacs_value *args = xzalloc (len * sizeof (*args));
  int i;

  for (i = 0; i < len; i++)
    {
      args[i] = (emacs_value) XCAR (arglist);
      arglist = XCDR (arglist);
    }

  emacs_env *env = (emacs_env*) XSAVE_POINTER (envptr, 0);
  emacs_subr subr = (emacs_subr) XSAVE_POINTER (subrptr, 0);
  emacs_value ret = subr (env, len, args);
  return (Lisp_Object) ret;
}

static int lt_init_done = 0;

EXFUN (Fmodule_load, 1);
DEFUN ("module-load", Fmodule_load, Smodule_load, 1, 1, 0,
       doc: /* Load module FILE.  */)
  (Lisp_Object file)
{
  lt_dlhandle handle;
  emacs_init_function module_init;
  void *gpl_sym;
  Lisp_Object doc_name, args[2];

  /* init libtool once per emacs process */
  if (!lt_init_done)
    {
      int ret = lt_dlinit ();
      if (ret)
        {
          const char* s = lt_dlerror ();
          error ("ltdl init fail: %s", s);
        }
      lt_init_done = 1;
    }

  /* FIXME: check for libltdl, load it if available; don't require
     --with-ltdl at configure time. See image.c for example. */

  CHECK_STRING (file);
  handle = lt_dlopen (SDATA (file));
  if (!handle)
    error ("Cannot load file %s : %s", SDATA (file), lt_dlerror());

  gpl_sym = lt_dlsym (handle, "plugin_is_GPL_compatible");
  if (!gpl_sym)
    error ("Module %s is not GPL compatible", SDATA (file));

  module_init = (emacs_init_function) lt_dlsym (handle, "emacs_module_init");
  if (!module_init)
    error ("Module %s does not have an init function.", SDATA (file));


  int r = module_init (module_get_runtime ());

  /* Errors are reported by calling env->signal_error. FIXME: so why does module_init return anything? */
  return Qt;
}

EXFUN (Fmodule_unsafe_unload, 1);
DEFUN ("module-unsafe-unload", Fmodule_unsafe_unload, Smodule_unsafe_unload, 1, 1, 0,
       doc: /* Unload module FILE; does not undefine any functions defined by the module.
This permits re-compiling and re-loading while developing the module,
but is otherwise not recommended.  */)
  (Lisp_Object file)
{
  lt_dlhandle handle;

  if (!lt_init_done)
    {
      error ("no module loaded");
    }

  CHECK_STRING (file);
  handle = lt_dlopen (SDATA (file));
  if (!handle)
    error ("file not loaded %s : %s", SDATA (file), lt_dlerror());

  if (lt_dlclose (handle))
    error ("Module %s not unloaded: %s", SDATA (file), lt_dlerror());

  return Qt;
}

EXFUN (Fmodule_emacs_value_type, 0);
DEFUN ("module-emacs_value-type", Fmodule_emacs_value_type, Smodule_emacs_value_type, 0, 0, 0,
       doc: /* Return a string specifying the type for emacs_value in emacs_modules.h.  */)
  ()
{
  if (sizeof (EMACS_INT) == 4) /* 4 bytes == 32 bits */
    return make_string ("uint32_t", 8);
  else
    return make_string ("uint64_t", 8);
}

void syms_of_module (void)
{
  defsubr (&Smodule_call);
  defsubr (&Smodule_load);
  defsubr (&Smodule_unsafe_unload);
  defsubr (&Smodule_emacs_value_type);
}
