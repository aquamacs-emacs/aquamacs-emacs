/*

  basic.c - sample module

  This module provides a simple `basic-sum' function.

  I've used the following prefixes throughout the code:
  - Sfoo: subr (function wraper)
  - Qfoo: symbol value
  - Ffoo: function value

*/

#include <emacs_module.h>

int plugin_is_GPL_compatible;

/* C function we want to expose to emacs */
static int64_t sum (int64_t a, int64_t b)
{
  return a + b;
}

/* Proper module subr that wraps the C function */
static emacs_value Fsum (emacs_env *env, int nargs, emacs_value args[])
{
  int64_t a = env->fixnum_to_int (env, args[0]);
  int64_t b = env->fixnum_to_int (env, args[1]);

  int64_t r = sum(a, b);

  return env->make_fixnum (env, r);
}

/* Binds NAME to FUN */
static void bind_function (emacs_env *env, const char *name, emacs_value Ffun)
{
  emacs_value Qfset = env->intern (env, "fset");
  emacs_value Qsym = env->intern (env, name);
  emacs_value args[] = { Qsym, Ffun };

  env->funcall (env, Qfset, 2, args);
}

/* Provide FEATURE to Emacs */
static void provide (emacs_env *env, const char *feature)
{
  emacs_value Qfeat = env->intern (env, feature);
  emacs_value Qprovide = env->intern (env, "provide");
  emacs_value args[] = { Qfeat };

  env->funcall (env, Qprovide, 1, args);
}

int emacs_module_init (struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment (ert);
  emacs_value Ssum = env->make_function (env, 2, 2, Fsum);

  bind_function (env, "basic-sum", Ssum);
  provide (env, "basic");

  return 0;
}
