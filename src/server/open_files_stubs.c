/* Limits on the number of files the process may open (RLIMIT_NOFILE), for
   Ocsigen.Open_files. */

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#ifdef _WIN32

/* Windows has no such limits. */

CAMLprim value ocsigen_open_files_limits(value unit)
{
  (void)unit;
  return Val_none;
}

CAMLprim value ocsigen_open_files_raise(value wanted)
{
  (void)wanted;
  return Val_none;
}

#else

#include <errno.h>
#include <limits.h>
#include <sys/resource.h>
#include <caml/unixsupport.h>

static value of_rlim(rlim_t r)
{
  return Val_long(r == RLIM_INFINITY || r > (rlim_t)Max_long ? Max_long
                                                              : (intnat)r);
}

static struct rlimit current_limits(void)
{
  struct rlimit rl;
  if (getrlimit(RLIMIT_NOFILE, &rl) != 0) unix_error(errno, "getrlimit", Nothing);
  return rl;
}

/* [Some (soft, hard)]. */
static value alloc_limits(struct rlimit rl)
{
  CAMLparam0();
  CAMLlocal1(pair);
  pair = caml_alloc_tuple(2);
  Store_field(pair, 0, of_rlim(rl.rlim_cur));
  Store_field(pair, 1, of_rlim(rl.rlim_max));
  CAMLreturn(caml_alloc_some(pair));
}

CAMLprim value ocsigen_open_files_limits(value unit)
{
  (void)unit;
  return alloc_limits(current_limits());
}

static int set_soft_limit(struct rlimit rl, rlim_t soft)
{
  rl.rlim_cur = soft;
  return setrlimit(RLIMIT_NOFILE, &rl);
}

/* Raises the soft limit to [wanted], or to the hard limit if it is lower.
   macOS refuses a soft limit above OPEN_MAX, even when the hard limit is
   higher or infinite: the soft limit is then raised to OPEN_MAX. */
CAMLprim value ocsigen_open_files_raise(value wanted)
{
  struct rlimit rl = current_limits();
  intnat n = Long_val(wanted);
  if (n > 0 && (rlim_t)n > rl.rlim_cur) {
    rlim_t soft = (rlim_t)n < rl.rlim_max ? (rlim_t)n : rl.rlim_max;
    if (set_soft_limit(rl, soft) != 0) {
#ifdef OPEN_MAX
      if (!(errno == EINVAL && soft > OPEN_MAX && rl.rlim_cur < OPEN_MAX &&
            set_soft_limit(rl, OPEN_MAX) == 0))
#endif
        unix_error(errno, "setrlimit", Nothing);
    }
  }
  return alloc_limits(current_limits());
}

#endif
