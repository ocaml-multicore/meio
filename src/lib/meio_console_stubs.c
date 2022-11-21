#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>
#include <caml/osdeps.h>

extern int64_t caml_time_counter(void);

value caml_eio_time_counter(value v_unit) {
  return caml_copy_int64(caml_time_counter());
}