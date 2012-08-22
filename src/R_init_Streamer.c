#include <R_ext/Rdynload.h>
#include "raw_input.h"
#include "lapply.h"

static const R_CallMethodDef callMethods[] = {
  /* raw_parse */
  {".raw_parse_count_records", (DL_FUNC) &raw_parse_count_records, 2},
  {".raw_parse", (DL_FUNC) &raw_parse, 3},
  /* lapply */
  {".lapply_Producer", (DL_FUNC) &lapply_Producer, 3},
  {NULL, NULL, 0}
};

void
R_init_Streamer(DllInfo *info)
{
    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
