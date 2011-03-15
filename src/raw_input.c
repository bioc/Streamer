#include "raw_input.h"

SEXP
raw_parse_count_records(SEXP buf, SEXP sep)
{
  Rbyte *b = RAW(buf);
  Rbyte *be = RAW(buf) + Rf_length(buf), *se = RAW(sep) + Rf_length(sep);

  if (b == be) 
    return ScalarInteger(0);
  int irec = 1;

  Rbyte *s;
  while (b != be) {

    s = RAW(sep);
    while (*b != *s && b != be) ++b;
    if (b == be) break;

    Rbyte *b0 = b;
    while (*b == *s && b != be && s != se) {
      ++b; 
      ++s;
    }
    if (s != se) 
      b = b0 + 1;
    else 
      ++irec;
  }

  return ScalarInteger(irec);
}

SEXP
raw_parse(SEXP buf, SEXP sep, SEXP trim)
{
  int nrec = INTEGER(raw_parse_count_records(buf, sep))[0];
  int irec = 0;

  if (0 == nrec)
    return NEW_LIST(0);

  Rbyte *b = RAW(buf);
  Rbyte *be = RAW(buf) + Rf_length(buf), *se = RAW(sep) + Rf_length(sep);

  SEXP result = PROTECT(NEW_LIST(nrec));

  Rbyte *s, *rec_start = RAW(buf);

  while (b != be) {

    s = RAW(sep);
    while (*b != *s && b != be) ++b;
    if (b == be) break;

    Rbyte *b0 = b;
    while (*b == *s && b != be && s != se) {
      ++b;
      ++s;
    }
    if (s != se) {
      b = b0 + 1;
    } else {
      /* new record */
      SET_VECTOR_ELT(result, irec, NEW_RAW(b0 - rec_start));
      memcpy(RAW(VECTOR_ELT(result, irec)), rec_start, b0 - rec_start);
      ++irec;
      b = b0 + Rf_length(sep);
      rec_start = b0 + Rf_length(trim);
    }
  }

  /* final record */
  SET_VECTOR_ELT(result, irec, NEW_RAW(b - rec_start));
  memcpy(RAW(VECTOR_ELT(result, irec)), rec_start, b - rec_start);

  UNPROTECT(1);
  return result;
}
