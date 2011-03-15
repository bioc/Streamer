#ifndef RAW_INPUT_H
#define RAW_INPUT_H

#include <Rdefines.h>

SEXP
raw_parse_count_records(SEXP buf, SEXP sep);

SEXP
raw_parse(SEXP buf, SEXP sep, SEXP trim);

#endif

