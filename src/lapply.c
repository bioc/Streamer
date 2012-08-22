#include "lapply.h"

SEXP
lapply_Producer(SEXP fun, SEXP X, SEXP rho)
{
    SEXP call1, ans1, ans;
    int iter = 0;
    R_len_t curr_size = 4096;
    PROTECT_INDEX px;

    /* FIXME: type checks */

    PROTECT(call1 = lang2(fun, X));
    PROTECT_WITH_INDEX(ans = Rf_allocVector(VECSXP, curr_size), &px);
    while (1) {
	PROTECT(ans1 = Rf_eval(call1, rho));
	if (0 == Rf_length(ans1)) {
	    UNPROTECT(1);
	    break;
	}
	if (iter == curr_size) {
	    if (curr_size == R_LEN_T_MAX)
		Rf_error("%s cannot create %d-element vector",
			 "'lapply,Producer-method'", curr_size);
	    curr_size *= 1.6;
	    if (curr_size > R_LEN_T_MAX)
		curr_size = R_LEN_T_MAX;
	    REPROTECT(ans = Rf_lengthgets(ans, curr_size), px);
	}
	SET_VECTOR_ELT(ans, iter++, ans1);
	UNPROTECT(1);
    }
    UNPROTECT(2);
    return Rf_lengthgets(ans, iter);
}
