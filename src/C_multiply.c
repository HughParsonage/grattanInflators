#include "grattanInflator.h"

SEXP C_multiply(SEXP x, SEXP R, SEXP nthreads) {
  int nThread = as_nThread(nthreads);
  if (!isReal(x)) {
    error("`x` was type '%s' but must be a double vector.", type2char(TYPEOF(x))); // # nocov
  }
  const double r = asReal(R);
  double * xp = REAL(x);
  R_xlen_t N = xlength(x);
  FORLOOP({
   xp[i] *= r;
  })
  return x;
}
