#include "grattanInflator.h"

SEXP C_multiply(SEXP x, SEXP R, SEXP nthreads) {
  int nThread = as_nThread(nthreads);
  const double r = asReal(R);
  double * xp = REAL(x);
  R_xlen_t N = xlength(x);
  FORLOOP({
   xp[i] *= r;
  })
  return x;
}
