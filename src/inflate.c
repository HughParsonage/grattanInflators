#include "grattanInflator.h"




SEXP C_Inflate2(SEXP From, SEXP To, SEXP Index, SEXP IndexMinIDate, SEXP IndexFreq, SEXP nthreads) {
  int nThread = as_nThread(nthreads);
  if (!isInteger(From) || !isInteger(To)) {
    return R_NilValue;
  }
  const int * xp = INTEGER(From);
  const int * yp = INTEGER(To);
  R_xlen_t N_x = xlength(From);
  R_xlen_t N_y = xlength(To);
  R_xlen_t N = N_x >= N_y ? N_x : N_y;
  if (!isReal(Index)) {
    error("Index wasn't REALSXP which is not supported.");
  }
  const double * index = REAL(Index);

  int index_min = asInteger(IndexMinIDate);
  int freq = asInteger(IndexFreq);
  const unsigned int div = 12 / freq;
  const unsigned int p_index_min = p_search(index_min) / div;
  SEXP ans = PROTECT(allocVector(REALSXP, N));
  double * restrict ansp = REAL(ans);
  switch(freq) {
  case 12:
    FORLOOP({
      unsigned int x_p = (p_search(xp[i])) - p_index_min;
      unsigned int y_p = (p_search(yp[i])) - p_index_min;
      ansp[i] = index[y_p] / index[x_p];
    })
    break;
  case 4:
    FORLOOP({
      unsigned int x_p = (p_search(xp[i]) / 3) - p_index_min;
      unsigned int y_p = (p_search(yp[i]) / 3) - p_index_min;
      ansp[i] = index[y_p] / index[x_p];
    })
    break;
  case 1:
    FORLOOP({
      unsigned int x_p = (p_search(xp[i]) / 12) - p_index_min;
      unsigned int y_p = (p_search(yp[i]) / 12) - p_index_min;
      ansp[i] = index[y_p] / index[x_p];
    })
    break;
  }

  UNPROTECT(1);
  return ans;
}



