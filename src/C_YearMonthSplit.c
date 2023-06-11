#include "grattanInflator.h"

SEXP C_YearMonthSplit(SEXP x, SEXP xClass, SEXP MonthFY, SEXP nthreads) {
  R_xlen_t N = xlength(x);
  int x_class = asInteger(xClass);
  int month_fy = asInteger(MonthFY);
  int nThread = as_nThread(nthreads);
  YearMonth * x_YM = malloc(sizeof(YearMonth) * N);
  if (x_YM == NULL) {
    free(x_YM); // # nocov
    return R_NilValue; // # nocov
  }
  SEXP2YearMonth(x_YM, x, x_class, month_fy, false, "x", nThread);
  int np = 0;
  SEXP ans1 = PROTECT(allocVector(INTSXP, N)); ++np;
  SEXP ans2 = PROTECT(allocVector(INTSXP, N)); ++np;

  int * restrict ans1p = INTEGER(ans1);
  int * restrict ans2p = INTEGER(ans2);

  FORLOOP({
    ans1p[i] = x_YM[i].year + MIN_YEAR;
    ans2p[i] = x_YM[i].month;
  })

  SEXP ans = PROTECT(allocVector(VECSXP, 2)); ++np;
    SET_VECTOR_ELT(ans, 0, ans1);
    SET_VECTOR_ELT(ans, 1, ans2);
    free(x_YM);
    UNPROTECT(np);
    return ans;
}
