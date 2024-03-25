#include "grattanInflator.h"

static bool leqcc1(const char * x, char y[8], bool equal_ok) {
  if (x[0] != y[0]) {
    return x[0] < y[0];
  }
  for (int j = 2; j < 4; ++j) {
    if (x[j] < y[j]) {
      return true;
    }
    if (x[j] > y[j]) {
      return false;
    }
  }
  for (int j = 5; j < 7; ++j) {
    if (x[j] < y[j]) {
      return true;
    }
    if (x[j] > y[j]) {
      return false;
    }
  }
  return equal_ok;
}


static void minDate(char yyyy_mm[8], const SEXP * xp, R_xlen_t N) {
  for (R_xlen_t i = 0; i < N; ++i) {
    const char * xi = CHAR(xp[i]);
    if (xi[0] == '1') {
      if (yyyy_mm[0] == '2') {
        for (int j = 0; j < 7; ++j) {
          yyyy_mm[j] = xi[j];
        }
        continue;
      }
      if (leqcc1(xi, yyyy_mm, true)) {
        for (int j = 0; j < 7; ++j) {
          yyyy_mm[j] = xi[j];
        }
      }
    } else {
      if (yyyy_mm[0] == '1') {
        continue;
      }
      if (leqcc1(xi, yyyy_mm, true)) {
        for (int j = 0; j < 7; ++j) {
          yyyy_mm[j] = xi[j];
        }
      }
    }
  }
}


SEXP C_minDate(SEXP x) {
  R_xlen_t N = xlength(x);
  const SEXP * xp = STRING_PTR(x);
  char yyyy_mm[8] = {'2', '9', '9', '9', '-', '1', '9', '\0'};
  minDate(yyyy_mm, xp, N);
  return ScalarString(mkCharCE(yyyy_mm, CE_UTF8));
}

SEXP C_all_dates(SEXP x) {
  int n = MAX_IDATE - MIN_IDATE + 1;
  SEXP ans = PROTECT(allocVector(INTSXP, n));
  int * restrict ansp = INTEGER(ans);
  for (int i = 0; i < n; ++i) {
    ansp[i] = MIN_IDATE + i;
  }
  UNPROTECT(1);
  return ans;
}

void iminmax(int xminmax[2], const int * xp, R_xlen_t N, const int fy_month, int nThread) {
  int xmin = xp[0];
  int xmax = xp[0];
  if (xmin == NA_INTEGER) {
    xmin = INT_MAX;
    // xmax will be INT_MIN naturally; also can signal a totally constant xp
  }
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(min : xmin) reduction(max : xmax)
#endif
  for (R_xlen_t i = 1; i < N; ++i) {
    int xpi = xp[i];
    if (xpi == NA_INTEGER) {
      continue;
    }
    if (xpi >= xmin && xpi <= xmax) {
      continue;
    }
    if (xpi < xmin) {
      xmin = xpi;
    } else {
      xmax = xpi;
    }
  }
  xminmax[0] = xmin;
  xminmax[1] = xmax;
}



