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
  const SEXP * xp = STRING_PTR_RO(x);
  char yyyy_mm[8] = {'2', '9', '9', '9', '-', '1', '9', '\0'};
  minDate(yyyy_mm, xp, N);
  return ScalarString(mkCharCE(yyyy_mm, CE_UTF8));
}

SEXP C_maxYearMonth(SEXP x, SEXP FyMonth) {
  if (TYPEOF(x) != STRSXP) {
    error("`x` must be a character vector."); // # nocov
  }
  const int fy_month = as_fy_month(FyMonth);
  const SEXP * xp = STRING_PTR_RO(x);
  const R_xlen_t N = xlength(x);
  int max_ym = -1;

  // CHAR() and length() use the R API, so this scan must remain serial.
  for (R_xlen_t i = 0; i < N; ++i) {
    if (xp[i] == NA_STRING) {
      continue;
    }
    const int n = length(xp[i]);
    if (n != 7 && n != 10) {
      continue;
    }
    YearMonth ym;
    string2YearMonth(&ym, CHAR(xp[i]), n, fy_month);
    if (!YM_valid(ym)) {
      continue;
    }
    const int packed = 12 * (ym.year + MIN_YEAR) + ym.month;
    if (packed > max_ym) {
      max_ym = packed;
    }
  }

  SEXP ans = PROTECT(allocVector(INTSXP, 2));
  if (max_ym < 0) {
    INTEGER(ans)[0] = NA_INTEGER;
    INTEGER(ans)[1] = NA_INTEGER;
  } else {
    INTEGER(ans)[0] = (max_ym - 1) / 12;
    INTEGER(ans)[1] = (max_ym - 1) % 12 + 1;
  }
  UNPROTECT(1);
  return ans;
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
  if (N <= 0) {
    // An empty input has no minimum or maximum; these sentinels compare as
    // "inside any range" so that callers report no violation.
    xminmax[0] = INT_MAX;
    xminmax[1] = INT_MIN;
    return;
  }
  int xmin = INT_MAX;
  int xmax = INT_MIN;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(min : xmin) reduction(max : xmax)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    const int xpi = xp[i];
    if (xpi == NA_INTEGER) {
      continue;
    }
    if (xpi < xmin) {
      xmin = xpi;
    }
    if (xpi > xmax) {
      xmax = xpi;
    }
  }
  xminmax[0] = xmin;
  xminmax[1] = xmax;
}

