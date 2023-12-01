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

void idate2char8(char yyyy_mm[8], int x) {
  if (x < MIN_IDATE) {
    strcpy(yyyy_mm, "1948-01");
    return;
  }
  if (x > MAX_IDATE) {
    strcpy(yyyy_mm, "2075-12");
    return;
  }
  YearMonth xYM = idate2YearMonth(x);
  int yr = xYM.year + MIN_YEAR;
  int mo = xYM.month;
  const char * digits = "0123456789";
  yyyy_mm[0] = yr < 2000 ? '1' : '2';
  yyyy_mm[1] = digits[(yr / 100) % 10];
  yyyy_mm[2] = digits[(yr / 10) % 10];
  yyyy_mm[3] = digits[yr % 10];
  yyyy_mm[4] = '-';
  yyyy_mm[5] = mo >= 10 ? '1' : '0';
  yyyy_mm[6] = digits[(mo % 10)];
}

static bool ibetween(int xminmax[2], const int * xp, R_xlen_t N, int nThread) {
  int xmin = xminmax[0];
  int xmax = xminmax[1];

  bool o = true;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(&& : o)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    int xpi = xp[i];
    if (xpi >= xmin && xpi <= xmax) {
      continue;
    }
    o = false;
  }
  return o;
}

static void ierr_if_outside(int xminmax[2], const int * xp, R_xlen_t N, int nThread, const char * var,
                            bool was_date) {
  if (!was_date) {
    xminmax[0] = idate2YearMonth(xminmax[0]).year + 1948;
    xminmax[1] = idate2YearMonth(xminmax[1]).year + 1948;
  }
  if (!ibetween(xminmax, xp, N, nThread)) {
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xp[i] == NA_INTEGER) {
        continue;
      }
      if (xp[i] < xminmax[0]) {
        if (was_date) {
          char yyyy_mm_min[8] = {0};
          char yyyy_mm_xpi[8] = {0};
          idate2char8(yyyy_mm_min, xminmax[0]);
          idate2char8(yyyy_mm_xpi, xp[i]);
          error("`%s[%lld] = %s` which is prior to the earliest allowable date: %s",
                var, (long long)i + 1, yyyy_mm_xpi, yyyy_mm_min);
        } else {
          error("`%s[%lld] = %d` (int) which is prior to the earliest allowable date: %d.",
                var, (long long)i + 1, xp[i], xminmax[0]);
        }
      }
      if (xp[i] > xminmax[1]) {
        if (was_date) {
          char yyyy_mm_max[8] = {0};
          char yyyy_mm_xpi[8] = {0};
          idate2char8(yyyy_mm_max, xminmax[1]);
          idate2char8(yyyy_mm_xpi, xp[i]);
          error("`%s[%lld] = %s` which is after the latest allowable date: %s",
                var, (long long)i + 1, yyyy_mm_xpi, yyyy_mm_max);
        } else {
          error("`%s[%lld] = %d` (int) which is after the latest allowable date: %d.",
                var, (long long)i + 1, xp[i], xminmax[1]);
        }
      }
    }
  }
}




static void serr_if_outside(int xminmax[2], const SEXP * xp, R_xlen_t N, int nThread, const char * var,
                            bool was_date) {
  char yyyy_mm_min[8] = {0};
  char yyyy_mm_max[8] = {0};
  idate2char8(yyyy_mm_min, xminmax[0]);
  idate2char8(yyyy_mm_max, xminmax[1]);
  bool o = false;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(|| : o)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    if (o || xp[i] == NA_STRING || length(xp[i]) < 7) {
      continue;
    }
    const char * xi = CHAR(xp[i]);
    if (leqcc1(xi, yyyy_mm_min, false) || !leqcc1(xi, yyyy_mm_max, false)) {
      o = true;
    }
  }
  if (o) {
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xp[i] == NA_STRING || length(xp[i]) < 7) {
        continue;
      }
      const char * xi = CHAR(xp[i]);
      if (leqcc1(xi, yyyy_mm_min, false)) {
        error("`%s[%lld] = %s` which is prior to the earliest allowable date: %s.",
              var, (long long)i + 1, xi, (const char *)yyyy_mm_min);
      }
      if (!leqcc1(xi, yyyy_mm_max, false)) {
        error("`%s[%lld] = %s` which is after the latest allowable date: %s.",
              var, (long long)i + 1, xi, (const char *)yyyy_mm_max);
      }
    }
  }
}

void err_if_anyOutsideDate(int minmax[2], SEXP x, int nThread, const char * var, bool was_date) {
  switch(TYPEOF(x)) {
  case INTSXP:
    ierr_if_outside(minmax, INTEGER(x), xlength(x), nThread, var, was_date);
    break;
    // # nocov start
  case REALSXP:
    error("Internal error: REALSXP not permitted.");
    break;
    // # nocov end
  case STRSXP:
    serr_if_outside(minmax, STRING_PTR(x), xlength(x), nThread, var, was_date);
    break;
  }
}

static bool ianyBeyond(int max_date, const int * xp, R_xlen_t N, int nThread) {
  bool o = false;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(|| : o)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    if (xp[i] > max_date) {
      o = true;
    }
  }
  return o;
}

static bool sanyBeyond(int max_date, const SEXP * xp, R_xlen_t N, int nThread) {
  char yyyy_mm_max[8] = {0};
  idate2char8(yyyy_mm_max, max_date);
  bool o = false;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(|| : o)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    if (o || xp[i] == NA_STRING || length(xp[i]) < 7) {
      continue;
    }
    const char * xi = CHAR(xp[i]);
    if (!leqcc1(xi, yyyy_mm_max, false)) {
      o = true;
    }
  }
  return o;
}

SEXP C_minDate(SEXP x) {
  R_xlen_t N = xlength(x);
  const SEXP * xp = STRING_PTR(x);
  char yyyy_mm[8] = {'2', '9', '9', '9', '-', '1', '9', '\0'};
  minDate(yyyy_mm, xp, N);
  return ScalarString(mkCharCE(yyyy_mm, CE_UTF8));
}

SEXP C_anyBeyond(SEXP x, SEXP maxDate, SEXP nthreads) {
  const int max_date = asInteger(maxDate);
  int nThread = as_nThread(nthreads);
  R_xlen_t N = xlength(x);
  switch(TYPEOF(x)) {
  case STRSXP:
   return ScalarLogical(sanyBeyond(max_date, STRING_PTR(x), N, nThread));
  case INTSXP:
    return ScalarLogical(ianyBeyond(max_date, INTEGER(x), N, nThread));
  default:
    error("Internal error: wrong TYPEOF in C_anyBeyond.");
  }
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

