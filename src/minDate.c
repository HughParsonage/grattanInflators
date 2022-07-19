#include "grattanInflator.h"

bool leqcc1(const char * x, char y[8]) {
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
  return true;
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
      if (leqcc1(xi, yyyy_mm)) {
        for (int j = 0; j < 7; ++j) {
          yyyy_mm[j] = xi[j];
        }
      }
    } else {
      if (yyyy_mm[0] == '1') {
        continue;
      }
      if (leqcc1(xi, yyyy_mm)) {
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
  Rprintf("yr-mo = %d-%d\n", yr, mo);
  const char * digits = "0123456789";
  yyyy_mm[0] = yr < 2000 ? '1' : '2';
  yyyy_mm[1] = digits[(yr / 100) % 10];
  yyyy_mm[2] = digits[(yr / 10) % 10];
  yyyy_mm[3] = digits[yr % 10];
  yyyy_mm[5] = mo >= 10 ? '1' : '0';
  yyyy_mm[6] = mo % 10;
}

void err_if_below_mindate(const SEXP * xp, R_xlen_t N, int minDate, const char * var) {
  char yyyy_mm[8] = {0};
  Rprintf("%d>", minDate);
  idate2char8(yyyy_mm, minDate);
  Rprintf("%s\n", (const char *)yyyy_mm);

  for (R_xlen_t i = 0; i < N; ++i) {
    if (xp[i] == NA_STRING) {
      continue;
    }
    const char * xi = CHAR(xp[i]);
    if (leqcc1(xi, yyyy_mm)) {
      error("`%s[%lld] = '%s'` which is prior to '%s-01', the earliest allowed date.",
            var, i + 1, xi, (const char *)yyyy_mm);
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
