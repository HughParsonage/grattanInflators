#include "grattanInflator.h"

#define ERR_CHAR_NOT_YYYY 11
#define ERR_CHAR_YR_RANGE 13
#define ERR_CHAR_NO_MONTH 15
#define ERR_CHAR_BAD_MDAY 17
#define ERR_CHAR_FY_QUART 19

bool starts_with_yyyy(const char * x) {
  return (x[0] == '1' || x[0] == '2') && isdigit(x[1]) && isdigit(x[2]) && isdigit(x[3]);
}

bool is_leap_yr(int yr) {
  if (yr < 0 || yr % 4) {
    return 0;
  }
  if (yr % 100) {
    return 0;
  }
  if (yr % 400) {
    return 1;
  }
  return 1;
}

static bool is_valid_fy_quartet(const char * z) {
  register char u = z[2], v = z[3], x = z[5], y = z[6];
  if (!isdigit(u) || !isdigit(v) || !isdigit(x) || !isdigit(y)) {
    return false;
  }

  if (v == '9') {
    if (u == '9') {
      return x == '0' && y == '0';
    }
    // 19-20
    // uv-xy
    return x == u + 1 && y == '0';
  }
  return x == u && y == v + 1;
}

unsigned char invalid_mday(const char * x, int yr, int month) {
  static int MDAYS[13] = {0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
  if (!isdigit(x[8]) || !isdigit(x[9])) {
    return 1;
  }
  int mday = 10 * (x[8] - '0') + (x[9] - '0');
  if (mday == 0 || month > 12 || mday > MDAYS[month]) {
    return 1;
  }
  if (month == 2 && !is_leap_yr(yr) && mday == 29) {
    return 1;
  }
  return 0;
}

static unsigned char err_string(const char * x, int n, int check) {
  if (!starts_with_yyyy(x)) {
    return ERR_CHAR_NOT_YYYY;
  }
  int yr = string2year(x);
  if (yr < 0 || yr > 127) {
    return ERR_CHAR_YR_RANGE;
  }
  if (n == 10) {
    int month = string2month(x);
    if (month == 15) {
      return ERR_CHAR_NO_MONTH;
    }
    if (check >= 2 && invalid_mday(x, yr + 1948, month)) {
      return ERR_CHAR_BAD_MDAY;
    }

  }
  if (n == 7 && !is_valid_fy_quartet(x)) {
    return ERR_CHAR_FY_QUART;
  }
  return 0;
}






static void check_valid_strings(const SEXP * xp, R_xlen_t N, int check, int nThread, const char * var) {
  unsigned char o = 0;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) schedule(static) reduction(| : o)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    if (xp[i] == NA_STRING) {
      continue;
    }
    int n = length(xp[i]);
    const char * xi = CHAR(xp[i]);
    o |= err_string(xi, n, check);
  }
  if (o != 0) {
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xp[i] == NA_STRING) {
        continue;
      }
      const char * xi = CHAR(xp[i]);
      int nxi = length(xp[i]);
      int ei = err_string(xi, nxi, check);
      switch(ei) {
      case ERR_CHAR_NOT_YYYY:
        error("`%s` contained invalid element:\n\t %s[%lld] = %s\n(must start with YYYY)",
              var, var, i + 1, CHAR(xp[i]));
      case ERR_CHAR_NO_MONTH:
        error("`%s` contained invalid element:\n\t %s[%lld] = %s\n(No month)",
              var, var, i + 1, CHAR(xp[i]));
      case ERR_CHAR_BAD_MDAY:
        error("`%s` contained invalid element:\n\t %s[%lld] = %s\n(Day component invalid)",
              var, var, i + 1, CHAR(xp[i]));
      case ERR_CHAR_YR_RANGE:
        error("`%s` contained invalid element:\n\t %s[%lld] = %s\n(Years must be between %d and %d)",
              var, var, i + 1, CHAR(xp[i]), MIN_YEAR, MAX_YEAR);
      case ERR_CHAR_FY_QUART:
        error("`%s` contained invalid element:\n\t %s[%lld] = %s\n(Invalid fy)",
              var, var, i + 1, CHAR(xp[i]));
      }
    }
  }
}

void check_strsxp(const SEXP * xp, R_xlen_t N, int check, const char * var, const int min_date, int nThread) {
  check_valid_strings(xp, N, check, nThread, var);
  err_if_below_mindate(xp, N, min_date, var, nThread);
}

void check_intsxp(const int * xp, R_xlen_t N, int nThread, int xclass, int min_date,
                  const char * var) {
  int xmin = INT_MAX; // since I want to exclude NA, which may be the first element
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(min : xmin)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    if (xp[i] < xmin && xp[i] != NA_INTEGER) {
      xmin = xp[i];
    }
  }
  if (xclass == CLASS_Date || xclass == CLASS_IDate) {
    if (xmin < min_date) {
      for (R_xlen_t i = 0; i < N; ++i) {
        if (xp[i] < min_date && xp[i] != NA_INTEGER) {
          if (xp[i] < MIN_IDATE) {
            error("%s[%lld] is outside the permitted range (1948-2075).");
          }
          YearMonth YMi = idate2YearMonth(xp[i]);
          error("`%s[%lld] = %d-%02d-01` before the earliest allowable date.",
                var, i + 1, YMi.year + MIN_YEAR, YMi.month);
        }
      }
    }
  } else {
    YearMonth minYearMonth = idate2YearMonth(min_date);
    int minYear = minYearMonth.year;
    minYear += MIN_YEAR;
    if (xmin < minYear) {
      for (R_xlen_t i = 0; i < N; ++i) {
        if (xp[i] < minYear) {
          if (xclass == CLASS_FY) {
            error("`%s[%lld] = %d-%d` which is earlier than the earliest allowable fy = %d-%d",
                  var, i + 1,
                  xp[i] - 1, xp[i] % 100,
                  minYear - 1, minYear % 100);
          } else {
            error("`%s[%lld] = %d` which is earlier than the earliest allowable year = %d",
                  var, i + 1, xp[i], minYear);
          }
        }
      }
    }
  }
}


SEXP C_check_input(SEXP x, SEXP Var, SEXP Check, SEXP Class, SEXP minDate, SEXP nthreads) {
  const int check = asInteger(Check);
  const char * var = CHAR(STRING_ELT(Var, 0));
  int nThread = as_nThread(nthreads);
  const int min_date = asInteger(minDate);
  int xclass = asInteger(Class);

  switch(TYPEOF(x)) {
  case STRSXP:
    check_strsxp(STRING_PTR(x), xlength(x), check, var, min_date, nThread);
    break;
  case INTSXP:
    check_intsxp(INTEGER(x), xlength(x), nThread, xclass, min_date, var);
  }

  return R_NilValue;
}
