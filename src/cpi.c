#include "grattanInflator.h"



bool is_supported_IDate(unsigned int x) {
  unsigned int ux = (x + NEG_MIN_IDATE);
  return ux < RANGE_IDATE;
}



void check_within_idaterange(const int * xp, R_xlen_t N, int nThread, const char * var) {
  char o = 1;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) schedule(static) reduction(& : o)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    o &= is_supported_IDate(xp[i]);
  }
  if (o != 1) {
    for (R_xlen_t i = 0; i < N; ++i) {
      if (!is_supported_IDate(xp[i])) {
        error("In `%s`, vector was integer but value %d at position %lld was not in valid range",
              var, xp[i], i + 1);
      }
    }
  }
}

int is_leap_year(int year) {
  if (year < 0 || year % 4) {
    return 0;
  }
  if (year % 100) {
    return 1;
  }
  if (year % 400) {
    return 0;
  }
  return 1;
}

static bool starts_with_yyyy(const char * x) {
  return (x[0] == '1' || x[0] == '2') && isdigit(x[1]) && isdigit(x[2]) && isdigit(x[3]);
}

int string2year(const char * x) {
  // no check on whether string length is adequate, or starts with digits
  int year = 0;
  year += (x[1] == '9') ? 1900 : 2000;
  year += 10 * (x[2] - '0');
  year += x[3] - '0';
  return year - 1948;
}

static bool is_valid_month(char u, char v) {
  if (u < '0' || u > '3') {
    return false;
  }
  if (!isdigit(v)) {
    return false;
  }
  if (u == '3') {
    return v == '0' || v == '1';
  }
  return true;
}

static bool is_valid_fy_quartet(char u, char v, char x, char y) {
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

YearMonth NA_YM() {
  YearMonth O;
  O.year = 127;
  O.month = 15; // nonsense values
  return O;
}


int valid_form(const char * x, int n, bool check_day, bool prefer_fy) {
  // which valid form
  // 0 not valid

  //   0123456789
  // 1 YYYY-MM-DD
  // 2 YYYY-ZZ (fy)
  // 3 YYYY-MM
  if (n != 10 && n != 7) {
    return 0;
  }
  if (!starts_with_yyyy(x)) {
    return 0;
  }
  if (!isdigit(x[6])) {
    return 0;
  }


  if (n == 10) {
    if (x[5] != '0' && x[5] != '1') {
      return 0;
    }
    // 00-12
    if (x[5] == '1' && x[6] != '0' && x[6] != '1' && x[6] != '2') {
      return 0;
    }
    if (check_day) {
      if (x[8] < '0' || x[8] > '3') {
        return 0;
      }
      if (!isdigit(x[9])) {
        return 0;
      }
      if (x[5] == '0' && x[6] == '2') {
        // February
        if (x[8] == '3') {
          return 0;
        }
        // TODO:
        // check leap year valid for 9
      }
    }
    return 1;
  }
  if (is_valid_fy_quartet(x[2], x[3], x[5], x[6])) {
    // maybe 2010-11
    if (!prefer_fy && is_valid_month(x[5], x[6])) {
      return 3;
    }
    return 2;
  }
  return 0;
}

void check_string_valid(SEXP x, bool constant_only, int nThread, bool check_day, bool prefer_fy, const char * var) {
  if (!isString(x)) {
    error("Internal error(check_string_valid): expected a string."); // # nocov
  }
  R_xlen_t N = xlength(x);
  if (N == 0) {
    return;
  }
  const SEXP * xp = STRING_PTR(x);
  R_xlen_t j = 0;
  while (j < N && xp[j] == NA_STRING) {
    ++j;
  }
  if (j == N) {
    warning("`%s` is naught but NA_STRING (completely empty) so no valid form can be identified.", var);
    return;
  }
  if (constant_only) {
    int first_form = valid_form(CHAR(xp[j]), length(xp[j]), check_day, prefer_fy);
    if (first_form == 0) {
      error("`%s` was not in valid form at position %lld = '%s'", var, j + 1, CHAR(xp[j]));
    }

    for (R_xlen_t i = j + 1; i < N; ++i) {
      if (xp[i] == NA_STRING) {
        continue;
      }
      int valid_form_i = valid_form(CHAR(xp[i]), length(xp[i]), check_day, prefer_fy);
      if (valid_form_i == 0) {
        error("`%s` was not of a valid form at position %lld = '%s'.", var, j + 1, CHAR(xp[j]));
      }
      if (valid_form_i != first_form) {
        error("`%s` was not of the same date form throughout.\n\t %s[%lld] = '%s'\n\t %s[%lld] = '%s'.",
              var,
              var, j + 1, CHAR(xp[j + 1]),
              var, i + 1, CHAR(xp[i + 1]));
      }

    }
    return;
  }
  for (R_xlen_t i = j + 1; i < N; ++i) {
    if (xp[i] == NA_STRING) {
      continue;
    }
    int valid_form_i = valid_form(CHAR(xp[i]), length(xp[i]), check_day, prefer_fy);
    if (valid_form_i == 0) {
      error("`%s` was not of a valid form at position %lld = '%s'.", var, j + 1, CHAR(xp[j]));
    }
  }
}

void check_SEXP_valid(SEXP x, bool constant_only, int nThread, bool check_day, bool prefer_fy, const char * var) {
  switch(TYPEOF(x)) {
  case INTSXP:
    check_within_idaterange(INTEGER(x), xlength(x), nThread, var);
    break;
  case REALSXP:
    error("Internal error(check_SEXP_valid): was passed a REALSXP, but numeric vectors should be integer here."); // # nocov
  case STRSXP:
    check_string_valid(x, constant_only, nThread, check_day, prefer_fy, var);
    break;
  default:
    error("`%s` was type '%s' but must be character or integer.", var, type2char(TYPEOF(x)));
  }
}






void SEXP2YearMonth(YearMonth * ansp,
                    SEXP x,
                    int x_class,
                    bool constant_only, bool prefer_fy,
                    int fy_month,
                    bool check_day, const char * var, int nThread) {
  check_SEXP_valid(x, constant_only, nThread, check_day, prefer_fy, var);
  if (ansp == NULL) {
    return;
  }
  R_xlen_t N = xlength(x);
  if (isInteger(x)) {
    const int * xp = INTEGER(x);
    switch(x_class) {
    case CLASS_FY:
      FORLOOP({
        YearMonth O;
        O.year = xp[i] - 1948;
        O.month = fy_month;
        ansp[i] = O;
      })
      break;
    case CLASS_Date:
    case CLASS_IDate:
      FORLOOP({
        ansp[i] = idate2YearMonth(xp[i]);
      })
      break;
    case CLASS_integer:
      FORLOOP({
        YearMonth O;
        O.year = xp[i];
        O.month = 1;
        ansp[i] = O;
      })
      break;
    }
    return;
  }
  const SEXP * xp = STRING_PTR(x);

  if (constant_only) {
    R_xlen_t j = 0;
    while (j < N && xp[j] == NA_STRING) {
      ++j;
    }
    int form = valid_form(CHAR(xp[j]), length(xp[j]), check_day, prefer_fy);
    if (form == 2) {
      FORLOOP({
        if (xp[i] == NA_STRING) {
          ansp[i] = NA_YM();
          continue;
        }
        const char * xj = CHAR(xp[i]);
        YearMonth O;
        O.year = string2year(xj) + 1 - (fy_month >= 7);
        O.month = fy_month;
        ansp[i] = O;
      })
    } else {
      FORLOOP({
        if (xp[i] == NA_STRING) {
          ansp[i] = NA_YM();
          continue;
        }
        const char * xj = CHAR(xp[i]);
        YearMonth O;
        O.year = string2year(xj);
        O.month = 10 * (xj[5] - '0') + (xj[6] - '0');
        ansp[i] = O;
      })
    }
    return;
  }
  FORLOOP({
    if (xp[i] == NA_STRING) {
      ansp[i] = NA_YM();
      continue;
    }
    const char * xj = CHAR(xp[i]);
    int form_i = valid_form(xj, length(xp[i]), check_day, prefer_fy);
    YearMonth O;
    O.year = string2year(xj);
    if (form_i == 2) {
      O.year += 1 - (fy_month >= 7);
      O.month = fy_month;
      ansp[i] = O;
      continue;
    }
    O.month = 10 * (xj[5] - '0') + (xj[6] - '0');
    ansp[i] = O;
  })
}

int index_freq2int(SEXP IndexFreq) {
  switch(TYPEOF(IndexFreq)) {
  case INTSXP:
  case REALSXP:
    return asInteger(IndexFreq);
  case STRSXP:
  {
    const char * x = CHAR(STRING_ELT(IndexFreq, 0));
    switch(toupper(x[0])) {
    case 'A':
      return 1;
    case 'Q':
      return 4;
    case 'M':
      return 12;
    case 'D':
      return 365;
    }
  }
    return 0;
  }
  return 0;
}

static int MONTH_TO_QUARTER[15] = {0,
                                   0, 0, 0,
                                   1, 1, 1,
                                   2, 2, 2,
                                   3, 3, 3};

int yqi(YearMonth YM) {
  // return (YM.year << 2) + MONTH_TO_QUARTER[YM.month];
  int i = (YM.year);
  i <<= 2;
  i += MONTH_TO_QUARTER[YM.month];
  return i;
}

void InflateQuarterly(double * restrict ansp, R_xlen_t N, int nThread,
                      YearMonth * FromDate,
                      YearMonth * ToDate,
                      R_xlen_t N_from,
                      R_xlen_t N_to,
                      const double * index, YearMonth index_min) {
  const int index_min_i = yqi(index_min);
  nThread = 1;
  if (N_from == N && N_to == N) {
    FORLOOP({
      int from_i = yqi(FromDate[i]) - index_min_i;
      int to_i = yqi(ToDate[i]) - index_min_i;

      if (to_i < 0 || from_i < 0) {
        ansp[i] = NaN;
        continue;
      }
      double from_x = index[from_i];
      double to_x = index[to_i];
      ansp[i] *= to_x / from_x;
    })
  } else if (N_from == N && N_to == 1) {
    int to_i = yqi(ToDate[0]) - index_min_i;
    const double to_x = index[to_i];
    FORLOOP({
      int from_i = yqi(FromDate[i]) - index_min_i;
      double from_x = index[from_i];
      ansp[i] *= to_x / from_x;
    })
  } else if (N_from == 1 && N_to == N) {
    int from_i = yqi(FromDate[0]) - index_min_i;
    const double from_x = index[from_i];
    FORLOOP({
      int to_i = yqi(ToDate[i]) - index_min_i;
      double to_x = index[to_i];
      ansp[i] *= to_x / from_x;
    })
  }
}

int ymi(YearMonth YM) {
  // 12 *
  return (YM.year << 3) + (YM.year << 2) + YM.month - 1;
}

void InflateMonthly(double * restrict ansp, R_xlen_t N, int nThread,
                    YearMonth * FromDate,
                    YearMonth * ToDate,
                    R_xlen_t N_from,
                    R_xlen_t N_to,
                    const double * index, YearMonth index_min) {
  const int index_min_i = ymi(index_min);
  if (N_from == N && N_to == N) {
    FORLOOP({
      int from_i = ymi(FromDate[i]) - index_min_i;
      int to_i = ymi(ToDate[i]) - index_min_i;
      double from_x = index[from_i];
      double to_x = index[to_i];

      ansp[i] *= to_x / from_x;
    })
  } else if (N_from == N && N_to == 1) {
    int to_i = ymi(ToDate[0]) - index_min_i;
    const double to_x = index[to_i];
    FORLOOP({
      int from_i = ymi(FromDate[i]) - index_min_i;
      double from_x = index[from_i];
      ansp[i] *= to_x / from_x;
    })
  } else if (N_from == 1 && N_to == N) {
    int from_i = ymi(FromDate[0]) - index_min_i;
    const double from_x = index[from_i];
    FORLOOP({
      int to_i = ymi(ToDate[i]) - index_min_i;
      double to_x = index[to_i];
      ansp[i] *= to_x / from_x;
    })
  }
}

void InflateYearly(double * restrict ansp, R_xlen_t N, int nThread,
                   YearMonth * FromDate,
                   YearMonth * ToDate,
                   R_xlen_t N_from,
                   R_xlen_t N_to,
                   const double * index, YearMonth index_min) {
  const int index_min_year = index_min.year;
  const int index_min_month = index_min.month;
  if (N_from == N && N_to == N) {
    FORLOOP({
      int from_i = FromDate[i].year - index_min_year - (FromDate[i].month < index_min_month);
      int to_i = ToDate[i].year - index_min_year - (ToDate[i].month < index_min_month);
      double from_x = index[from_i];
      double to_x = index[to_i];
      ansp[i] *= to_x / from_x;
    })
  } else if (N_from == N && N_to == 1) {
    int to_i = ToDate[0].year - index_min_year - (ToDate[0].month < index_min_month);
    const double to_x = index[to_i];
    FORLOOP({
      int from_i = FromDate[i].year - index_min_year - (FromDate[i].month < index_min_month);
      double from_x = index[from_i];
      ansp[i] *= to_x / from_x;
    })
  } else if (N_from == 1 && N_to == N) {
    int from_i = FromDate[0].year - index_min_year - (FromDate[0].month < index_min_month);
    const double from_x = index[from_i];
    FORLOOP({
      int to_i = ToDate[i].year - index_min_year - (ToDate[i].month < index_min_month);
      double to_x = index[to_i];
      ansp[i] *= to_x / from_x;
    })
  }
}



SEXP C_Inflate(SEXP From, SEXP To, SEXP Index, SEXP IndexMinIDate, SEXP IndexFreq,
               SEXP FyMonth,
               SEXP x,
               SEXP FromClass, SEXP ToClass,
               SEXP FromConstantForm, SEXP ToConstantForm,
               SEXP nthreads) {
  int nThread = as_nThread(nthreads);
  R_xlen_t N_from = xlength(From);
  R_xlen_t N_to = xlength(To);
  R_xlen_t N = N_from >= N_to ? N_from : N_to;

  bool x_was_null = TYPEOF(x) == NILSXP;

  if (!x_was_null) {
    if (!isReal(x)) {
      error("`x` was type '%s' but must be a REALSXP", type2char(TYPEOF(x)));
    }
    if (xlength(x) != N) {
      error("`length(x) = %lld` but `%lld` was expected", xlength(x), N);
    }
  }

  int from_class = asInteger(FromClass);
  int to_class = asInteger(ToClass);
  int MonthFY = asInteger(FyMonth);
  if (MonthFY < 1 || MonthFY > 12) {
    MonthFY = 3;
  }
  if (!isReal(Index)) {
    error("Index wasn't REALSXP which is not supported.");
  }
  bool from_constant_form = asLogical(FromConstantForm);
  bool to_constant_from = asLogical(ToConstantForm);


  YearMonth * FromDate = malloc(sizeof(YearMonth) * N_from);
  YearMonth * ToDate = malloc(sizeof(YearMonth) * N_to);
  if (FromDate == NULL || ToDate == NULL) {
    free(FromDate);
    free(ToDate);
    error("Could not malloc.");
  }


  int index_min = asInteger(IndexMinIDate);
  YearMonth index_min_ym = idate2YearMonth(index_min);

  const double * index = REAL(Index);
  int freq = index_freq2int(IndexFreq);


  SEXP2YearMonth(FromDate, From, from_class, from_constant_form, true, MonthFY, false, "from", nThread);
  SEXP2YearMonth(ToDate, To, to_class, to_constant_from, true, MonthFY, false, "to", nThread);

  SEXP ans = PROTECT(isNull(x) ? allocVector(REALSXP, N) : x);
  double * restrict ansp = REAL(ans);
  if (x_was_null) {
    FORLOOP({
      ansp[i] = 1; // we use the product
    })
  }

  switch(freq) {
  case 1:
    InflateYearly(ansp, N, nThread, FromDate, ToDate, N_from, N_to, index, index_min_ym);
    break;
  case 4:
    InflateQuarterly(ansp, N, nThread, FromDate, ToDate, N_from, N_to, index, index_min_ym);
    break;
  case 12:
    InflateMonthly(ansp, N, nThread, FromDate, ToDate, N_from, N_to, index, index_min_ym);
    break;
  }
  free(FromDate);
  free(ToDate);
  UNPROTECT(1);
  return ans;
}

SEXP C_YearMonthSplit(SEXP x, SEXP xClass, SEXP MonthFY, SEXP nthreads) {
  R_xlen_t N = xlength(x);
  int x_class = asInteger(xClass);
  int month_fy = asInteger(MonthFY);
  int nThread = as_nThread(nthreads);
  YearMonth * x_YM = malloc(sizeof(YearMonth) * N);
  if (x_YM == NULL) {
    free(x_YM);
    return R_NilValue;
  }

  SEXP2YearMonth(x_YM, x, x_class, true, true, month_fy, false, "x", nThread);
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








