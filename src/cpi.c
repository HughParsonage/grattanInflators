#include "grattanInflator.h"



unsigned char isnt_supported_IDate(int x) {
  return (x < MIN_IDATE) | (x > MAX_IDATE);
}



unsigned char all_within_daterange(const int * xp, R_xlen_t N, int nThread) {
  unsigned char o = 0;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) schedule(static) reduction(| : o)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    o |= isnt_supported_IDate(xp[i]);
  }
  return o;
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


static int string2month(const char * x) {
  // can't see a faster way to both validate and mark
  switch(x[5]) {
  case '0':
    switch(x[6]) {
    case '1':
      return 1;
    case '2':
      return 2;
    case '3':
      return 3;
    case '4':
      return 4;
    case '5':
      return 5;
    case '6':
      return 6;
    case '7':
      return 7;
    case '8':
      return 8;
    case '9':
      return 9;
    default:
      return 15;
    }
    break;
  case '1':
    switch(x[6]) {
    case '0':
      return 10;
    case '1':
      return 11;
    case '2':
      return 12;
    }
    break;
  default:
    return 15;
  }
  return 15;
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

static YearMonth NA_YM() {
  YearMonth O;
  O.year = 127;
  O.month = 15; // nonsense values
  return O;
}



static void string2YearMonth(unsigned char * err,
                             YearMonth * ans,
                             const char * x, int n) {
  if ((n != 10 && n != 7) || !starts_with_yyyy(x)) {
    ans->year = 127;
    ans->month = 15;
    *err = 1;
    return;
  }
  ans->year = string2year(x);

  switch(n) {
  case 10:
    ans->month = string2month(x);
    break;
  case 7:
    if (is_valid_fy_quartet(x)) {
      ans->year++;
      ans->month = 3;
    } else {
      ans->month = 15;
      *err = 2;
    }
    break;
  }
}







static void SEXP2YearMonth(unsigned char * err,
                           YearMonth * ansp,
                           SEXP x,
                           int x_class,
                           bool constant_only, bool prefer_fy,
                           int fy_month,
                           bool check_day, const char * var, int nThread) {
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
      if (all_within_daterange(xp, N, nThread)) {
        FORLOOP({
          ansp[i] = idate2YearMonth(xp[i]);
        })
      } else {
        err[0] = ERR_IDATE_OUT_OF_RANGE;
      }
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
  unsigned char err_ = err[0];

#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(| : err_)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    YearMonth O;
    string2YearMonth(&err_, &O, CHAR(xp[i]), length(xp[i]));
    ansp[i] = O;
  }
  err[0] = err_;
}

static int index_freq2int(SEXP IndexFreq) {
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

static R_xlen_t find_err(YearMonth * xp, R_xlen_t N) {
  for (R_xlen_t i = 0; i < N; ++i) {
    if (xp[i].month > 12) {
      return i + 1;
    }
  }
  return 0;
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

  unsigned char err = 0;

  SEXP2YearMonth(&err, FromDate, From, from_class, from_constant_form, true, MonthFY, false, "from", nThread);
  if (err != 0) {
    R_xlen_t j_err = find_err(FromDate, N_from);
    free(FromDate);
    free(ToDate);
    error("`from` contained element '%s' at position %lld, not in valid form.",
          CHAR(STRING_ELT(From, j_err)), j_err);
  }
  SEXP2YearMonth(&err, ToDate, To, to_class, to_constant_from, true, MonthFY, false, "to", nThread);
  if (err != 0) {
    R_xlen_t j_err = find_err(ToDate, N_to);
    free(FromDate);
    free(ToDate);
    error("`to` contained element '%s' at position %lld, not in valid form.",
          CHAR(STRING_ELT(To, j_err)), j_err);
  }

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
  unsigned char err = 0;
  SEXP2YearMonth(&err, x_YM, x, x_class, true, true, month_fy, false, "x", nThread);
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








