#include "grattanInflator.h"

static void iminmax(int xminmax[2], const int * xp, R_xlen_t N, int nThread, bool narm) {
  int xmin = xp[0];
  int xmax = xp[0];
  if (narm) {
    if (xmin == NA_INTEGER) {
      xmin = INT_MAX; // so first non-NA will become candidate min
    }
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(min : xmin) reduction(max : xmax)
#endif
    for (R_xlen_t i = 1; i < N; ++i) {
      int xi = xp[i];
      bool nochange = (xi == NA_INTEGER) || (xi >= xmin && xi <= xmax);
      if (nochange) continue;
      xmin = (xi < xmin) ? xi : xmin;
      xmax = (xi > xmax) ? xi : xmax;
    }
  } else {
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(min : xmin) reduction(max : xmax)
#endif
  for (R_xlen_t i = 1; i < N; ++i) {
    int xi = xp[i];
    bool nochange = xi >= xmin && xi <= xmax;
    if (nochange) continue;
    xmin = (xi < xmin) ? xi : xmin;
    xmax = (xi > xmax) ? xi : xmax;
  }
  xminmax[0] = xmin;
  xminmax[1] = xmax;
}
}

static unsigned char prior_cpi_orig(const char * x) {
  // known to be 1900-2099, check > 1948-09
  return 1;
}

static unsigned char prior_cpi_orig_date(int x) {
  // known to be 1900-2099, check > 1948-09-01
  return x >= -7792;
}

static unsigned char prior_cpi_trim(const char * x) {
  // known to be 1900-2099, check > 2002
  return x[0] == '1' || (x[2] == '0' && x[3] < '3');
}

static unsigned char prior_cpi_trim_date(int x) {
  // known to be 1900-2099, check > 2002-06-01
  return x >= 11839;
}

static unsigned char prior_cpi_seas(const char * x) {
  // known to be 1900-2099, check > 1986
  return x[0] == '1' && x[2] <= '8' &&
    // 1986-12-01 and 1986-87 are ok
    (x[2] < '8' ||
      (x[3] < '7' &&
        (x[5] != '1' || x[6] != '2') &&
        (x[5] != '8' || x[6] != '7')));
}

static unsigned char prior_cpi_seas_date(int x) {
  // known to be 1900-2099, check > 1948-09-01
  return x >= 6268;
}

static unsigned char prior_lfi(const char * x) {
  // before 1979
  return x[0] == '1' && x[2] <= '7' && (x[2] < '7' || x[3] == '9');
}

static unsigned char prior_wpi(const char * x) {
  // before 1997
  return x[0] == '1' && (x[2] != '9' || x[3] < '7');
}



unsigned char isnt_supported_IDate(int x) {
  return (x < MIN_IDATE) | (x > MAX_IDATE);
}









unsigned char any_outside_daterange(const int * xp, R_xlen_t N, int nThread) {
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




int string2year(const char * x) {
  // no check on whether string length is adequate, or starts with digits
  int year = 0;
  year += (x[1] == '9') ? 1900 : 2000;
  year += 10 * (x[2] - '0');
  year += x[3] - '0';
  return year - 1948;
}


int string2month(const char * x) {
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



static YearMonth NA_YM() {
  YearMonth O;
  O.year = 127;
  O.month = 15; // nonsense values
  return O;
}

static void string2YearMonth(unsigned char * err,
                             YearMonth * ans,
                             const char * x, int n) {
  ans->year = string2year(x);

  switch(n) {
  case 10:
    ans->month = string2month(x);
    break;
  case 7:
    ans->year++;
    ans->month = 3;

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
      if (any_outside_daterange(xp, N, nThread)) {
        err[0] = ERR_IDATE_OUT_OF_RANGE;

      } else {
        FORLOOP({
          ansp[i] = idate2YearMonth(xp[i]);
        })
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
    switch(err) {
    case ERR_BADFORM:
      error("`from` contained element '%s' at position %lld, not in valid form.",
            CHAR(STRING_ELT(From, j_err)), j_err);
    case ERR_IDATE_OUT_OF_RANGE:
      error("`from` contained element not in valid range.");
    }
  }
  SEXP2YearMonth(&err, ToDate, To, to_class, to_constant_from, true, MonthFY, false, "to", nThread);
  if (err != 0) {
    R_xlen_t j_err = find_err(ToDate, N_to);
    free(FromDate);
    free(ToDate);
    switch(err) {
    case ERR_BADFORM:
      error("`to` contained element '%s' at position %lld, not in valid form.",
            CHAR(STRING_ELT(From, j_err)), j_err);
    case ERR_IDATE_OUT_OF_RANGE:
      error("`to` contained element not in valid range.");
    }
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

static unsigned int p_search_string(const char * x, int n, int freq) {
  unsigned int px = (n == 10) ? p_search_string10(x) : p_search_string7_unsafe(x);
  if (freq <= 4) {
    px /= 3;
    if (freq == 1) {
      px /= 4;
    }
  }
  return px;
}

static unsigned int p_search_int(int x, int freq) {
  unsigned int px = p_search(x);
  if (freq <= 4) {
    px /= 3;
    if (freq == 1) {
      px /= 4;
    }
  }
  return px;
}

// string-string (equilength)
static void inflate4_SS(double * restrict ansp, R_xlen_t N, int nThread,
                        const SEXP * xp,
                        const SEXP * yp,
                        const double * v,
                        const unsigned int p_index_min,
                        const int freq) {
  FORLOOP({
    const char * xpi = CHAR(xp[i]);
    const char * ypi = CHAR(yp[i]);

    int nxpi = length(xp[i]);
    if (nxpi != 10 && nxpi != 7) {
      ansp[i] = NA_REAL;
      continue;
    }
    int nypi = length(yp[i]);
    if (nypi != 10 && nypi != 7) {
      ansp[i] = NA_REAL;
    }
    unsigned int px = p_search_string(xpi, nxpi, freq);
    unsigned int py = p_search_string(ypi, nypi, freq);
    px -= p_index_min;
    py -= p_index_min;
    double vx = v[px];
    double vy = v[py];
    ansp[i] = vy / vx;
  })
}

// string-string (equilength)
static void inflate4_Ss(double * restrict ansp, R_xlen_t N, int nThread,
                        const SEXP * xp,
                        const SEXP * yp,
                        const double * v,
                        const unsigned int p_index_min,
                        const int freq,
                        bool deflate) {
  const char * ypi = CHAR(yp[0]);

  const int nypi = length(yp[0]);
  if (nypi != 10 && nypi != 7) {
    FORLOOP({
      ansp[i] = NA_REAL;
    })
    return;
  }
  unsigned int py = p_search_string(ypi, nypi, freq);
  py -= p_index_min;
  const double vy = v[py];

  FORLOOP({
    const char * xpi = CHAR(xp[i]);
    int nxpi = length(xp[i]);
    if (nxpi != 10 && nxpi != 7) {
      ansp[i] = NA_REAL;
      continue;
    }
    unsigned int px = p_search_string(xpi, nxpi, freq);
    px -= p_index_min;
    double vx = v[px];
    ansp[i] = deflate ? (vx / vy) : (vy / vx);
  })
}

static void inflate4_SI(double * restrict ansp, R_xlen_t N, int nThread,
                        const SEXP * xp,
                        const int * yp,
                        const double * v,
                        const unsigned int p_index_min,
                        const int freq,
                        bool deflate) {
  FORLOOP({
    const char * xpi = CHAR(xp[i]);
    int nxpi = length(xp[i]);
    if (nxpi != 10 && nxpi != 7) {
      ansp[i] = NA_REAL;
      continue;
    }
    unsigned int px = p_search_string(xpi, nxpi, freq);
    px -= p_index_min;

    unsigned int py = p_search(yp[i]);
    double vx = v[px];
    double vy = v[py];
    ansp[i] = deflate ? (vx / vy) : (vy / vx);
  })
}

static void inflate4_Si(double * restrict ansp, R_xlen_t N, int nThread,
                        const SEXP * xp,
                        const int yp,
                        const double * v,
                        const unsigned int p_index_min,
                        const int freq,
                        bool deflate) {
  unsigned int py = p_search(yp);
  if (freq <= 4) {
    py /= 3;
    if (freq == 1) {
      py /= 4;
    }
  }
  py -= p_index_min;
  const double vy = v[py];

  FORLOOP({
    const char * xpi = CHAR(xp[i]);
    int nxpi = length(xp[i]);
    if (nxpi != 10 && nxpi != 7) {
      ansp[i] = NA_REAL;
      continue;
    }
    unsigned int px = p_search_string(xpi, nxpi, freq);
    px -= p_index_min;
    double vx = v[px];
    ansp[i] = deflate ? (vx / vy) : (vy / vx);
  })
}

static void inflate4_Is(double * restrict ansp, R_xlen_t N, int nThread,
                        const int * xp,
                        const SEXP * yp,
                        const double * v,
                        const unsigned int p_index_min,
                        const int freq,
                        bool deflate) {
  unsigned int py = p_search_string(CHAR(yp[0]), length(yp[0]), freq);
  if (freq <= 4) {
    py /= 3;
    if (freq == 1) {
      py /= 4;
    }
  }
  py -= p_index_min;
  const double vy = v[py];

  FORLOOP({
    unsigned int px = p_search(xp[i]);
    if (freq <= 4) {
      px /= 3;
      if (freq == 1) {
        px /= 4;
      }
    }
    px -= p_index_min;
    double vx = v[px];
    ansp[i] = deflate ? (vx / vy) : (vy / vx);
  })
}

static void inflate4_II(double * restrict ansp, R_xlen_t N, int nThread,
                        const int * xp,
                        const int * yp,
                        const double * v,
                        const unsigned int p_index_min,
                        const int freq) {
  FORLOOP({
    unsigned int px = p_search_int(xp[i], freq);
    unsigned int py = p_search_int(yp[i], freq);
    double vy = v[py];
    double vx = v[px];
    ansp[i] = vy / vx;
  })
}

static void inflate4_Ii(double * restrict ansp, R_xlen_t N, int nThread,
                        const int * xp,
                        const int yp,
                        const double * v,
                        const unsigned int p_index_min,
                        const int freq,
                        bool deflate) {
  unsigned int py = p_search_int(yp, freq);
  const double vy = v[py];
  FORLOOP({
    unsigned int px = p_search_int(xp[i], freq);
    double vx = v[px];
    ansp[i] = deflate ? (vx / vy) : (vy / vx);
  })
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



static void check_char(const SEXP * xp, R_xlen_t N, int nThread, const char * var,
                       const int series) {
  unsigned char * err[1] = {0};
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
    // o |= err_string(xi, n);
    if (o == 0) {
      switch(series) {
      case CPI_ORIG:
        o |= prior_cpi_orig(xi);
        break;
      case CPI_SEAS:
        o |= prior_cpi_seas(xi);
        break;
      case CPI_TRIM:
        o |= prior_cpi_trim(xi);
        break;
      case WPI_ORIG:
        o |= prior_wpi(xi);
        break;
      case LFI_ORIG:
        o |= prior_lfi(xi);
        break;
      }
    }
  }
  if (o != 0) {
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xp[i] == NA_STRING) {
        continue;
      }
      const char * xi = CHAR(xp[i]);
      int nxi = length(xp[i]);
      if (err_string(xi, nxi)) {
        error("`%s` contained invalid element:\n\t %s[%lld] = %s",
              var, var, i + 1, CHAR(xp[i]));
      }
      switch(series) {
      case CPI_ORIG:
        if (prior_cpi_orig(xi)) {
          error("`%s` contained element\n\t%s[%lld] = %s\nwhich is prior to the earliest permitted date (%s)",
                var, var, i + 1, xi, "1948-09-01");
        }
        break;
      case CPI_SEAS:
        if (prior_cpi_seas(xi)) {
          error("`%s` contained element\n\t%s[%lld] = %s\nwhich is prior to the earliest permitted date (%s)",
                var, var, i + 1, xi, "1986-12-01");
        }
        break;
      case CPI_TRIM:
        if (prior_cpi_trim(xi)) {
          error("`%s` contained element\n\t%s[%lld] = %s\nwhich is prior to the earliest permitted date (%s)",
                var, var, i + 1, xi, "2002-03-01");
        }
        break;
      case WPI_ORIG:
        if (prior_wpi(xi)) {
          error("`%s` contained element\n\t%s[%lld] = %s\nwhich is prior to the earliest permitted date (%s)",
                var, var, i + 1, xi, "1997-09-01");
        }
        break;
      case LFI_ORIG:
        if (prior_lfi(xi)) {
          error("`%s` contained element\n\t%s[%lld] = %s\nwhich is prior to the earliest permitted date (%s)",
                var, var, i + 1, xi, "1978-02-01");
        }
        break;
      }
    }
  }
}

static void ierr_if_min(int xmin, int min_allowed, const char * var, const int * xp) {
  R_xlen_t i = 0;
  if (xmin < min_allowed) {
    while (xp[i] >= min_allowed) {
      ++i;
    }
    error("`%s` contained element\n\t%s[%lld] = %d\nwhich is prior to the earliest permitted year (%d)",
          var, var, i + 1, xmin, min_allowed);
  }
}

static void check_int(const int * xp, R_xlen_t N, int nThread, const char * var, const int series) {
  int xminmax[2] = {0};
  iminmax(xminmax, xp, N, nThread, true);
  const int xmin = xminmax[0];
  R_xlen_t i = -1;
  switch(series) {
  case CPI_ORIG:
    ierr_if_min(xmin, 1948, var, xp);
    break;
  case CPI_SEAS:
    ierr_if_min(xmin, 1986, var, xp);
    break;
  case CPI_TRIM:
    ierr_if_min(xmin, 2002, var, xp);
    break;
  case WPI_ORIG:
    ierr_if_min(xmin, 1997, var, xp);
    break;
  case LFI_ORIG:
    ierr_if_min(xmin, 1978, var, xp);
    break;
  }
  const int xmax = xminmax[1];
  if (xmax >= MAX_YEAR) {
    R_xlen_t i = 0;
    if (xmin > MAX_YEAR) {
      while (xp[i] >= MAX_YEAR) {
        ++i;
      }
      error("`%s` contained element\n\t%s[%lld] = %d\nwhich is after the latest permitted year (%d)",
            var, var, i + 1, xmin, MAX_YEAR);
    }
  }
}
static void check_idate(const int * xp, R_xlen_t N, int nThread, const char * var, const int series) {
  int xminmax[2] = {0};
  iminmax(xminmax, xp, N, nThread, true);
  const int xmin = xminmax[0];
  R_xlen_t i = -1;
}

bool anyPrior(const SEXP * xp, R_xlen_t N, const char * y) {
  register char y2 = y[2];
  register char y3 = y[3];
  if (y[0] == '1') {
    for (R_xlen_t i = 0; i < N; ++i) {
      const char * xi = CHAR(xp[i]);
      if (xi[0] == '1') {
        if (xi[2] > y2) {
          continue;
        }
        if (xi[2] < y2) {
          return true;
        }
        if (xi[3] > y3) {
          continue;
        }
        if (xi[3] < y3) {
          return true;
        }
        if (xi[5] < y[5]) {
          return true;
        }
        if (xi[5] > y[5]) {
          continue;
        }
        if (xi[6] < y[6]) {
          return true;
        }
      }
    }
  }
  for (R_xlen_t i = 0; i < N; ++i) {
    const char * xi = CHAR(xp[i]);
    if (xi[0] == '1') {
      return true;
    }
    if (xi[2] > y2) {
      continue;
    }
    if (xi[2] < y2) {
      return true;
    }
    if (xi[3] > y3) {
      continue;
    }
    if (xi[3] < y3) {
      return true;
    }
    if (xi[5] < y[5]) {
      return true;
    }
    if (xi[5] > y[5]) {
      continue;
    }
    if (xi[6] < y[6]) {
      return true;
    }
  }
  return false;
}

SEXP C_anyPrior(SEXP x, SEXP y) {
  if (!isString(x) || !isString(y) || length(y) != 1) {
    return R_NilValue;
  }
  R_xlen_t N = xlength(x);
  const SEXP * xp = STRING_PTR(x);
  const char * yp = CHAR(STRING_ELT(y, 0));

  return ScalarLogical(anyPrior(STRING_PTR(x), xlength(x), yp));
}
SEXP C_inflate4(SEXP From, SEXP To, SEXP nthreads, SEXP Index, SEXP IndexMinIDate,
                SEXP Freq) {
  R_xlen_t N_from = xlength(From);
  R_xlen_t N_to = xlength(To);
  R_xlen_t N = N_from >= N_to ? N_from : N_to;

  int n_index = length(Index);
  int index_min = asInteger(IndexMinIDate);
  YearMonth index_min_ym = idate2YearMonth(index_min);
  const unsigned int p_index_min = p_search(index_min);
  int freq = asInteger(Freq);

  const double * index = REAL(Index);

  SEXP ans = PROTECT(allocVector(REALSXP, N));
  double * restrict ansp = REAL(ans);
  int nThread = as_nThread(nthreads);


  switch(TYPEOF(From)) {
  case STRSXP:
    switch(TYPEOF(To)) {
    case STRSXP:
      if (xlength(From) == xlength(To)) {
        inflate4_SS(ansp, N, nThread, STRING_PTR(From), STRING_PTR(To), index, p_index_min, freq);
      } else {
        if (xlength(From) == 1) {
          inflate4_Ss(ansp, N, nThread, STRING_PTR(To), STRING_PTR(From), index, p_index_min, freq, true);
        } else if (xlength(To) == 1) {
          inflate4_Ss(ansp, N, nThread, STRING_PTR(From), STRING_PTR(To), index, p_index_min, freq, false);
        }
      }
      break;
    case INTSXP:
      if (xlength(From) == xlength(To)) {
        inflate4_SI(ansp, N, nThread, STRING_PTR(From), INTEGER(To), index, p_index_min, freq, false);
      } else if (xlength(From) == 1) {
        inflate4_Si(ansp, N, nThread, STRING_PTR(From), asInteger(To), index, p_index_min, freq, false);
      } else if (xlength(To) == 1) {
        inflate4_Is(ansp, N, nThread, INTEGER(To), STRING_PTR(From), index, p_index_min, freq, true);
      }
      break;
    }
    break;
  case INTSXP:
    switch(TYPEOF(To)) {
    case STRSXP:
      if (xlength(From) == xlength(To)) {
        inflate4_SI(ansp, N, nThread, STRING_PTR(To), INTEGER(From), index, p_index_min, freq, true);
      } else if (xlength(From) == 1) {
        inflate4_Si(ansp, N, nThread, STRING_PTR(To), asInteger(From), index, p_index_min, freq, true);
      } else if (xlength(To) == 1) {
        inflate4_Is(ansp, N, nThread, INTEGER(From), STRING_PTR(To), index, p_index_min, freq, false);
      }
      break;
    case INTSXP:
      if (xlength(From) == xlength(To)) {
        inflate4_II(ansp, N, nThread, INTEGER(From), INTEGER(To), index, p_index_min, freq);
      } else if (xlength(From) == 1) {
        inflate4_Ii(ansp, N, nThread, INTEGER(From), asInteger(To), index, p_index_min, freq, true);
      } else if (xlength(To) == 1) {
        inflate4_Ii(ansp, N, nThread, INTEGER(To), asInteger(From), index, p_index_min, freq, false);
      }
    }
  }
  UNPROTECT(1);
  return ans;
}

