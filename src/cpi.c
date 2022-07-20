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


static YearMonth NA_YM() {
  YearMonth O;
  O.year = 127;
  O.month = 15; // nonsense values
  return O;
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
