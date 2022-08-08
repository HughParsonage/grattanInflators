#include "grattanInflator.h"

static int index_freq2int(SEXP IndexFreq) {
  switch(TYPEOF(IndexFreq)) {
  case INTSXP:
  case REALSXP:
    return asInteger(IndexFreq);
  }
  return 0; // # nocov
}

static int MONTH_TO_QUARTER[16] = {0,
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

const bool YMNAs[16] = {1,
                        0, 0, 0, 0,
                        0, 0, 0, 0,
                        0, 0, 0, 0,
                        1, 1, 1};

bool is_YMNA(YearMonth O) {
  return YMNAs[O.month];
}

void InflateQuarterly(double * restrict ansp, R_xlen_t N, int nThread,
                      YearMonth * FromDate,
                      YearMonth * ToDate,
                      R_xlen_t N_from,
                      R_xlen_t N_to,
                      const double * index, YearMonth index_min) {
  const int index_min_i = yqi(index_min);
  if (N_from == N && N_to == N) {
    FORLOOP({
      // # nocov start
      if (is_YMNA(FromDate[i]) || is_YMNA(ToDate[i])) {
        ansp[i] = NaN;
        continue;
      }
      // # nocov end
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
    // # nocov start
    if (is_YMNA(ToDate[0])) {
      FORLOOP({
        ansp[i] = NaN;
      })
      return;
    }
    // # nocov end
    int to_i = yqi(ToDate[0]) - index_min_i;
    const double to_x = index[to_i];
    FORLOOP({
      // # nocov start
      if (is_YMNA(FromDate[i])) {
        ansp[i] = NaN;
        continue;
      }
      // # nocov end
      int from_i = yqi(FromDate[i]) - index_min_i;
      double from_x = index[from_i];
      ansp[i] *= to_x / from_x;
    })
  } else if (N_from == 1 && N_to == N) {
    if (is_YMNA(FromDate[0])) {
      // # nocov start
      FORLOOP({
        ansp[i] = NaN;
      })
      return;
      // # nocov end
    }
    int from_i = yqi(FromDate[0]) - index_min_i;
    const double from_x = index[from_i];
    FORLOOP({
      // # nocov start
      if (is_YMNA(ToDate[i])) {
        ansp[i] = NaN;
        continue;
      }
      // # nocov end
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
      if (is_YMNA(FromDate[i]) || is_YMNA(ToDate[i])) {
        ansp[i] = NaN;
        continue;
      }
      int from_i = ymi(FromDate[i]) - index_min_i;
      int to_i = ymi(ToDate[i]) - index_min_i;
      double from_x = index[from_i];
      double to_x = index[to_i];

      ansp[i] *= to_x / from_x;
    })
  } else if (N_from == N && N_to == 1) {
    if (is_YMNA(ToDate[0])) {
      // # nocov start
      FORLOOP({
        ansp[i] = NaN;
      })
      return;
      // # nocov end
    }
    int to_i = ymi(ToDate[0]) - index_min_i;
    const double to_x = index[to_i];
    FORLOOP({
      if (is_YMNA(FromDate[i])) {
        ansp[i] = NaN;
        continue;
      }
      int from_i = ymi(FromDate[i]) - index_min_i;
      double from_x = index[from_i];
      ansp[i] *= to_x / from_x;
    })
  } else if (N_from == 1 && N_to == N) {
    if (is_YMNA(FromDate[0])) {
      FORLOOP({
        ansp[i] = NaN;
      })
      return;
    }
    int from_i = ymi(FromDate[0]) - index_min_i;
    const double from_x = index[from_i];
    FORLOOP({
      if (is_YMNA(ToDate[i])) {
        ansp[i] = NaN;
        continue;
      }
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
      if (is_YMNA(FromDate[i]) || is_YMNA(ToDate[i])) {
        ansp[i] = NaN;
        continue;
      }
      int from_i = FromDate[i].year - index_min_year - (FromDate[i].month < index_min_month);
      int to_i = ToDate[i].year - index_min_year - (ToDate[i].month < index_min_month);
      double from_x = index[from_i];
      double to_x = index[to_i];
      ansp[i] *= to_x / from_x;
    })
  } else if (N_from == N && N_to == 1) {
    if (is_YMNA(ToDate[0])) {
      FORLOOP({
        ansp[i] = NaN;
      })
      return;
    }
    int to_i = ToDate[0].year - index_min_year - (ToDate[0].month < index_min_month);
    const double to_x = index[to_i];
    FORLOOP({
      if (is_YMNA(FromDate[i])) {
        ansp[i] = NaN;
        continue;
      }
      int from_i = FromDate[i].year - index_min_year - (FromDate[i].month < index_min_month);
      double from_x = index[from_i];
      ansp[i] *= to_x / from_x;
    })
  } else if (N_from == 1 && N_to == N) {
    if (is_YMNA(FromDate[0])) {
      FORLOOP({
        ansp[i] = NaN;
      })
      return;
    }
    int from_i = FromDate[0].year - index_min_year - (FromDate[0].month < index_min_month);
    const double from_x = index[from_i];
    FORLOOP({
      if (is_YMNA(ToDate[i])) {
        ansp[i] = NaN;
        continue;
      }
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
               SEXP nthreads) {
  int nThread = as_nThread(nthreads);
  prohibit_vector_recyling(From, To, "from", "to");
  R_xlen_t N_from = xlength(From);
  R_xlen_t N_to = xlength(To);
  R_xlen_t N = N_from >= N_to ? N_from : N_to;

  bool x_was_null = TYPEOF(x) == NILSXP;

  if (!x_was_null) {
// # nocov start
    if (!isReal(x)) {
      error("`x` was type '%s' but must be a REALSXP", type2char(TYPEOF(x)));
    }
    if (xlength(x) != N) {
      if (N != 1) {
        error("x was type '%s' and `length(x) = %lld` but `%lld` was expected",
              type2char(TYPEOF(x)),
              xlength(x), N);
      }
      N = xlength(x);
    }
    // # nocov end
  }
  int index_min = asInteger(IndexMinIDate);

  int from_class = asInteger(FromClass);
  int to_class = asInteger(ToClass);
  int MonthFY = asInteger(FyMonth);
  // # nocov start
  if (MonthFY < 1 || MonthFY > 12) {
    MonthFY = 3;
  }

  if (!isReal(Index)) {
    error("Index was type '%s' and length-%lld, only REALSXP.",
          type2char(TYPEOF(Index)), xlength(Index));
  }
  // # nocov end


  YearMonth * FromDate = malloc(sizeof(YearMonth) * N_from);
  YearMonth * ToDate = malloc(sizeof(YearMonth) * N_to);
  // # nocov start
  if (FromDate == NULL || ToDate == NULL) {
    free(FromDate);
    free(ToDate);
    error("Could not malloc.");
  }
  // # nocov end

  YearMonth index_min_ym = idate2YearMonth(index_min);

  const double * index = REAL(Index);
  int freq = index_freq2int(IndexFreq);

  SEXP2YearMonth(FromDate, From, from_class, MonthFY, false, "from", nThread);

  SEXP2YearMonth(ToDate, To, to_class, MonthFY, false, "to", nThread);

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
