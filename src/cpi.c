#include "grattanInflator.h"

static int index_freq2int(SEXP IndexFreq) {
  switch(TYPEOF(IndexFreq)) {
  case INTSXP:
  case REALSXP:
    return asInteger(IndexFreq);
  }
  return 0; // # nocov
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
                      const double * index, R_xlen_t index_n, YearMonth index_min) {
  const int index_min_i = yqi(index_min);
  if (N_from == N && N_to == N) {
    FORLOOP({
      if (is_YMNA(FromDate[i]) || is_YMNA(ToDate[i])) {
        ansp[i] = NaN;
        continue;
      }
      int from_i = yqi(FromDate[i]) - index_min_i;
      int to_i = yqi(ToDate[i]) - index_min_i;

      if (IDX_OOB(from_i, index_n) || IDX_OOB(to_i, index_n)) {
        ansp[i] = NaN;
        continue;
      }
      double from_x = index[from_i];
      double to_x = index[to_i];

      ansp[i] *= to_x / from_x;
    })
  } else if (N_from == N && N_to == 1) {
    int to_i = yqi(ToDate[0]) - index_min_i;
    if (is_YMNA(ToDate[0]) || IDX_OOB(to_i, index_n)) {
      FORLOOP({
        ansp[i] = NaN;
      })
      return;
    }
    const double to_x = index[to_i];
    FORLOOP({
      if (is_YMNA(FromDate[i])) {
        ansp[i] = NaN;
        continue;
      }
      int from_i = yqi(FromDate[i]) - index_min_i;
      if (IDX_OOB(from_i, index_n)) {
        ansp[i] = NaN;
        continue;
      }
      double from_x = index[from_i];
      ansp[i] *= to_x / from_x;
    })
  } else if (N_from == 1 && N_to == N) {
    int from_i = yqi(FromDate[0]) - index_min_i;
    if (is_YMNA(FromDate[0]) || IDX_OOB(from_i, index_n)) {
      FORLOOP({
        ansp[i] = NaN;
      })
      return;
    }
    const double from_x = index[from_i];
    FORLOOP({
      if (is_YMNA(ToDate[i])) {
        ansp[i] = NaN;
        continue;
      }
      int to_i = yqi(ToDate[i]) - index_min_i;
      if (IDX_OOB(to_i, index_n)) {
        ansp[i] = NaN;
        continue;
      }
      double to_x = index[to_i];
      ansp[i] *= to_x / from_x;
    })
  }
}

void InflateMonthly(double * restrict ansp, R_xlen_t N, int nThread,
                    YearMonth * FromDate,
                    YearMonth * ToDate,
                    R_xlen_t N_from,
                    R_xlen_t N_to,
                    const double * index, R_xlen_t index_n, YearMonth index_min) {
  const int index_min_i = ymi(index_min);
  if (N_from == N && N_to == N) {
    FORLOOP({
      if (is_YMNA(FromDate[i]) || is_YMNA(ToDate[i])) {
        ansp[i] = NaN;
        continue;
      }
      int from_i = ymi(FromDate[i]) - index_min_i;
      int to_i = ymi(ToDate[i]) - index_min_i;
      if (IDX_OOB(from_i, index_n) || IDX_OOB(to_i, index_n)) {
        ansp[i] = NaN;
        continue;
      }
      double from_x = index[from_i];
      double to_x = index[to_i];

      ansp[i] *= to_x / from_x;
    })
  } else if (N_from == N && N_to == 1) {
    int to_i = ymi(ToDate[0]) - index_min_i;
    if (is_YMNA(ToDate[0]) || IDX_OOB(to_i, index_n)) {
      FORLOOP({
        ansp[i] = NaN;
      })
      return;
    }
    const double to_x = index[to_i];
    FORLOOP({
      if (is_YMNA(FromDate[i])) {
        ansp[i] = NaN;
        continue;
      }
      int from_i = ymi(FromDate[i]) - index_min_i;
      if (IDX_OOB(from_i, index_n)) {
        ansp[i] = NaN;
        continue;
      }
      double from_x = index[from_i];
      ansp[i] *= to_x / from_x;
    })
  } else if (N_from == 1 && N_to == N) {
    int from_i = ymi(FromDate[0]) - index_min_i;
    if (is_YMNA(FromDate[0]) || IDX_OOB(from_i, index_n)) {
      FORLOOP({
        ansp[i] = NaN;
      })
      return;
    }
    const double from_x = index[from_i];
    FORLOOP({
      if (is_YMNA(ToDate[i])) {
        ansp[i] = NaN;
        continue;
      }
      int to_i = ymi(ToDate[i]) - index_min_i;
      if (IDX_OOB(to_i, index_n)) {
        ansp[i] = NaN;
        continue;
      }
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
                   const double * index, R_xlen_t index_n, YearMonth index_min) {
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
      if (IDX_OOB(from_i, index_n) || IDX_OOB(to_i, index_n)) {
        ansp[i] = NaN;
        continue;
      }
      double from_x = index[from_i];
      double to_x = index[to_i];
      ansp[i] *= to_x / from_x;
    })
  } else if (N_from == N && N_to == 1) {
    int to_i = ToDate[0].year - index_min_year - (ToDate[0].month < index_min_month);
    if (is_YMNA(ToDate[0]) || IDX_OOB(to_i, index_n)) {
      FORLOOP({
        ansp[i] = NaN;
      })
      return;
    }
    const double to_x = index[to_i];
    FORLOOP({
      if (is_YMNA(FromDate[i])) {
        ansp[i] = NaN;
        continue;
      }
      int from_i = FromDate[i].year - index_min_year - (FromDate[i].month < index_min_month);
      if (IDX_OOB(from_i, index_n)) {
        ansp[i] = NaN;
        continue;
      }
      double from_x = index[from_i];
      ansp[i] *= to_x / from_x;
    })
  } else if (N_from == 1 && N_to == N) {
    int from_i = FromDate[0].year - index_min_year - (FromDate[0].month < index_min_month);
    if (is_YMNA(FromDate[0]) || IDX_OOB(from_i, index_n)) {
      FORLOOP({
        ansp[i] = NaN;
      })
      return;
    }
    const double from_x = index[from_i];
    FORLOOP({
      if (is_YMNA(ToDate[i])) {
        ansp[i] = NaN;
        continue;
      }
      int to_i = ToDate[i].year - index_min_year - (ToDate[i].month < index_min_month);
      if (IDX_OOB(to_i, index_n)) {
        ansp[i] = NaN;
        continue;
      }
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
              (long long)xlength(x), (long long)N);
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
          type2char(TYPEOF(Index)), (long long)xlength(Index));
  }
  // # nocov end
  if (xlength(Index) == 0) {
    error("`index` had zero values, so no inflator can be computed."); // # nocov
  }


  // malloc(0) may legitimately return NULL, which must not be an error
  YearMonth * FromDate = malloc(sizeof(YearMonth) * (N_from ? N_from : 1));
  YearMonth * ToDate = malloc(sizeof(YearMonth) * (N_to ? N_to : 1));
  // # nocov start
  if (FromDate == NULL || ToDate == NULL) {
    free(FromDate);
    free(ToDate);
    error("Could not malloc.");
  }
  // # nocov end

  YearMonth index_min_ym = idate2YearMonth(index_min);

  const double * index = REAL(Index);
  const R_xlen_t index_n = xlength(Index);
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
    InflateYearly(ansp, N, nThread, FromDate, ToDate, N_from, N_to, index, index_n, index_min_ym);
    break;
  case 4:
    InflateQuarterly(ansp, N, nThread, FromDate, ToDate, N_from, N_to, index, index_n, index_min_ym);
    break;
  case 12:
    InflateMonthly(ansp, N, nThread, FromDate, ToDate, N_from, N_to, index, index_n, index_min_ym);
    break;
  }
  free(FromDate);
  free(ToDate);
  UNPROTECT(1);
  return ans;
}
