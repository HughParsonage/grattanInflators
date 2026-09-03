#include "grattanInflator.h"

static inline bool idate_index_position(R_xlen_t * position,
                                        int idate,
                                        unsigned int index_first_month,
                                        unsigned int months_per_period,
                                        R_xlen_t index_len) {
  if (idate == NA_INTEGER || idate < MIN_IDATE || idate > MAX_IDATE) {
    return false;
  }

  const unsigned int month = p_search(idate);
  R_xlen_t offset;
  if (months_per_period == 3) {
    // Quarterly observations are calendar quarters in the generic kernel,
    // including indices dated in their quarter's final month.
    const unsigned int quarter = month / 3;
    const unsigned int first_quarter = index_first_month / 3;
    if (quarter < first_quarter) {
      return false;
    }
    offset = quarter - first_quarter;
  } else {
    // Monthly indices use exact months. Annual indices can be anchored in any
    // month, so their year changes at that anchor rather than in January.
    if (month < index_first_month) {
      return false;
    }
    offset = (month - index_first_month) / months_per_period;
  }
  if (offset >= index_len) {
    return false;
  }

  *position = offset;
  return true;
}




SEXP C_Inflate2(SEXP ans, SEXP From, SEXP To, SEXP Index, SEXP IndexMinIDate, SEXP IndexFreq, SEXP nthreads) {
  int nThread = as_nThread(nthreads);
  if (!isInteger(From) || !isInteger(To)) {
    return R_NilValue; // # nocov
  }
  const int * xp = INTEGER(From);
  const int * yp = INTEGER(To);
  R_xlen_t N_x = xlength(From);
  R_xlen_t N_y = xlength(To);
  if (N_x < N_y) {
    return R_NilValue; // # nocov
  }
  R_xlen_t N = N_x;
  // `ans` is the user's `x`, written to in place. Its type and length must be
  // established here, not merely by the R wrapper: this is an exported native
  // entry point and writing past the end of `x` would corrupt memory.
  if (!isReal(ans)) {
    error("`x` was type '%s' but must be a double vector.", type2char(TYPEOF(ans)));
  }
  if (XLENGTH(ans) != N) {
    error("`length(x) = %lld` but `%lld` values are required (the length of `from`).",
          (long long)XLENGTH(ans), (long long)N);
  }
  if (!isReal(Index)) {
    error("Index was type '%s' REALSXP which is not supported.", type2char(TYPEOF(Index))); // # nocov
  }
  if (xlength(Index) == 0) {
    error("`index` had zero values, so no inflator can be computed."); // # nocov
  }
  const double * index = REAL(Index);
  const R_xlen_t index_len = xlength(Index);

  const int index_min = asInteger(IndexMinIDate);
  if (index_min == NA_INTEGER || index_min < MIN_IDATE || index_min > MAX_IDATE) {
    error("The first index date is outside the supported date range."); // # nocov
  }
  const int freq = asInteger(IndexFreq);
  if (freq != 1 && freq != 4 && freq != 12) {
    error("Index frequency was %d; only annual, quarterly, and monthly indices are supported.", freq); // # nocov
  }
  const unsigned int months_per_period = 12 / freq;
  const unsigned int index_first_month = p_search(index_min);
  double * ansp = REAL(ans);

  if (N_y == 1) {
    R_xlen_t y_p;
    if (!idate_index_position(&y_p, yp[0], index_first_month,
                              months_per_period, index_len)) {
      FORLOOP({
        ansp[i] = NaN;
      })
      return ans;
    }
    const double iyp = index[y_p];
    FORLOOP({
      R_xlen_t x_p;
      if (!idate_index_position(&x_p, xp[i], index_first_month,
                                months_per_period, index_len)) {
        ansp[i] = NaN;
        continue;
      }
      ansp[i] *= iyp / index[x_p];
    })
  } else {
    FORLOOP({
      R_xlen_t x_p;
      R_xlen_t y_p;
      if (!idate_index_position(&x_p, xp[i], index_first_month,
                                months_per_period, index_len) ||
          !idate_index_position(&y_p, yp[i], index_first_month,
                                months_per_period, index_len)) {
        ansp[i] = NaN;
        continue;
      }
      ansp[i] *= index[y_p] / index[x_p];
    })
  }

  return ans;
}

double future_rate_12mo(const double * index, const int freq, const unsigned int index_len) {
  switch(freq) {
  case 1:
    if (index_len < 2) {
      error("(future_rate_12mo)index_len was < 2 for annual series."); // # nocov
    } else {
      return index[index_len - 1] / index[index_len - 2];
    }
    break;
  case 4:
    if (index_len < 5) {
      error("(future_rate_12mo)index_len was < 5 for quarterly series."); // # nocov
    } else {
      return index[index_len - 1] / index[index_len - 5];
    }
    break;
  case 12:
    if (index_len < 13) {
      error("(future_rate_12mo)index_len was < 13 for monthly series."); // # nocov
    } else {
      return index[index_len - 1] / index[index_len - 13];
    }
    break;
  default:
    error("(future_rate_12mo)freq = %d, not supported.", freq); // # nocov
  }
}

SEXP C_coalesce_forecast_12mo_avg(SEXP ans, SEXP From, SEXP To, SEXP Index, SEXP IndexMinIDate, SEXP IndexFreq, SEXP nthreads) {
  int nThread = as_nThread(nthreads);
  if (!isReal(ans)) {
    error("(Internal error):ans was type '%s' but must be double", type2char(TYPEOF(ans))); // # nocov
  }
  if (!isInteger(From) || !isInteger(To)) {
    return R_NilValue; // # nocov
  }


  R_xlen_t N_x = xlength(From);
  R_xlen_t N_y = xlength(To);
  if (N_x < N_y) {
    return R_NilValue; // # nocov
  }
  R_xlen_t N = N_x;
  if (!isReal(Index)) {
    error("Index was type '%s' REALSXP which is not supported.", type2char(TYPEOF(Index))); // # nocov
  }
  const double * index = REAL(Index);
  const unsigned int index_len = length(Index);

  int index_min = asInteger(IndexMinIDate);
  int freq = asInteger(IndexFreq);
  // const unsigned int div = 12 / freq;
  // const unsigned int p_index_min = p_search(index_min) / div;
  double * ansp = REAL(ans);

  const double r_future = future_rate_12mo(index, freq, index_len);
  const double last_index = index[index_len - 1];
  YearMonth index_min_YM = idate2YearMonth(index_min);
  YearMonth index_max_ym = idate2YearMonth(index_min + index_len - 1);
  const int index_max_yr = index_max_ym.year + MIN_YEAR;
  // const int index_max_mo = index_max_ym.month;



  if (isInteger(From)) {
    if (isInteger(To)) {
      const int * xp = INTEGER(From);
      const int * yp = INTEGER(To);
      if (xp[0] != yp[0]) {
        ansp[0] = 1;
      }
      FORLOOP({
        if (ISNAN(ansp[i])) {
          int ypi = yp[i];
          // int xpi = xp[i];
          if (ypi < index_max_yr) {
            YearMonth YM_from;
            YM_from.year = ypi - MIN_YEAR;
            YM_from.month = 1;

            int index_from_i = yqi(YM_from) - yqi(index_min_YM);
            if (IDX_OOB(index_from_i, (R_xlen_t)index_len)) {
              continue;
            }
            double index_from = index[index_from_i];
            ansp[i] = last_index / index_from; // provisionally
            int d_years = (ypi - index_max_yr);
            ansp[i] *= pow(r_future, d_years);
          }

        }
      })

    } else {

    }

  } else {
   if (isInteger(To)) {

   } else {

   }
  }
  return ans;

}


