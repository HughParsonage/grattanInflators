#include "grattanInflator.h"




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
  if (!isReal(Index)) {
    error("Index was type '%s' REALSXP which is not supported.", type2char(TYPEOF(Index))); // # nocov
  }
  const double * index = REAL(Index);
  const unsigned int index_len = length(Index);

  int index_min = asInteger(IndexMinIDate);
  int freq = asInteger(IndexFreq);
  const unsigned int div = 12 / freq;
  const unsigned int p_index_min = p_search(index_min) / div;
  double * ansp = REAL(ans);
  switch(freq) {
  case 12:
    if (N_y == 1) {
      const unsigned int y_p = (p_search(yp[0])) - p_index_min;
      if (y_p >= index_len) {
        // # nocov start
        FORLOOP({
          ansp[i] = NaN;
        })
        break;
        // # nocov end
      }
      const double iyp = index[y_p];
      FORLOOP({
        unsigned int x_p = (p_search(xp[i])) - p_index_min;
        if (x_p >= index_len) {
          ansp[i] = NaN;
          continue;
        }
        ansp[i] *= iyp / index[x_p];
      })
    } else {
      FORLOOP({
        unsigned int x_p = (p_search(xp[i])) - p_index_min;
        unsigned int y_p = (p_search(yp[i])) - p_index_min;
        if (x_p >= index_len || y_p >= index_len) {
          ansp[i] = NaN;
          continue;
        }
        ansp[i] *= index[y_p] / index[x_p];
      })
    }
    break;
  case 4:
    if (N_y == 1) {
      const unsigned int y_p = (p_search(yp[0]) / 3) - p_index_min;
      if (y_p >= index_len) {
        // # nocov start
        FORLOOP({
          ansp[i] = NaN;
        })
        break;
        // # nocov end
      }
      const double iyp = index[y_p];

      FORLOOP({
        unsigned int x_p = (p_search(xp[i]) / 3) - p_index_min;
        if (x_p >= index_len) {
          ansp[i] = NaN;
          continue;
        }
        ansp[i] *= iyp / index[x_p];
      })
    } else {
      FORLOOP({
        unsigned int x_p = (p_search(xp[i]) / 3) - p_index_min;
        unsigned int y_p = (p_search(yp[i]) / 3) - p_index_min;
        if (x_p >= index_len || y_p >= index_len) {
          ansp[i] = NaN;
          continue;
        }
        ansp[i] *= index[y_p] / index[x_p];
      })
    }
    break;
  case 1:
    if (N_y == 1) {
      const unsigned int y_p = (p_search(yp[0]) / 12) - p_index_min;
      if (y_p >= index_len) {
        // # nocov start
        FORLOOP({
          ansp[i] = NaN;
        })
        break;
        // # nocov end
      }
      const double iyp = index[y_p];
      FORLOOP({
        unsigned int x_p = (p_search(xp[i]) / 12) - p_index_min;
        if (x_p >= index_len) {
          ansp[i] = NaN;
          continue;
        }
        ansp[i] *= iyp / index[x_p];
      })
    } else {
      FORLOOP({
        unsigned int x_p = (p_search(xp[i]) / 12) - p_index_min;
        unsigned int y_p = (p_search(yp[i]) / 12) - p_index_min;
        if (x_p >= index_len || y_p >= index_len) {
          ansp[i] = NaN;
          continue;
        }
        ansp[i] *= index[y_p] / index[x_p];
      })
    }
    break;
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
  const int * xp = INTEGER(From);
  const int * yp = INTEGER(To);
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
  const unsigned int div = 12 / freq;
  const unsigned int p_index_min = p_search(index_min) / div;
  double * ansp = REAL(ans);

  const double r_future = future_rate_12mo(index, freq, index_len);
  const double last_index = index[index_len - 1];
  YearMonth index_min_YM = idate2YearMonth(index_min);
  YearMonth index_max_ym = idate2YearMonth(index_min + index_len - 1);
  const int index_max_yr = index_max_ym.year + MIN_YEAR;
  const int index_max_mo = index_max_ym.month;



  if (isInteger(From)) {
    if (isInteger(To)) {
      const int * xp = INTEGER(From);
      const int * yp = INTEGER(To);
      FORLOOP({
        if (ISNAN(ansp[i])) {
          int ypi = yp[i];
          int xpi = xp[i];
          if (ypi < index_max_yr) {
            YearMonth YM_from;
            YM_from.year = ypi - MIN_YEAR;
            YM_from.month = 1;

            double index_from = index[yqi(YM_from) - yqi(index_min_YM)];
            ansp[i] = last_index / index_from; // provisionally
            int d_years = (ypi - index_max_yr);
            ansp[i] *= pow(r_future, d_years);
          }

        }
      })

    } else {
      const int * xp = INTEGER(From);
      const SEXP * yp = STRING_PTR(To);
    }

  } else {
   if (isInteger(To)) {

   } else {

   }
  }
  return ans;

}




