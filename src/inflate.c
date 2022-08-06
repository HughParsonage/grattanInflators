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



