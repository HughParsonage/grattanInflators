#include "grattanInflator.h"

YearMonth YM_NA(void) {
  YearMonth O;
  O.year = 0;
  O.month = 15;
  return O;
}

bool YM_valid(YearMonth YM) {
  return YM.month >= 1 && YM.month <= 12;
}

// Returns the year as an offset from MIN_YEAR, or a negative value if the
// first four characters are not a year in [MIN_YEAR, MAX_YEAR].
// The caller must have established that x has at least four characters.
int string2year(const char * x) {
  if (!gi_isdigit(x[0]) || !gi_isdigit(x[1]) ||
      !gi_isdigit(x[2]) || !gi_isdigit(x[3])) {
    return -1;
  }
  int year =
    1000 * (x[0] - '0') +
     100 * (x[1] - '0') +
      10 * (x[2] - '0') +
            (x[3] - '0');
  if (year < MIN_YEAR || year > MAX_YEAR) {
    return -1;
  }
  return year - MIN_YEAR;
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
    break; // # nocov
  case '1':
    switch(x[6]) {
    case '0':
      return 10;
    case '1':
      return 11;
    case '2':
      return 12;
    }
    break; // # nocov
  default:
    return 15;
  }
  return 15; // # nocov
}

// The grammar consumed here must be the grammar validated by err_string()
// in check_input.c.
static void string2YearMonth(YearMonth * ans,
                             const char * x, int n, int fy_month) {
  int yr = string2year(x);
  if (yr < 0) {
    *ans = YM_NA();
    return;
  }
  ans->year = yr;
  ans->month = 15;
  switch(n) {
  case 10:
    {
      int month = string2month(x);
      ans->month = (month >= 1 && month <= 12) ? month : 15;
    }
    break;
  case 7:
    if (gi_isdigit(x[5])) {
      // is fy: Jul-Dec fall in the first calendar year of the label,
      // Jan-Jun in the second.
      ans->year += (fy_month < 7);
      ans->month = fy_month;
    } else {
      // Quarters are dated by the last month of the quarter, matching the
      // ABS convention and err_string().
      switch(x[6]) {
      case '1':
        ans->month = 3;
        break;
      case '2':
        ans->month = 6;
        break;
      case '3':
        ans->month = 9;
        break;
      case '4':
        ans->month = 12;
        break;
      }
    }
    break;
  }
  if (!YM_valid(*ans)) {
    *ans = YM_NA();
  }
}


void SEXP2YearMonth(YearMonth * ansp,
                    SEXP x,
                    int x_class,
                    int fy_month,
                    bool check_day, const char * var, int nThread) {
  if (ansp == NULL) {
    return; // # nocov
  }
  R_xlen_t N = xlength(x);
  if (isInteger(x)) {
    const int * xp = INTEGER(x);
    switch(x_class) {
    case CLASS_FY:
      // xp[i] is the ending year of the financial year (as fy::fy2yr gives),
      // so a Jul-Dec fy_month falls in the preceding calendar year.
      FORLOOP({
        if (xp[i] == NA_INTEGER) {
          ansp[i] = YM_NA();
          continue;
        }
        int yr = xp[i] - (fy_month >= 7) - MIN_YEAR;
        if (yr < 0 || yr > 127) {
          ansp[i] = YM_NA();
          continue;
        }
        YearMonth O;
        O.year = yr;
        O.month = fy_month;
        ansp[i] = O;
      })
      break;
    case CLASS_Date:
    case CLASS_IDate:
        FORLOOP({
          ansp[i] = (xp[i] == NA_INTEGER || xp[i] < MIN_IDATE || xp[i] > MAX_IDATE) ?
            YM_NA() : idate2YearMonth(xp[i]);
        })
      break;
    default:
      FORLOOP({
        if (xp[i] == NA_INTEGER) {
          ansp[i] = YM_NA();
          continue;
        }
        int yr = xp[i] - MIN_YEAR;
        if (yr < 0 || yr > 127) {
          ansp[i] = YM_NA();
          continue;
        }
        YearMonth O;
        O.year = yr;
        O.month = 1;
        ansp[i] = O;
      })
      break;
    }
    return;
  }
  const SEXP * xp = STRING_PTR_RO(x);
  if (N == 0) {
    return;
  }

  // Extract strings on the main thread in bounded chunks, then convert
  // without R API calls. A small CHARSXP dictionary means repeated dates are
  // parsed once per chunk rather than once per element.
  const R_xlen_t max_chunk = 1 << 20;
  const R_xlen_t chunk_capacity = N < max_chunk ? N : max_chunk;
  const char ** strings = (const char **)R_alloc(chunk_capacity, sizeof(*strings));
  int * lengths = (int *)R_alloc(chunk_capacity, sizeof(*lengths));
  int * string_ids = (int *)R_alloc(chunk_capacity, sizeof(*string_ids));
  YearMonth * parsed = (YearMonth *)R_alloc(chunk_capacity, sizeof(*parsed));
  const unsigned int cache_capacity = 1 << 16;
  const unsigned int cache_mask = cache_capacity - 1;
  SEXP * cache_keys = (SEXP *)R_alloc(cache_capacity, sizeof(*cache_keys));
  int * cache_ids = (int *)R_alloc(cache_capacity, sizeof(*cache_ids));

  for (R_xlen_t base = 0; base < N; base += chunk_capacity) {
    R_xlen_t chunk_n = N - base;
    if (chunk_n > chunk_capacity) {
      chunk_n = chunk_capacity;
    }
    SEXP first = xp[base];
    bool all_same = true;
    for (R_xlen_t j = 1; j < chunk_n; ++j) {
      if (xp[base + j] != first) {
        all_same = false;
        break;
      }
    }
    if (all_same) {
      YearMonth value = YM_NA();
      if (first != NA_STRING) {
        int n = length(first);
        if (n == 7 || n == 10) {
          string2YearMonth(&value, CHAR(first), n, fy_month);
        }
      }
#if defined _OPENMP
#pragma omp parallel for num_threads(nThread) schedule(static)
#endif
      for (R_xlen_t j = 0; j < chunk_n; ++j) {
        ansp[base + j] = value;
      }
      continue;
    }

    for (unsigned int k = 0; k < cache_capacity; ++k) {
      cache_keys[k] = NULL;
    }
    SEXP previous = R_NilValue;
    int previous_id = -1;
    int n_distinct = 0;
    for (R_xlen_t j = 0; j < chunk_n; ++j) {
      SEXP elt = xp[base + j];
      if (elt == NA_STRING) {
        string_ids[j] = -1;
      } else if (elt == previous) {
        string_ids[j] = previous_id;
      } else {
        unsigned int slot = ((uintptr_t)elt >> 3) & cache_mask;
        if (cache_keys[slot] == elt) {
          string_ids[j] = cache_ids[slot];
        } else {
          string_ids[j] = n_distinct;
          strings[n_distinct] = CHAR(elt);
          lengths[n_distinct] = length(elt);
          cache_keys[slot] = elt;
          cache_ids[slot] = n_distinct;
          ++n_distinct;
        }
        previous = elt;
        previous_id = string_ids[j];
      }
    }
#if defined _OPENMP
#pragma omp parallel for num_threads(nThread) schedule(static)
#endif
    for (int id = 0; id < n_distinct; ++id) {
      parsed[id] = YM_NA();
      if (lengths[id] == 7 || lengths[id] == 10) {
        string2YearMonth(&parsed[id], strings[id], lengths[id], fy_month);
      }
    }
#if defined _OPENMP
#pragma omp parallel for num_threads(nThread) schedule(static)
#endif
    for (R_xlen_t j = 0; j < chunk_n; ++j) {
      int id = string_ids[j];
      ansp[base + j] = id < 0 ? YM_NA() : parsed[id];
    }
  }
}
