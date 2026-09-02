#include "grattanInflator.h"

#define ERR_CHAR_NOT_YYYY 11
#define ERR_CHAR_YR_RANGE 13
#define ERR_CHAR_NO_MONTH 15
#define ERR_CHAR_BAD_MDAY 17
#define ERR_CHAR_FY_QUART 19
#define ERR_CHAR_YYYY__QQ 20
#define ERR_CHAR_BAD_NCHAR 21
#define ERR_CHAR_BAD_SEPTR 22

bool starts_with_yyyy(const char * x, int n) {
  // n must be checked first: fixed offsets are read below
  return n >= 4 &&
    (x[0] == '1' || x[0] == '2') &&
    gi_isdigit(x[1]) && gi_isdigit(x[2]) && gi_isdigit(x[3]);
}

bool isnt_leap_yr(unsigned int yr) {
  return yr & 3u;
  // note that 2000 is the only yr divisible by 100, so we only check divisibility by 4
  // if (yr < 0 || yr % 4) {
  //   return 0;
  // }
  // if (yr % 100) {
  //   return 0;
  // }
  // if (yr % 400) {
  //   return 1;
  // }
  // return 1;
}

static bool is_valid_YYYYQQ(const char * z) {
  // assumes year has been checked
  if (z[5] == 'Q' || z[5] == 'q') {
    return z[6] >= '1' && z[6] <= '4';
  }
  return false;
}

static bool is_valid_fy_quartet(const char * z) {
  register char u = z[2], v = z[3], x = z[5], y = z[6];
  if (!gi_isdigit(u) || !gi_isdigit(v) || !gi_isdigit(x) || !gi_isdigit(y)) {
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

unsigned char invalid_mday(const char * x, int yr, int month) {
  static int MDAYS[13] = {0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
  if (!gi_isdigit(x[8]) || !gi_isdigit(x[9])) {
    return 1;
  }
  int mday = 10 * (x[8] - '0') + (x[9] - '0');
  if (mday == 0 || month > 12 || mday > MDAYS[month]) {
    return 1;
  }
  if (month == 2 && isnt_leap_yr(yr) && mday == 29) {
    return 1;
  }
  return 0;
}

// The grammar accepted here must be exactly the grammar consumed by
// string2YearMonth(): a string that passes this check must convert to the
// same YearMonth that the converter produces, and vice versa.
static unsigned char err_string(YearMonth * YM, const char * x, int n, int check, const int fy_month) {
  // Only YYYY-mm-dd (10) and YYYY-YY / YYYY-Qq (7) can be converted, so any
  // other width must be rejected rather than silently accepted.
  if (n != 7 && n != 10) {
    return ERR_CHAR_BAD_NCHAR;
  }
  if (!starts_with_yyyy(x, n)) {
    return ERR_CHAR_NOT_YYYY;
  }
  int yr = string2year(x);
  if (yr < 0 || yr > 127) {
    return ERR_CHAR_YR_RANGE;
  }
  if (!gi_issep(x[4])) {
    return ERR_CHAR_BAD_SEPTR;
  }
  YM->year = yr;
  if (n == 10) {
    if (!gi_issep(x[7])) {
      return ERR_CHAR_BAD_SEPTR;
    }
    int month = string2month(x);
    if (month == 15) {
      return ERR_CHAR_NO_MONTH;
    }
    if (check >= 2 && invalid_mday(x, yr + MIN_YEAR, month)) {
      return ERR_CHAR_BAD_MDAY;
    }
    YM->month = month;
    return 0;
  }
  // n == 7
  if (is_valid_fy_quartet(x)) {
    YM->month = fy_month;
    // A financial year labelled YYYY-YY falls in the first calendar year for
    // months Jul-Dec and in the second for Jan-Jun. string2YearMonth() applies
    // the same rule.
    YM->year += (fy_month < 7);
    return 0;
  }

  if (is_valid_YYYYQQ(x)) {
    // Quarters are dated by the last month of the quarter, as the ABS does;
    // string2YearMonth() uses the same months.
    YM->month = 3 * (x[6] - '0');
    return 0;
  }
  if (x[5] == 'Q' || x[5] == 'q') {
    return ERR_CHAR_YYYY__QQ;
  }
  return ERR_CHAR_FY_QUART;
}


// Function to pack a YearMonth into an unsigned int
unsigned int packYearMonth(YearMonth ym) {
  return (ym.year << 4) | ym.month;
}



static void check_valid_strings(unsigned int xpackminmax[2],
                                const SEXP * xp, R_xlen_t N, int check, int nThread,
                                const char * var,
                                const int fy_month,
                                const int min_date) {
  unsigned char o = 0;
  unsigned int pack_min = 2044; // maximum packYearMonth value
  unsigned int pack_max = 0;

  // Extract CHARSXP data on the main thread, then validate bounded batches in
  // parallel without calling the R API from worker threads.
  const R_xlen_t chunk_capacity = 1 << 20;
  const char ** strings = (const char **)R_alloc(chunk_capacity, sizeof(*strings));
  int * lengths = (int *)R_alloc(chunk_capacity, sizeof(*lengths));
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
      if (first != NA_STRING) {
        YearMonth YM;
        YM.year = 0;
        YM.month = 15;
        unsigned char ei = err_string(&YM, CHAR(first), length(first), check,
                                      fy_month);
        o |= ei;
        if (ei == 0 && YM.month <= 12) {
          unsigned int packedYM = packYearMonth(YM);
          if (packedYM < pack_min) {
            pack_min = packedYM;
          }
          if (packedYM > pack_max) {
            pack_max = packedYM;
          }
        }
      }
      continue;
    }
    SEXP previous = R_NilValue;
    const char * previous_string = NULL;
    int previous_length = 0;
    for (R_xlen_t j = 0; j < chunk_n; ++j) {
      SEXP elt = xp[base + j];
      if (elt == NA_STRING) {
        strings[j] = NULL;
        lengths[j] = 0;
      } else if (elt == previous) {
        strings[j] = previous_string;
        lengths[j] = previous_length;
      } else {
        strings[j] = CHAR(elt);
        lengths[j] = length(elt);
        previous = elt;
        previous_string = strings[j];
        previous_length = lengths[j];
      }
    }
    unsigned char chunk_o = 0;
    unsigned int chunk_min = 2044;
    unsigned int chunk_max = 0;
#if defined _OPENMP
#pragma omp parallel for num_threads(nThread) schedule(static) reduction(| : chunk_o) reduction(min : chunk_min) reduction(max : chunk_max)
#endif
    for (R_xlen_t j = 0; j < chunk_n; ++j) {
      if (strings[j] == NULL) {
        continue;
      }
      YearMonth YM;
      YM.year = 0;
      YM.month = 15;
      unsigned char ei = err_string(&YM, strings[j], lengths[j], check, fy_month);
      chunk_o |= ei;
      if (ei == 0 && YM.month <= 12) {
        unsigned int packedYM = packYearMonth(YM);
        if (packedYM < chunk_min) {
          chunk_min = packedYM;
        }
        if (packedYM > chunk_max) {
          chunk_max = packedYM;
        }
      }
    }
    o |= chunk_o;
    if (chunk_min < pack_min) {
      pack_min = chunk_min;
    }
    if (chunk_max > pack_max) {
      pack_max = chunk_max;
    }
  }

  xpackminmax[0] = pack_min;
  xpackminmax[1] = pack_max;
  const unsigned int pack_min_date = packYearMonth(idate2YearMonth(min_date));
  if (o != 0 || pack_min < pack_min_date) {
    for (R_xlen_t i = 0; i < N; ++i) {
      if (xp[i] == NA_STRING) {
        continue;
      }
      const char * xi = CHAR(xp[i]);
      int nxi = length(xp[i]);
      YearMonth YM;
      YM.year = 0;
      YM.month = 15;
      int ei = err_string(&YM, xi, nxi, check, fy_month);
      if (!ei && packYearMonth(YM) < pack_min_date) {
        YearMonth YM_min = idate2YearMonth(min_date);
        error("`%s[%lld] = %s`, which is earlier than the earliest date in the series (%d-%02d-01)",
              var, (long long)i + 1, xi, YM_min.year + MIN_YEAR, YM_min.month);
      }
      switch(ei) {
      case 0:
        continue;
      case ERR_CHAR_NOT_YYYY:
        error("`%s` contained invalid element:\n\t %s[%lld] = %s\n(must start with YYYY)",
              var, var, (long long)i + 1, CHAR(xp[i]));
      case ERR_CHAR_NO_MONTH:
        error("`%s` contained invalid element:\n\t %s[%lld] = %s\n(No month)",
              var, var, (long long)i + 1, CHAR(xp[i]));
      case ERR_CHAR_BAD_MDAY:
        error("`%s` contained invalid element:\n\t %s[%lld] = %s\n(Day component invalid)",
              var, var, (long long)i + 1, CHAR(xp[i]));
      case ERR_CHAR_YR_RANGE:
        error("`%s` contained invalid element:\n\t %s[%lld] = %s\n(Years must be between %d and %d)",
              var, var, (long long)i + 1, CHAR(xp[i]), MIN_YEAR, MAX_YEAR);
      case ERR_CHAR_FY_QUART:
        error("`%s` contained invalid element:\n\t %s[%lld] = %s\n(Invalid fy)",
              var, var, (long long)i + 1, CHAR(xp[i]));
      case ERR_CHAR_YYYY__QQ:
        error("`%s` contained invalid element:\n\t %s[%lld] = %s\n(Invalid YYYY-QQ).",
              var, var, (long long)i + 1, CHAR(xp[i]));
      case ERR_CHAR_BAD_NCHAR:
        error("`%s` contained invalid element:\n\t %s[%lld] = %s\n"
              "(must be 10 characters (YYYY-mm-dd) or 7 (YYYY-YY or YYYY-Qq), not %d)",
              var, var, (long long)i + 1, CHAR(xp[i]), (int)length(xp[i]));
      case ERR_CHAR_BAD_SEPTR:
        error("`%s` contained invalid element:\n\t %s[%lld] = %s\n"
              "(date components must be separated by '-', '/' or '.')",
              var, var, (long long)i + 1, CHAR(xp[i]));
      default:
        error("`%s` contained invalid element but error condition not known.", var);
      }
    }
  }
}

void check_strsxp(bool * any_beyond,
                  const SEXP * xp, R_xlen_t N, int check, const char * var,
                  const int fy_month,
                  int nThread,
                  const int min_date,
                  const int max_date) {
  unsigned int xpackminmax[2];
  check_valid_strings(xpackminmax, xp, N, check, nThread, var, fy_month, min_date);
  YearMonth YM_max_date = idate2YearMonth(max_date);
  unsigned int packed_max_date = packYearMonth(YM_max_date);
  if (xpackminmax[1] > packed_max_date) {
    if (check >= 2) {
      // need error on excessive date, not just a signal
      for (R_xlen_t i = 0; i < N; ++i) {
        if (xp[i] == NA_STRING) {
          continue;
        }
        const char * xpi = CHAR(xp[i]);
        YearMonth YM_i;
        err_string(&YM_i, xpi, length(xp[i]), check, fy_month);
        unsigned int packed_i = packYearMonth(YM_i);
        if (packed_i > packed_max_date) {
          error("`%s[%lld] = %s` which is later than the latest allowable date (%d-%02d-01)",
                var, (long long)i + 1, xpi, YM_max_date.year + MIN_YEAR, YM_max_date.month);
        }
      }
    }
    *any_beyond = true;
  }
}

void check_intsxp(bool * any_beyond,
                  const int * xp, R_xlen_t N, int check, const char * var,
                  bool was_date, bool was_fy, int fy_month,
                  int nThread,
                  const int min_date,
                  const int max_date) {
  int xminmax[2];
  iminmax(xminmax, xp, N, 3, nThread);

  if (was_date) {
    if (min_date > xminmax[0] || xminmax[0] < MIN_IDATE) {
      for (R_xlen_t i = 0; i < N; ++i) {
        if (xp[i] >= min_date || xp[i] == NA_INTEGER) {
          continue;
        }
        // Need to error for minimum supported dates since format_1_idate
        // relies on xp[i] being in the correct range
        if (xp[i] < MIN_IDATE) {
          error("`%s[%lld] = %d`, which is earlier than the earliest supported date (1948-01-01).",
                var, (long long)i + 1, xp[i]);
        }
        char oi[11] = {0};
        char oj[11] = {0};
        format_1_idate(oi, xp[i]);
        format_1_idate(oj, min_date);
        error("`%s[%lld] = %s`, which is earlier than the earliest date in the series (%s).",
              var, (long long)i + 1, (const char *)oi, (const char *)oj);
      }
    }
    *any_beyond = max_date < xminmax[1];
    if ((check >= 2 || xminmax[1] > MAX_IDATE) && *any_beyond) {
      for (R_xlen_t i = 0; i < N; ++i) {
        if (xp[i] <= max_date || xp[i] == NA_INTEGER) {
          continue;
        }

        if (xp[i] > MAX_IDATE) {
          error("`%s[%lld] = %d`, which is later than the latest supported date (2075-12-31).",
                var, (long long)i + 1, xp[i]);
        }
        char oi[11] = {0};
        char oj[11] = {0};
        format_1_idate(oi, xp[i]);
        format_1_idate(oj, max_date);
        error("`check >= 2` yet `%s[%lld] = %s`, which is later than the latest date in the series (%s). [ERR262]",
              var, (long long)i + 1, (const char *)oi, (const char *)oj);
      }
    }


  } else {
    // was year
    int yr_min_date = year(min_date);
    int yr_max_date = year(max_date);
    // fy::fy2yr() supplies the ending year. A Jul-Dec financial-year month is
    // in the preceding calendar year, matching SEXP2YearMonth().
    int fy_adjust = was_fy && fy_month >= 7;
    int resolved_min = xminmax[0] - fy_adjust;
    int resolved_max = xminmax[1] - fy_adjust;
    if (yr_min_date > resolved_min) {
      for (R_xlen_t i = 0; i < N; ++i) {
        if (xp[i] == NA_INTEGER) {
          continue;
        }
        int resolved_i = xp[i] - fy_adjust;
        if (yr_min_date <= resolved_i) {
          continue;
        }
        error("`%s[%lld] = %d`, which is earlier than the earliest date in the series (%d).",
                var, (long long)i + 1, resolved_i, yr_min_date);
        break;
      }
    }

    *any_beyond = yr_max_date < resolved_max;
    if ((check >= 2 && *any_beyond) || resolved_max > MAX_YEAR) {
      for (R_xlen_t i = 0; i < N; ++i) {
        if (xp[i] == NA_INTEGER) {
          continue;
        }
        int resolved_i = xp[i] - fy_adjust;
        if (resolved_i > MAX_YEAR) {
          error("`%s[%lld] = %d`, which is outside the supported years (latest %d)",
                var, (long long)i + 1, resolved_i, MAX_YEAR);
        }
        if (resolved_i <= yr_max_date) {
          continue;
        }
        error("`check >= 2` yet `%s[%lld] = %d`, which is later than the latest year in the series (%d).",
              var, (long long)i + 1, resolved_i, yr_max_date);
      }
    }
  }
}



// are the strings or inputs valid (i.e. cannot represent dates, and
// should be brought to the user's attention)?
// Is the series sufficient for the input? (Are the dates between the series?)
//  -- if not need to signal extension
SEXP C_check_input(SEXP x, SEXP Var, SEXP Check, SEXP Class, SEXP minDate, SEXP maxDate, SEXP nthreads, SEXP Fymonth) {
  const int check = asInteger(Check);
  if (!check) {
    return ScalarLogical(0);
  }

  const int fy_month = asInteger(Fymonth);
  const char * var = CHAR(STRING_ELT(Var, 0));
  int nThread = as_nThread(nthreads);
  int xclass = asInteger(Class);
  bool was_date = xclass == CLASS_Date || xclass == CLASS_IDate;
  bool was_fy = xclass == CLASS_FY;
  const int min_date = asInteger(minDate);
  const int max_date = asInteger(maxDate);
  if (min_date < MIN_IDATE || min_date > MAX_IDATE || max_date < MIN_IDATE || max_date > MAX_IDATE) {
    error("(Internal error C_check_input 331): min_date, max_date out-of-range."); // # nocov
  }
  bool any_beyond = false;

  switch(TYPEOF(x)) {
  case STRSXP:
    check_strsxp(&any_beyond, STRING_PTR_RO(x), xlength(x), check, var, fy_month, nThread, min_date, max_date);
    break;
  case INTSXP:
    check_intsxp(&any_beyond, INTEGER(x), xlength(x), check, var,
                 was_date, was_fy, fy_month, nThread, min_date, max_date);
  }


  return ScalarLogical(any_beyond);
}
