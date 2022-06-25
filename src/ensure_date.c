#include "grattanInflator.h"

const static int MONTHDAYS[13] = {0, 31, 28, 31,  30,  31,  30,  31,  31,  30,  31,  30, 31};
const static int MONTHDAYC[12] = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334};

const static int IDAY_1948 = -8036;
const static int IDAY_2020 = 18262;
const static int IDAY_2021 = 18628;



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



Date initializeDate(int year, int month, int day) {
  Date O;
  O.year = (year - 1948) & 127;
  O.month = month % 13;
  O.day = day % 32;
  return O;
}

Date NA_DATE() {
  Date O;
  O.year = 127;
  O.month = 15; // nonsense values
  O.day = 0;
  return O;
}

Date yearqtr2Date(int year, int quarter) {
  switch(quarter) {
  case 1:
    return initializeDate(year, 3, 1);
  case 2:
    return initializeDate(year, 6, 1);
  case 3:
    return initializeDate(year, 9, 1);
  case 4:
    return initializeDate(year, 12, 1);
  }
  return initializeDate(year, 6, 1);
}

int days_since_1970(Date x) {
  // 1970-01-01 = 0;
  int o = 0;
  const int x_year = 1948 + x.year;
  if (x_year >= 2020) {
    o = IDAY_2020;
    for (int y = 2020; y < x_year; ++y) {
      o += 365;
      o += is_leap_year(y);
    }
  } else if (x_year >= 1970) {
    for (int y = 1970; y < x_year; ++y) {
      o += 365;
      o += is_leap_year(y);
    }
  } else {
    for (int y = 1970; y > x_year; --y) {
      o -= 365;
      o -= is_leap_year(y);
    }
  }
  o += MONTHDAYC[x.month - 1];
  o += x.day;
  return o;
}



Date int2Date(int x) {
  if (x > MAX_IDATE) {
    return NA_DATE();
  }
  int year = 1970, month = 1;

  if (x == IDAY_2020) {
    return initializeDate(2020, 1, 1);
  }
  int xx = 0; // value of year,month,day
  if (x >= IDAY_2020) {
    year = 2020;
    month = 1;
    xx = IDAY_2020;
  }

  if (x < 0) {
    if (x < IDAY_1948) {
      return initializeDate(1948, 0, 0);
    }

    while (xx > x) {
      xx -= 365;
      --year;
      xx -= is_leap_year(year);
    }
    if (x <= (xx + 31)) {
      return initializeDate(year, 1, x - xx + 1);
    }
    xx += MONTHDAYS[month];
    while (xx < x && month < 13) {
      xx += MONTHDAYS[month];
      ++month;
    }
    return initializeDate(year, month, x - xx + 1);
  }
  while (xx < x) {
    xx += 365 + is_leap_year(year);
    ++year;
  }
  --year;
  xx -= 365 + is_leap_year(year);
  while (xx < x) {
    xx += MONTHDAYS[month];
    ++month;
  }
  --month;
  xx -= MONTHDAYS[month];
  return initializeDate(year, month, x - xx + 1);
}

static Date yyyy_mm_dd2Date(const char * x) {
  Date O;
  int year = 1900 + 100 * (x[1] == '0') + 10 * (x[2] - '0') + (x[3] - '0');
  if (year < 1948) {
    year = 1948;
  }
  if (year > 1948 + 127) {
    year = 1948 + 127;
  }
  int month = 10 * (x[5] - '0') + (x[6] - '0');
  int day = 0 * (x[8] - '0') + (x[9] - '0');
  O.year = year - 1948;
  O.month = month;
  O.day = day;
  return O;
}

static Date yyyy_xx(const char * x, int choose_fy) {
  // 2010-Q1 - easy enough
  // 2010-01 - also easy -- just 2010-01-01
  // 2010-11 - hard to know whether this is a financial year
  //           or November 2011.  We need extra info, provided by
  //           choose_fy:
  //                    0 - always interpret as month (Nov 2011 here)
  //                    n - interpret as fy and month n (1 = Jan) thereof (so 6 => 2011-06-01)

  // have already checked with starts_with_yyyy in ensure_Date
  int year = 1900 + 100 * (x[1] == '0') + 10 * (x[2] - '0') + (x[3] - '0');
  if (x[5] == 'Q') {
    // interpret as quarters
    if (isdigit(x[6])) {
      return yearqtr2Date(year, x[6] - '0');
    } else {
      return NA_DATE();
    }
  }

  if (x[4] != ' ' && x[4] != '-') {
    return NA_DATE();
  }
  if (!isdigit(x[5]) || !isdigit(x[6])) {
    return NA_DATE();
  }
  int month = 10 * (x[5] - '0') + (x[6] - '0');
  // but may not be month if this comes from a
  // data of fy
  if (choose_fy && ((year + 1) % 100 == (month % 100))) {
    // 2020-21 => 2021-06-30
    // but if choose_fy falls in the last half of the year, it becomes
    // the same year
    return initializeDate(year + (choose_fy < 7), choose_fy, 1);
  }
  Date O = initializeDate(year, month, 1);
  return O;
}

Date yyyy2Date(const char * x) {
  int yr = 1900 + 100 * (x[1] == '0') + 10 * (x[2] - '0') + (x[3] - '0') - 1948;
  Date O = initializeDate(yr, 1, 1);
  return O;
}


Date string2Date(const char * x, int n, int choose_fy) {
  if (n < 4 || !starts_with_yyyy(x)) {
    return NA_DATE();
  }
  switch(n) {
  case 10:
    if (((x[4] == '-' || x[4] == ' ') && (x[7] == '-' || x[7] == ' '))) {
      return yyyy_mm_dd2Date(x);
    } else {
      return NA_DATE();
    }
    break;
  case 7:
    // financial year, YYYY-MM, or YYYY-Q4
    return yyyy_xx(x, choose_fy);

  case 4:
    return yyyy2Date(x);
  }
  return initializeDate(2020, 1, 1);
}
static bool is_fy(const char * x) {
  if (starts_with_yyyy(x) &&
      isdigit(x[5]) && (isdigit(x[4]) || isdigit(x[6]))) {
    int y1 = 1000 * (x[0] - '0');
    y1 += 100 * (x[1] - '0');
    y1 += 10 * (x[2] - '0');
    y1 += x[3] - '0';
    int y2 = 0;
    if (isdigit(x[4])) {
      y2 = 10 * (x[4] - '0') + (x[5] - '0');
    } else {
      y2 = 10 * (x[5] - '0') + (x[6] - '0');
    }
    int yy1 = y1 + 1;
    return (yy1 % 100) == (y2 % 100);
  }
  return false;
}

static bool all_fy(const SEXP * xp, R_xlen_t N, int nThread) {
  int n_fy = 0, are_na = 0;
  int n = N >= INT_MAX ? (INT_MAX / 4) : (N / 4);
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) reduction(+ : n_fy,are_na)
#endif
  for (int i = 0; i < n; ++i) {
    if (xp[i] == NA_STRING) {
      are_na += 1;
      continue;
    }
    n_fy += is_fy(CHAR(xp[i]));
  }
  return (n_fy + are_na) == n;
}

void character2dates(Date * dates, R_xlen_t N, int nThread, int choose_fy, const SEXP * xp) {
  bool has_fy = all_fy(xp, N, nThread);
  const int this_choose_fy = has_fy ? choose_fy : 0;

  FORLOOP({
    if (xp[i] == NA_STRING) {
      dates[i] = NA_DATE();
      continue;
    }
    dates[i] = string2Date(CHAR(xp[i]), length(xp[i]), this_choose_fy);
  })
}

static void integer2dates(Date * dates, R_xlen_t N, int nThread, const int * xp) {
  FORLOOP({

  })
}


SEXP C_ensure_date(SEXP x, SEXP nthreads) {
  if (!isString(x)) {
    error("x was type '%s' but must be a character vector.", type2char(TYPEOF(x)));
  }
  R_xlen_t N = xlength(x);
  const SEXP * xp = STRING_PTR(x);
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  AS_NTHREAD;

  FORLOOP({
    // ansp[i] = ensure_date(CHAR(xp[i]), length(xp[i]));
  });

  UNPROTECT(1);
  return ans;
}

SEXP C_print_IDATE(SEXP x) {
  if (!isInteger(x)) {
    return R_NilValue;
  }
  int xx = asInteger(x);
  Date O = int2Date(xx);
  Rprintf("Date = %d-%02d-%02d\n", O.year + 1948, O.month, O.day);
  return R_NilValue;

}
