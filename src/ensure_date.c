#include "grattanInflator.h"

const static int MONTHDAYS[13] = {0, 31, 28, 31,  30,  31,  30,  31,  31,  30,  31,  30, 31};
const static int MONTHDAYC[12] = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334};

const static int IDAYS_1948_2075_0101[128] =
  {-8036, -7670, -7305, -6940, -6575, -6209, -5844, -5479, -5114, -4748, -4383, -4018,
   -3653, -3287, -2922, -2557, -2192, -1826, -1461, -1096, -731, -365, 0, 365,
   730, 1096, 1461, 1826, 2191, 2557, 2922, 3287, 3652, 4018, 4383, 4748, 5113,
   5479, 5844, 6209, 6574, 6940, 7305, 7670, 8035, 8401, 8766, 9131, 9496, 9862,
   10227, 10592, 10957, 11323, 11688, 12053, 12418, 12784, 13149, 13514, 13879,
   14245, 14610, 14975, 15340, 15706, 16071, 16436, 16801, 17167, 17532, 17897,
   18262, 18628, 18993, 19358, 19723, 20089, 20454, 20819, 21184, 21550, 21915,
   22280, 22645, 23011, 23376, 23741, 24106, 24472, 24837, 25202, 25567, 25933,
   26298, 26663, 27028, 27394, 27759, 28124, 28489, 28855, 29220, 29585, 29950,
   30316, 30681, 31046, 31411, 31777, 32142, 32507, 32872, 33238, 33603, 33968,
   34333, 34699, 35064, 35429, 35794, 36160, 36525, 36890, 37255, 37621, 37986, 38351};

const static int IDAY_1948 = -8036;
const static int IDAY_2020 = 18262;
const static int IDAY_2021 = 18628;
#define LOCATION_IDAY_2020_IN_IDAYS 72

bool is_supported_IDate(unsigned int x) {
  unsigned int ux = (x + MIN_IDATE);
  return ux < RANGE_IDATE;
}



void check_within_idaterange(const int * xp, R_xlen_t N, int nThread, const char * var) {
  char o = 1;
#if defined _OPENMP && _OPENMP >= 201511
#pragma omp parallel for num_threads(nThread) schedule(static) reduction(& : o)
#endif
  for (R_xlen_t i = 0; i < N; ++i) {
    o &= is_supported_IDate(xp[i]);
  }
  if (o != 1) {
    for (R_xlen_t i = 0; i < N; ++i) {
      if (!is_supported_IDate(xp[i])) {
        error("In `%s`, vector was integer but value %d at position %lld was not in valid range",
              var, xp[i], i + 1);
      }
    }
  }
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

static bool starts_with_yyyy(const char * x) {
  return (x[0] == '1' || x[0] == '2') && isdigit(x[1]) && isdigit(x[2]) && isdigit(x[3]);
}

static bool is_valid_month(char u, char v) {
  if (u < '0' || u > '3') {
    return false;
  }
  if (!isdigit(v)) {
    return false;
  }
  if (u == '3') {
    return v == '0' || v == '1';
  }
  return true;
}

static bool is_valid_fy_quartet(char u, char v, char x, char y) {
  if (!isdigit(u) || !isdigit(v) || !isdigit(x) || !isdigit(y)) {
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

YearMonth NA_YM() {
  YearMonth O;
  O.year = 127;
  O.month = 15; // nonsense values
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



SEXP C_print_IDATE(SEXP x) {
  if (!isInteger(x)) {
    return R_NilValue;
  }
  int xx = asInteger(x);
  Date O = int2Date(xx);
  Rprintf("Date = %d-%02d-%02d\n", O.year + 1948, O.month, O.day);
  return R_NilValue;
}


int valid_form(const char * x, int n, bool check_day, bool prefer_fy) {
  // which valid form
  // 0 not valid

  //   0123456789
  // 1 YYYY-MM-DD
  // 2 YYYY-ZZ (fy)
  // 3 YYYY-MM
  if (n != 10 && n != 7) {
    return 0;
  }
  if (!starts_with_yyyy(x)) {
    return 0;
  }
  if (!isdigit(x[6])) {
    return 0;
  }


  if (n == 10) {
    if (x[5] != '0' && x[5] != '1') {
      return 0;
    }
    // 00-12
    if (x[5] == '1' && x[6] != '0' && x[6] != '1' && x[6] != '2') {
      return 0;
    }
    if (check_day) {
      if (x[8] < '0' || x[8] > '3') {
        return 0;
      }
      if (!isdigit(x[9])) {
        return 0;
      }
      if (x[5] == '0' && x[6] == '2') {
        // February
        if (x[8] == '3') {
          return 0;
        }
        // TODO:
        // check leap year valid for 9
      }
    }
    return 1;
  }
  if (is_valid_fy_quartet(x[2], x[3], x[5], x[6])) {
    // maybe 2010-11
    if (!prefer_fy && is_valid_month(x[5], x[6])) {
      return 3;
    }
    return 2;
  }
  return 0;
}

void check_string_valid(SEXP x, bool constant_only, int nThread, bool check_day, bool prefer_fy, const char * var) {
  if (!isString(x)) {
    error("Internal error(check_string_valid): expected a string."); // # nocov
  }
  R_xlen_t N = xlength(x);
  if (N == 0) {
    return;
  }
  const SEXP * xp = STRING_PTR(x);
  R_xlen_t j = 0;
  while (j < N && xp[j] == NA_STRING) {
    ++j;
  }
  if (j == N) {
    warning("`%s` is naught but NA_STRING (completely empty) so no valid form can be identified.", var);
    return;
  }
  if (constant_only) {
    int first_form = valid_form(CHAR(xp[j]), length(xp[j]), check_day, prefer_fy);
    if (first_form == 0) {
      error("`%s` was not in valid form at position %lld = '%s'", var, j + 1, CHAR(xp[j]));
    }

    for (R_xlen_t i = j + 1; i < N; ++i) {
      if (xp[i] == NA_STRING) {
        continue;
      }
      int valid_form_i = valid_form(CHAR(xp[i]), length(xp[i]), check_day, prefer_fy);
      if (valid_form_i == 0) {
        error("`%s` was not of a valid form at position %lld = '%s'.", var, j + 1, CHAR(xp[j]));
      }
      if (valid_form_i != first_form) {
        error("`%s` was not of the same date form throughout.\n\t %s[%lld] = '%s'\n\t %s[%lld] = '%s'.",
              var,
              var, j + 1, CHAR(xp[j + 1]),
              var, i + 1, CHAR(xp[i + 1]));
      }

    }
    return;
  }
  for (R_xlen_t i = j + 1; i < N; ++i) {
    if (xp[i] == NA_STRING) {
      continue;
    }
    int valid_form_i = valid_form(CHAR(xp[i]), length(xp[i]), check_day, prefer_fy);
    if (valid_form_i == 0) {
      error("`%s` was not of a valid form at position %lld = '%s'.", var, j + 1, CHAR(xp[j]));
    }
  }
}

void check_SEXP_valid(SEXP x, bool constant_only, int nThread, bool check_day, bool prefer_fy, const char * var) {
  switch(TYPEOF(x)) {
  case INTSXP:
    check_within_idaterange(INTEGER(x), xlength(x), nThread, var);
    break;
  case REALSXP:
    error("Internal error(check_SEXP_valid): was passed a REALSXP, but numeric vectors should be integer here."); // # nocov
  case STRSXP:
    check_string_valid(x, constant_only, nThread, check_day, prefer_fy, var);
    break;
  default:
    error("`%s` was type '%s' but must be character or integer.", var, type2char(TYPEOF(x)));
  }
}

int string2year(const char * x) {
  // no check on whether string length is adequate, or starts with digits
  int year = 0;
  year += (x[1] == '9') ? 1900 : 2000;
  year += 10 * (x[2] - '0');
  year += x[3] - '0';
  return year - 1948;
}

YearMonth idate2YearMonth(int x) {
  YearMonth O;
  int j = x >= IDAY_2020 ? LOCATION_IDAY_2020_IN_IDAYS : 0;
  while (j < 128 && IDAYS_1948_2075_0101[j] < x) {
    ++j;
  }
  O.year = j;
  int days_rem = x - IDAYS_1948_2075_0101[j];
  int m = 0;
  for (; m < 12; ++m) {
    if (days_rem <= MONTHDAYC[m]) {
      break;
    }
  }
  O.month = m;
  return O;
}


void SEXP2YearMonth(YearMonth * ansp,
                    SEXP x, bool constant_only, bool prefer_fy,
                    int fy_month,
                    bool check_day, const char * var, int nThread) {
  check_SEXP_valid(x, constant_only, nThread, check_day, prefer_fy, var);
  if (ansp == NULL) {
    return;
  }
  R_xlen_t N = xlength(x);
  if (isInteger(x)) {
    const int * xp = INTEGER(x);
    FORLOOP({
      ansp[i] = idate2YearMonth(xp[i]);
    })

    return;
  }
  const SEXP * xp = STRING_PTR(x);

  if (constant_only) {
    R_xlen_t j = 0;
    while (j < N && xp[j] == NA_STRING) {
      ++j;
    }
    int form = valid_form(CHAR(xp[j]), length(xp[j]), check_day, prefer_fy);
    if (form == 2) {
      FORLOOP({
        if (xp[i] == NA_STRING) {
          ansp[i] = NA_YM();
          continue;
        }
        const char * xj = CHAR(xp[i]);
        YearMonth O;
        O.year = string2year(xj) + (fy_month < 7);
        O.month = fy_month;
        ansp[i] = O;
      })
    } else {
      FORLOOP({
        if (xp[i] == NA_STRING) {
          ansp[i] = NA_YM();
          continue;
        }
        const char * xj = CHAR(xp[i]);
        YearMonth O;
        O.year = string2year(xj);
        O.month = 10 * (xj[5] - '0') + (xj[6] - '0');
        ansp[i] = O;
      })
    }
    return;
  }
  FORLOOP({
    if (xp[i] == NA_STRING) {
      ansp[i] = NA_YM();
      continue;
    }
    const char * xj = CHAR(xp[i]);
    int form_i = valid_form(xj, length(xp[i]), check_day, prefer_fy);
    YearMonth O;
    O.year = string2year(xj);
    if (form_i == 2) {
      O.month = fy_month;
      ansp[i] = O;
      continue;
    }
    O.month = 10 * (xj[5] - '0') + (xj[6] - '0');
    ansp[i] = O;
  })
}


