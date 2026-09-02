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

  // NOT parallelised: CHAR() and length() are R API calls, which are not
  // thread-safe.
  FORLOOP_SERIAL({
    if (xp[i] == NA_STRING) {
      ansp[i] = YM_NA();
      continue;
    }
    int n = length(xp[i]);
    if (n != 10 && n != 7) {
      ansp[i] = YM_NA();
      continue;
    }
    YearMonth O;
    string2YearMonth(&O, CHAR(xp[i]), n, fy_month);
    ansp[i] = O;
  })
}

