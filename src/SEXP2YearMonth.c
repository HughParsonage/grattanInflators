#include "grattanInflator.h"

static YearMonth YM_NA(void) {
  YearMonth O;
  O.year = 0;
  O.month = 15;
  return O;
}

int string2year(const char * x) {
  // no check on whether string length is adequate, or starts with digits
  int year = 0;
  year += (x[1] == '9') ? 1900 : 2000;
  year += 10 * (x[2] - '0');
  year += x[3] - '0';
  return year - 1948;
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

static void string2YearMonth(YearMonth * ans,
                             const char * x, int n) {
  ans->year = string2year(x);
  switch(n) {
  case 10:
    ans->month = string2month(x);
    break;
  case 7:
    ans->year++;
    ans->month = 3;

    break;
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
      FORLOOP({
        if (xp[i] == NA_INTEGER) {
          ansp[i] = YM_NA();
          continue;
        }
        YearMonth O;
        O.year = xp[i] - MIN_YEAR;
        O.month = fy_month;
        ansp[i] = O;
      })
      break;
    case CLASS_Date:
    case CLASS_IDate:
        FORLOOP({
          ansp[i] = xp[i] == NA_INTEGER ? YM_NA() : idate2YearMonth(xp[i]);
        })
      break;
    default:
      FORLOOP({
        if (xp[i] == NA_INTEGER) {
          ansp[i] = YM_NA();
          continue;
        }
        YearMonth O;
        O.year = xp[i] - MIN_YEAR;
        O.month = 1;
        ansp[i] = O;
      })
      break;
    }
    return;
  }
  const SEXP * xp = STRING_PTR(x);

  FORLOOP({
    int n = length(xp[i]);
    if (n != 10 && n != 7) {
      ansp[i] = YM_NA();
      continue;
    }
    YearMonth O;
    string2YearMonth(&O, CHAR(xp[i]), length(xp[i]));
    ansp[i] = O;
  })
}

