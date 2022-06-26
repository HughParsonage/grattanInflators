#include "grattanInflator.h"

#define CPI_SIZE 400
// Q10 = "Quarterly CPI .. times 10"
static uint16_t Q10_CPI_SINCE_1948[CPI_SIZE]; // the values
static int nCPI_QUARTERS; // the number of quarters
static Date CPI_SINCE_1948_Dates[CPI_SIZE]; // the array of dates
#define CPI_FIRST_QUARTER_1948 3
bool cpi_prepared = false;

SEXP C_cpi_prepared() {
  return ScalarLogical(cpi_prepared);
}

void print_Date(Date O) {
  Rprintf("Date = %d-%02d-%02d\n", O.year + 1948, O.month, O.day);
}

int Date2CPI_idx(Date x) {
  if (x.year == 0) {
    return x.month <= 9 ? 0 : 1;
  }
  int yr = x.year + 1948;
  int i = yr - 1949;
  i *= 4;
  return i + 2 + x.month / 3;
}

int YearMonth2CPI_idx(YearMonth x) {
  if (x.year == 0) {
    return x.month <= 9 ? 0 : 1;
  }
  int yr = x.year + 1948;
  int i = yr - 1949;
  i *= 4;
  return i + 2 + x.month / 3;
}

int Year2CPI_idx(int yr) {
  unsigned int o = 4 * (yr - 1949) + 2;
  return o < 399 ? o : 399;
}

static double cpi_Date_inflator(Date from, Date to) {
  uint16_t y_from = Q10_CPI_SINCE_1948[Date2CPI_idx(from)];
  uint16_t y_to = Q10_CPI_SINCE_1948[Date2CPI_idx(to)];
  return ((double)y_to) / ((double)y_from);
}

static double cpi_YearMonth_inflator(YearMonth from, YearMonth to) {
  uint16_t y_from = Q10_CPI_SINCE_1948[YearMonth2CPI_idx(from)];
  uint16_t y_to = Q10_CPI_SINCE_1948[YearMonth2CPI_idx(to)];
  return ((double)y_to) / ((double)y_from);
}

void update_cpi(const double * xp, const bool verbose, int N) {
  for (int i = 0; i < CPI_SIZE; ++i) {
    Q10_CPI_SINCE_1948[i] = 10 * xp[i];
  }
  CPI_SINCE_1948_Dates[0] = initializeDate(1948, 9, 1);
  CPI_SINCE_1948_Dates[1] = initializeDate(1948, 12, 1);
  int year = 1949;
  for (int i = 2; i < (CPI_SIZE - 4); i += 4) {
    for (int q = 1; q <= 4; ++q) {
      CPI_SINCE_1948_Dates[i + q - 1] = initializeDate(year, 3 * q, 1);
    }
    ++year;
  }
  nCPI_QUARTERS = N;
  if (verbose) {
    Rprintf("First 3 elements:\n");
    Rprintf("\t %d %d %d\n", Q10_CPI_SINCE_1948[0], Q10_CPI_SINCE_1948[1], Q10_CPI_SINCE_1948[2]);
    Rprintf("Last 3 elements:\n");
    Rprintf("\t %d %d %d\n", Q10_CPI_SINCE_1948[N - 3], Q10_CPI_SINCE_1948[N - 2], Q10_CPI_SINCE_1948[N - 1]);
    Rprintf("\t nCPI_QUARTERS = %d\n", nCPI_QUARTERS);
    Rprintf("max_year = %d\n", year);
    Rprintf("complete\n");
  }
  cpi_prepared = true;
}

SEXP C_update_cpi(SEXP x, SEXP Verbose) {
  if (!isReal(x)) {
    error("Expected the value of CPI provided in the update to be a numeric vector"
            " but x was type '%s'.", type2char(TYPEOF(x)));
  }
  int N = length(x);
  if (N >= CPI_SIZE) {
    error("`length(x) == %d which exceeds %d, the maximum allocated size for the CPI array. ",
          N, CPI_SIZE + 1);
  }
  const double * xp = REAL(x);
  const bool verbose = asLogical(Verbose);
  update_cpi(xp, verbose, N);
  return x;
}

SEXP print_Q10_CPI_SINCE_1948() {
  for (int i = 0; i < 300; ++i) {
    Rprintf("%d, ", Q10_CPI_SINCE_1948[i]);
    if ((i % 8) == 0) {
      Rprintf("\n");
    }
  }
  return R_NilValue;
}

static void fy_cpi_inflator(double * restrict ansp,
                            const int * from,
                            R_xlen_t N_from,
                            const int * to,
                            R_xlen_t N_to,
                            int nThread) {
  R_xlen_t N = N_from >= N_to ? N_from : N_to;
  if (N_from == N_to) {
    FORLOOP({
      double a = Q10_CPI_SINCE_1948[Year2CPI_idx(from[i])];
      double b = Q10_CPI_SINCE_1948[Year2CPI_idx(to[i])];
      ansp[i] = b / a;
    })
  } else if (N_from == 1) {
    double a = Q10_CPI_SINCE_1948[Year2CPI_idx(from[0])];
    FORLOOP({
      double b = Q10_CPI_SINCE_1948[Year2CPI_idx(to[i])];
      ansp[i] = b / a;
    })
  } else {
    double b = Q10_CPI_SINCE_1948[Year2CPI_idx(to[0])];
    FORLOOP({
      double a = Q10_CPI_SINCE_1948[Year2CPI_idx(from[i])];
      ansp[i] = b / a;
    })
  }
}

SEXP C_cpi_inflator(SEXP From, SEXP To, SEXP FromClass, SEXP ToClass) {
  prohibit_vector_recyling(From, To, "from", "to");
  R_xlen_t N_from = xlength(From);
  R_xlen_t N_to = xlength(To);
  R_xlen_t N = N_from >= N_to ? N_from : N_to;

  int from_class = asInteger(FromClass);
  int to_class = asInteger(ToClass);
  if (from_class == 1 && to_class == 1) {
    SEXP ans = PROTECT(allocVector(REALSXP, N));
    double * restrict ansp = REAL(ans);
    fy_cpi_inflator(ansp, INTEGER(From), xlength(From), INTEGER(To), xlength(To), 1);
    UNPROTECT(1);
    return ans;
  }


  YearMonth * FromDate = malloc(sizeof(YearMonth) * N_from);
  YearMonth * ToDate = malloc(sizeof(YearMonth) * N_to);
  if (FromDate == NULL || ToDate == NULL) {
    free(FromDate);
    free(ToDate);
    error("Could not malloc.");
  }
  int nThread = 1;
  int MonthFY = 6;
  SEXP ans = PROTECT(allocVector(REALSXP, N));
  double * restrict ansp = REAL(ans);
  SEXP2YearMonth(FromDate, From, true, true, MonthFY, false, "from", nThread);
  SEXP2YearMonth(ToDate, To, true, true, MonthFY, false, "to", nThread);

  if (N_from == N_to) {
    FORLOOP({
      ansp[i] = cpi_YearMonth_inflator(FromDate[i], ToDate[i]);
    })
  } else if (N_from == 1) {
    FORLOOP({
      ansp[i] = cpi_YearMonth_inflator(FromDate[0], ToDate[i]);
    })
  } else if (N_to == 1) {
    FORLOOP({
      ansp[i] = cpi_YearMonth_inflator(FromDate[i], ToDate[0]);
    })
  } else {
    warning("N_from/N_to prohibit vector recyling."); // # nocov
  }

  free(FromDate);
  free(ToDate);
  UNPROTECT(1);
  return ans;
}


