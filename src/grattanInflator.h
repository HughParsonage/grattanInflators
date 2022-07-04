#ifndef grattanInflator_H
#define grattanInflator_H

#if _OPENMP
#include <omp.h>
#define AS_NTHREAD int nThread = as_nThread(nthreads);
#else
#define AS_NTHREAD
#endif

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>
#include <Rversion.h>
#include <stdint.h> // for uint64_t rather than unsigned long long
#include <stdbool.h>
#include <math.h>
#include <ctype.h>
#if defined _OPENMP && _OPENMP >= 201511
#define FORLOOP(content) do {                                           \
_Pragma("omp parallel for num_threads(nThread)")                        \
  for (R_xlen_t i = 0; i < N; ++i) {                                    \
    content;                                                            \
  }                                                                     \
} while (0);                                                            \

#else
#define FORLOOP(content) do {                                       \
for (R_xlen_t i = 0; i < N; ++i) {                                  \
  content;                                                          \
}                                                                   \
} while (0);
#endif



#define MAX_NBRACK 8
#define MAX_OFFSETN 15
#define MAX_N_OFFSETN 15
#define MIN_YEAR 1984
#define MAX_YEAR 2030
#define NA_INT -2147483648
#define TEMP_BUDGET_REPAIR_LEVY_THRESH 180000
#define TEMP_BUDGET_REPAIR_LEVY_RATE 0.02
#ifndef NaN
  #define NaN NAN
#endif



// 0-127 with months
typedef struct {
  unsigned int years : 7;
  unsigned int month : 4;
} Age;

typedef struct {
  unsigned int year : 7;
  unsigned int month : 4;
  unsigned int day : 5;
} Date;


typedef struct {
  unsigned int year : 7;
  unsigned int month : 4;
} YearMonth;

typedef struct {
  unsigned int year : 7;
  unsigned int qtr : 2;
} YearQtr;

typedef struct {
  unsigned int year : 7;
} Year;


// validate dates are 1948-07-01 to 2075-12-31 (approximately 127 years)
#define MAX_IDATE 38715
#define MIN_IDATE -7854
#define NEG_MIN_IDATE 7854
#define RANGE_IDATE 46570

#define CLASS_FY 1
#define CLASS_Date 2
#define CLASS_IDate 3
#define CLASS_integer 4
#define CLASS_character 5

#define ADJUSTMENT_ORIG 0
#define ADJUSTMENT_SEAS 1
#define ADJUSTMENT_TREN 2
#define ADJUSTMENT_TRIM 3

#define CPI_ORIG 23258472
#define CPI_SEAS 36045075
#define CPI_TRIM 36045111
#define LFI_ORIG 844230860
#define LFI_SEAS 844230442
#define LFI_TREN 844231291
#define WPI_ORIG 26036109
#define WPI_SEAS 27138502
#define WPI_TREN 27138537




// ensure_date.c
int string2year(const char * x);
int valid_form(const char * x, int n, bool check_day, bool prefer_fy) ;
void character2dates(Date * dates, R_xlen_t N, int nThread, int choose_fy, const SEXP * xp);
YearMonth idate2YearMonth(int x) ;
Date initializeDate(int year, int month, int day);
void SEXP2YearMonth(YearMonth * ansp,
                    SEXP x,
                    int x_class,
                    bool constant_only, bool prefer_fy,
                    int fy_month,
                    bool check_day, const char * var, int nThread);

// errif.c
void prohibit_vector_recyling(SEXP x, SEXP y, const char * wx, const char * wy);

// omp-diagnose.c
int as_nThread(SEXP x);

#endif