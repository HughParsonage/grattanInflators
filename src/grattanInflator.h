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
#include <stdint.h> // for uint64_t rather than unsigned long long, intptr
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




#define MIN_YEAR 1948
#define MAX_YEAR 2075
#define NA_INT -2147483648
#ifndef NaN
  #define NaN NAN
#endif



typedef struct {
  unsigned int year : 7;
  unsigned int month : 4;
} YearMonth;






// validate dates are 1948-01-01 to 2075-12-31 (approximately 127 years)
#define MAX_IDATE 38715
#define MIN_IDATE -8036
#define NEG_MIN_IDATE 8036
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

#define ERR_BADFORM 1
#define ERR_IDATE_OUT_OF_RANGE 2


int string2month(const char * x);

// check_input

// YearMonth
YearMonth idate2YearMonth(int x);
uint16_t year(int x) ;
unsigned int p_search(int x);
int ymi(YearMonth YM);
int yqi(YearMonth YM);


// errif.c
void prohibit_vector_recyling(SEXP x, SEXP y, const char * wx, const char * wy);

// minmaxDate.c
void err_if_anyOutsideDate(int minmax[2], SEXP x, int nThread, const char * var, bool was_date);

// omp-diagnose.c
int as_nThread(SEXP x);

// SEXP2YearMonth.c
int string2year(const char * x);
void SEXP2YearMonth(YearMonth * ansp,
                    SEXP x,
                    int x_class,
                    int fy_month,
                    bool check_day, const char * var, int nThread) ;

#endif
