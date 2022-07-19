#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP C_all_Y2k(SEXP);
extern SEXP C_anyPrior(SEXP, SEXP);
extern SEXP C_check_input(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP C_fastIDate(SEXP, SEXP);
extern SEXP C_Inflate(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP C_Inflate2(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP C_inflate4(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP C_minDate(SEXP);
extern SEXP C_Year(SEXP, SEXP);
extern SEXP C_YearMonthSplit(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"C_all_Y2k",        (DL_FUNC) &C_all_Y2k,         1},
    {"C_anyPrior",       (DL_FUNC) &C_anyPrior,        2},
    {"C_check_input",    (DL_FUNC) &C_check_input,     5},
    {"C_fastIDate",      (DL_FUNC) &C_fastIDate,       2},
    {"C_Inflate",        (DL_FUNC) &C_Inflate,        12},
    {"C_Inflate2",       (DL_FUNC) &C_Inflate2,        6},
    {"C_inflate4",       (DL_FUNC) &C_inflate4,        6},
    {"C_minDate",        (DL_FUNC) &C_minDate,         1},
    {"C_Year",           (DL_FUNC) &C_Year,            2},
    {"C_YearMonthSplit", (DL_FUNC) &C_YearMonthSplit,  4},
    {NULL, NULL, 0}
};

void R_init_grattanInflators(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
