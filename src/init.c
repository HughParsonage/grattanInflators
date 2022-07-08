#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP C_fastIDate(SEXP, SEXP);
extern SEXP C_Inflate(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP C_Inflate2(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP C_ValidateDate(SEXP, SEXP);
extern SEXP C_Year(SEXP, SEXP);
extern SEXP C_YearMonthSplit(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"C_fastIDate",      (DL_FUNC) &C_fastIDate,       2},
    {"C_Inflate",        (DL_FUNC) &C_Inflate,        12},
    {"C_Inflate2",       (DL_FUNC) &C_Inflate2,        6},
    {"C_ValidateDate",   (DL_FUNC) &C_ValidateDate,    2},
    {"C_Year",           (DL_FUNC) &C_Year,            2},
    {"C_YearMonthSplit", (DL_FUNC) &C_YearMonthSplit,  4},
    {NULL, NULL, 0}
};

void R_init_grattanInflators(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
