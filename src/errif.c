#include "grattanInflator.h"

void prohibit_vector_recyling(SEXP x, SEXP y, const char * wx, const char * wy) {
  if (xlength(x) == xlength(y)) {
    return;
  }
  if (xlength(x) == 1 || xlength(y) == 1) {
    return;
  }
  error("`length(%s) = %lld`, yet `length(%s) = %lld`. "
          "Vectors must be of equal length, or length-one.",
          wx, (long long)xlength(x), wy, (long long)xlength(y));
}
