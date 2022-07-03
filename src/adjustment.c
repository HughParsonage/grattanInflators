#include "grattanInflator.h"

int char2adjustment(SEXP x) {
  const char * xp = CHAR(STRING_ELT(x, 0));
  switch(xp[0]) {
  case 'o':
    return ADJUSTMENT_ORIG;
  case 'n':
    return ADJUSTMENT_ORIG;
  case 's':
    return ADJUSTMENT_SEAS;
  case 't':
    switch(xp[2]) {
    case 'i':
      return ADJUSTMENT_TRIM;
    case 'e':
      return ADJUSTMENT_TREN;
    }
  }
  return ADJUSTMENT_ORIG;
}
