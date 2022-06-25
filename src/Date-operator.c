#include "grattanInflator.h"

bool Date_gt_Date(Date x, Date y) {
  if (x.year > y.year) {
    return true;
  }
  if (x.year < y.year) {
    return false;
  }
  if (x.month > y.month) {
    return true;
  }
  if (x.month < y.month) {
    return false;
  }
  return (x.month == y.month && x.day > y.day);
}

