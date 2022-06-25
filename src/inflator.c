#include "grattanInflator.h"

static double inflator(int from, int to,
                       const int * xp,
                       const int * yp,
                       int n) {
  // xp is sorted so a binary search is more principled
  // but is unlikely to be actually faster than a linear
  // search for the data we're using (c. 100 elements)
  int y_from = yp[0];
  int y_to = yp[0];
  if (from > xp[0]) {
    int x_from = n - 1;
    while (from < xp[x_from]) {
      --x_from;
    }
    y_from = yp[x_from];
  }
  if (to > xp[0]) {
    int x_to = n - 1;
    while (to < xp[x_to]) {
      --x_to;
    }
    y_to = yp[x_to];
  }
  return ((double)y_to) / ((double)y_from);
}

static double inflator_dbl(int from, int to,
                           const int * xp,
                           const double * yp,
                           int n) {
  // xp is sorted so a binary search is more principled
  // but is unlikely to be actually faster than a linear
  // search for the data we're using (c. 100 elements)
  double y_from = yp[0];
  double y_to = yp[0];
  if (from > xp[0]) {
    int x_from = n - 1;
    while (from < xp[x_from]) {
      --x_from;
    }
    y_from = yp[x_from];
  }
  if (to > xp[0]) {
    int x_to = n - 1;
    while (to < xp[x_to]) {
      --x_to;
    }
    y_to = yp[x_to];
  }
  return ((double)y_to) / ((double)y_from);
}

double inflator_Date_dbl(Date from, Date to,
                                Date * xp,
                                const double * yp,
                                int n) {
  // xp is sorted so a binary search is more principled
  // but is unlikely to be actually faster than a linear
  // search for the data we're using (c. 100 elements)
  double y_from = yp[0];
  double y_to = yp[0];
  if (Date_gt_Date(from, xp[0])) {
    int x_from = n - 1;
    while (Date_gt_Date(xp[x_from], from)) {
      --x_from;
    }
    y_from = yp[x_from];
  }
  if (Date_gt_Date(to, xp[0])) {
    int x_to = n - 1;
    while (Date_gt_Date(xp[x_to], to)) {
      --x_to;
    }
    y_to = yp[x_to];
  }
  return ((double)y_to) / ((double)y_from);
}

double inflator_Date_u16(Date from, Date to,
                         Date * xp,
                         uint16_t * yp,
                         int n) {
  // xp is sorted so a binary search is more principled
  // but is unlikely to be actually faster than a linear
  // search for the data we're using (c. 100 elements)
  double y_from = yp[0];
  double y_to = yp[0];
  if (Date_gt_Date(from, xp[0])) {
    int x_from = n - 1;
    while (Date_gt_Date(xp[x_from], from)) {
      --x_from;
    }
    y_from = yp[x_from];
  }
  if (Date_gt_Date(to, xp[0])) {
    int x_to = n - 1;
    while (Date_gt_Date(xp[x_to], to)) {
      --x_to;
    }
    y_to = yp[x_to];
  }
  return ((double)y_to) / ((double)y_from);
}





