## R CMD check results

0 errors | 0 warnings | 0 note

* This is a package update. It fixes several out-of-bounds reads and writes in
  the compiled code (a short `x` written past its end, index positions used
  without a bounds check, element zero read from zero-length inputs, and a
  month-name/month-zero underflow in the fast date parser), removes R API calls
  from OpenMP worker loops, and reconciles the input checker with the date
  converter so that the two agree on financial years, quarters and four-digit
  years.

* The test suite no longer skips itself when the mirrored ABS data is
  unavailable: the parser, native-safety and synthetic-index tests now run
  regardless, so the compiled code is exercised under the sanitizers. The tests
  that do need the ABS data skip individually, and the exported inflators still
  return `NULL` with a message when no data can be obtained, so checks on a
  machine without internet access pass as before.

* Checked locally under gcc ASAN/UBSAN with 1 and 4 threads: no diagnostics.
