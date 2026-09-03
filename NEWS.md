## grattanInflators 0.6.0

### Bug fixes (memory safety)

* `Inflate(x = )` no longer writes past the end of `x` when `x` is shorter than
  `from`, and no longer requires `x` to be a double: the length is checked in
  both R and C, and an integer `x` is coerced (and so must be read back from
  the return value).
* The monthly, quarterly and annual kernels now bounds-check every index
  position before reading it. An out-of-range date gives `NaN`. This check is
  unconditional: `check = 0` suppresses diagnostics, never bounds checks.
* Zero-length inputs no longer read element zero of an empty vector.
  `fast_as_idate()`, `Inflate()` and the internal `Year()`/`YearMonthSplit()`
  return empty results.
* `fast_as_idate()` no longer indexes before the start of its month table when
  the month component is zero or a month name is unrecognised (e.g.
  `"01/00/2024"`, `"01XYZ1948"`).
* Loops that call the R API (`CHAR()`, `length()`) are no longer run under
  OpenMP, as most of the R API is not thread-safe.
* Dates in December 2075, the last month of the supported range, are no longer
  reported as November: the binary search over the month table could not return
  its final element. `format_idate()` produced impossible strings such as
  `"2075-11-61"`.

### Bug fixes (dates)

* Years are now parsed from all four digits. `"2999-01-01"` was silently read
  as 1999; it is now rejected (or `NaN` when `check = 0`).
* A financial year now means the same thing to the input checker and to the
  converter. Previously, for `fy_month >= 7`, the two disagreed by one calendar
  year.
* An `<fy>` object now means the same thing as the equivalent `"YYYY-YY"`
  string. Previously it lost its class during conversion and was read as
  January of the ending year, whatever `fy_month` was.
* A quarter (`"2015-Q1"`) is now the last month of that quarter everywhere.
  The checker used Mar/Jun/Sep/Dec and the converter Feb/May/Aug/Nov, which
  differ on a monthly index.
* The input checker now accepts exactly the strings the converter can read: a
  string of any other length (such as `"2020"`), or with arbitrary bytes in the
  separator positions, is rejected rather than passed and then silently
  converted to `NaN`.
* A fractional year (`cpi_inflator(2015.9, 2016)`) is an error rather than
  being truncated to 2015.

### Bug fixes (`fast_as_idate`)

* `format = "%d-%m-%Y"` is now supported, as announced in 0.5.2.
  `guess_format()` could return it while the parser did not recognise it and
  read the date year-first instead.
* An unrecognised `format` is now an error rather than being read as
  `"%Y-%m-%d"`.
* `guess_format()` now recognises day-first dates beginning 30 or 31, and
  reports the separator actually used.
* `check` now applies to every format, not only `"%Y-%m-%d"`.

### Bug fixes (custom series and forecasting)

* `dr2index()` no longer skips the first period after the end of the published
  series, and no longer adds a period beyond the requested date.
* `.next_date()` no longer produces a thirteenth month for a quarterly series
  anchored in October or November, and clamps the day of the month rather than
  producing an invalid date.
* `r2index()` no longer errors with "object 'next_date' not found".
* A trailing rate (`cpi_original(2030, 0.1, 0.05)`) is now dispatched
  correctly.
* Rates given as strings keep their sign and exponent: `"-5%"` was read as
  `+5%` and `"1e-2"` as `12`. Rates are now validated against a strict grammar
  and a rate of -100% or less is rejected.
* The non-`fable` fallback extension built one more date than value for annual
  series (producing a recycling warning and a wrong final row), and used
  quarterly arithmetic for monthly series. Both are fixed.
* The `fable` path restores the `data.table` thread count with `on.exit()`, so
  a modelling error no longer leaves it changed.

### Other changes

* All CPI, wage-price and labour-force series are now bundled with the package,
  so every inflator works immediately after installation without an internet
  connection. `download_data()` continues to store newer copies in the user
  data directory. A downloaded copy takes precedence only when its observation
  coverage is at least as recent as the bundled snapshot, so a cache left by an
  older package version cannot mask newer bundled data.
* `index` is now validated before use: it must have `date` and `value` columns,
  at least two rows, strictly increasing dates forming a regular annual,
  quarterly or monthly sequence, and finite non-zero values. The native code
  computes a row offset arithmetically, so an irregular or unsorted index would
  otherwise be read at the wrong position.
* A downloaded series is fully parsed and validated before it replaces the
  cached file, the replacement is a same-filesystem rename (with the previous
  version kept alongside as `.bak`), and the in-memory copy of a refreshed
  series is dropped so that `download_data()` takes effect within a session.
* `GET_SERIES_FY()` no longer adds an `fy` column by reference to the cached
  series.
* The test suite now runs the exported inflators on every platform, including
  machines without internet access, using the bundled ABS data.
* Removed a unit test that had been gated on a date now past, and so had
  stopped running.

## grattanInflators 0.5.7

* Bug fix:
  - Forecast defaults no longer exceed the permissible date range

## grattanInflators 0.5.6

* Bug fix:
  - `fast_as_idate(x, format = "%d/%m/%Y")` now works with nchar(x) != 10, 
    e.g. "1/1/2000"

## grattanInflators 0.5.5

* Internal
  - Checking can occur without error in the absence of an internet connection
    (though the tests will be perfunctory)

## grattanInflators 0.5.4

* Internal
  - `STRING_PTR` now `STRING_PTR_RO`

## grattanInflators 0.5.3

* Fixed a time-dependent unit test

## grattanInflators 0.5.2

### Internal

* Fixed a test failing on CRAN due to fall-back to narrower date series
* `fast_as_idate` now parses `%d%b%Y` and `%d-%m-%Y` type dates

## grattanInflators 0.5.1

### Bug fixes
* `cpi_` functions did not pass down `FORECAST` argument; `wage_`, `lfi_` did not
  pass down `series` argument
* `fy_month` now factored into character strings when determining check
* More error messages and warnings now refer to the calling functions.

## grattanInflators 0.5.0

* `download_data` now downloads/updates monthly CPI data

### Internal
* Fix -Wformat warnings from CRAN about `R_xlen_t` printing.

## grattanInflators 0.4.2

* Fixed an error for out-of-date example

## grattanInflators 0.4.0

* Added a `NEWS.md` file to track changes to the package.

### Features

* Quarters in the form of YYYY.Q[1-4] are now recognized
* Series can now be accessed and modified by e.g. `cpi_original(...)`.
* Series are automatically forecast
