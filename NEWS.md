## grattanInflators 0.5.1

### Bug fixes
* `cpi_` functions did not pass down `FORECAST` argument; `wage_`, `lfi_` did not
  pass down `series` argument
* `fy_month` now factored into character strings when determining check

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
