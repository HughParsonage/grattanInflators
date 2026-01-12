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
