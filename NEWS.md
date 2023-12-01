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
