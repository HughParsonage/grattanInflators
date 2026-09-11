grattanInflators
----------------

Utility package for CPI and other inflators.

### Average weekly earnings

`awe_inflator()` uses all employees' average weekly total earnings;
`awote_inflator()` uses full-time adults' average weekly ordinary time earnings.
Both cover persons, Australia. Original, seasonally adjusted and trend data are
available through `awe_original()`, `awe_seasonal()`, `awe_trend()` and the
corresponding `awote_*()` functions.

```r
awe <- awe_original()  # date and value (dollars per week)
awote_inflator("2024-05-15", "2025-05-15")
awe_inflator("2024-05-15", "2025-05-15", series = awe_seasonal())
awote_inflator("2030-05-15", "2031-05-15", series = awote_original("3%"))
```

The [ABS May 2026 release](https://www.abs.gov.au/statistics/labour/earnings-and-working-conditions/average-weekly-earnings-australia/may-2026)
is bundled for offline use; `download_data()` refreshes it through the package's
ABS mirror. Original series begin in November 1994; adjusted and trend series
begin in May 2012. Inflators use May observations for May to October and November
observations for November to April, with the usual endpoint checks. Earnings
changes include workforce composition effects; `wage_inflator()` uses the WPI.

### Benchmarks

``` r
x <- rep_len(fy::yr2fy(1999:2020), 1e7)

system_time(grattan::cpi_inflator(, x, "2019-20"))
```

    ## process    real 
    ##   531ms   482ms

``` r
system_time(cpi_inflator(x, "2019-20"))
```

    ## process    real 
    ##   438ms   439ms

``` r
system_time(cpi_inflator(x, "2019-20", nThread = 4L))
```

    ## process    real 
    ##   391ms   114ms

``` r
y <- dqrng::dqsample(x)
system_time(grattan::cpi_inflator(, x, y))
```

    ## process    real 
    ##   24.3s   21.1s

``` r
system_time(cpi_inflator(x, y, nThread = 4L))
```

    ## process    real 
    ##   984ms   241ms

``` r
x <- rep_len(x, 1e8)
system_time(cpi_inflator(x, "2019-20", nThread = 4L))
```

    ## process    real 
    ##   4.61s   1.25s

``` r
x <- y <- NULL
```

``` r
x <- seq(as.Date("1999-01-01"), as.Date("2020-01-01"), by = "1 day")
x <- rep_len(x, 1e7)
system_time(cpi_inflator(x, "2019-01-01"))
```

    ## process    real 
    ##   297ms   311ms

``` r
x <- rep_len(x, 1e8)
system_time(cpi_inflator(x, "2019-01-01", nThread = 4L))
```

    ## process    real 
    ##   3.67s   1.29s

``` r
x <- as.IDate(x)
system_time(cpi_inflator(x, as.IDate("2019-01-01"), nThread = 4L))
```

    ##  process     real 
    ##    2.66s 905.37ms
