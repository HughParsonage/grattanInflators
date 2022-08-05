grattanInflators
----------------

Utility package for CPI and other inflators.

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
