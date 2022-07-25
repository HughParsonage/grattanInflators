grattanInflators
----------------

Utility package for CPI and other inflators.

### Benchmarks

``` r
x <- rep_len(fy::yr2fy(1999:2020), 1e7)

system_time(grattan::cpi_inflator(, x, "2019-20"))
```

    ## process    real 
    ##   578ms   338ms

``` r
system_time(cpi_inflator(x, "2019-20"))
```

    ## process    real 
    ##   312ms   328ms

``` r
system_time(cpi_inflator(x, "2019-20", nThread = 4L))
```

    ## process    real 
    ## 296.9ms  84.8ms

``` r
y <- dqrng::dqsample(x)
system_time(grattan::cpi_inflator(, x, y))
```

    ## process    real 
    ##   21.7s   18.5s

``` r
system_time(cpi_inflator(x, y, nThread = 4L))
```

    ## process    real 
    ##   594ms   180ms

``` r
x <- rep_len(x, 1e8)
system_time(cpi_inflator(x, "2019-20", nThread = 4L))
```

    ##  process     real 
    ##    2.81s 769.14ms

``` r
x <- y <- NULL
```

``` r
x <- seq(as.Date("1999-01-01"), as.Date("2020-01-01"), by = "1 day")
x <- rep_len(x, 1e7)
system_time(cpi_inflator(x, "2019-01-01"))
```

    ## process    real 
    ##   266ms   264ms

``` r
x <- rep_len(x, 1e8)
system_time(cpi_inflator(x, "2019-01-01", nThread = 4L))
```

    ## process    real 
    ##   3.42s   1.29s

``` r
x <- as.IDate(x)
system_time(cpi_inflator(x, as.IDate("2019-01-01"), nThread = 4L))
```

    ##  process     real 
    ##    2.31s 669.83ms
