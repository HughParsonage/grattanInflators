grattanInflators
----------------

Utility package for CPI and other inflators.

### Benchmarks

``` r
x <- rep_len(fy::yr2fy(1999:2020), 1e7)

bench::system_time(grattan::cpi_inflator(, x, "2019-20"))
```

    ## process    real 
    ##   438ms   348ms

``` r
bench::system_time(cpi_inflator(x, "2019-20"))
```

    ## process    real 
    ##   281ms   277ms

``` r
bench::system_time(cpi_inflator(x, "2019-20", nThread = 4L))
```

    ## process    real 
    ## 265.6ms  81.6ms

``` r
y <- dqrng::dqsample(x)
bench::system_time(grattan::cpi_inflator(, x, y))
```

    ## process    real 
    ##     23s   19.4s

``` r
bench::system_time(cpi_inflator(x, y, nThread = 4L))
```

    ## process    real 
    ##   469ms   153ms

``` r
x <- rep_len(x, 1e8)
bench::system_time(cpi_inflator(x, "2019-20", nThread = 4L))
```

    ##  process     real 
    ##    2.84s 975.77ms

``` r
x <- y <- NULL
```

``` r
x <- seq(as.Date("1999-01-01"), as.Date("2020-01-01"), by = "1 day")
x <- rep_len(x, 1e7)
bench::system_time(cpi_inflator(x, "2019-01-01"))
```

    ## process    real 
    ##   312ms   321ms

``` r
x <- rep_len(x, 1e8)
bench::system_time(cpi_inflator(x, "2019-01-01", nThread = 4L))
```

    ## process    real 
    ##   3.86s   1.58s
