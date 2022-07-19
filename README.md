grattanInflators
----------------

Utility package for CPI and other inflators.

### Benchmarks

``` r
x <- rep_len(fy::yr2fy(1999:2020), 1e7)

bench::system_time(grattan::cpi_inflator(, x, "2019-20"))
```

    ## process    real 
    ##   453ms   346ms

``` r
bench::system_time(grattanInflators::cpi_inflator(x, "2019-20"))
```

    ## process    real 
    ##   375ms   440ms

``` r
y <- dqrng::dqsample(x)
bench::system_time(grattan::cpi_inflator(, x, y))
```

    ## process    real 
    ##   23.2s   20.2s

``` r
bench::system_time(grattanInflators::cpi_inflator(x, y))
```

    ## process    real 
    ##   375ms   378ms
