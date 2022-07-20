grattanInflators
----------------

Utility package for CPI and other inflators.

### Benchmarks

``` r
x <- rep_len(fy::yr2fy(1999:2020), 1e7)

bench::system_time(grattan::cpi_inflator(, x, "2019-20"))
```

    ## process    real 
    ##   609ms   452ms

``` r
bench::system_time(grattanInflators::cpi_inflator(x, "2019-20"))
```

    ## process    real 
    ##   281ms   290ms

``` r
y <- dqrng::dqsample(x)
bench::system_time(grattan::cpi_inflator(, x, y))
```

    ## process    real 
    ##   24.8s   20.9s

``` r
bench::system_time(grattanInflators::cpi_inflator(x, y))
```

    ## process    real 
    ##   438ms   433ms
