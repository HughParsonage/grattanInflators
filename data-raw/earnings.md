# Preparing AWE and AWOTE data

Install the current package checkout first (`R CMD INSTALL .`), then run from
the package root. Preparation reuses its series-ID registry, catalogue URLs,
TSV reader and index validation.

The default source is readabs, which discovers the latest ABS release and reads
the requested series without assumptions about workbook rows, sheets or column
positions. It bypasses readabs's local cache. readabs is needed only to prepare
data directly from ABS, not to use the installed inflators.

```sh
Rscript data-raw/earnings.R
```

Alternatively, use the same ABS-Catalogue mirror as the package's runtime
updates. This needs no readabs installation. The catalogue itself is built using
readabs. Its default Git reference is `master`; pass a commit SHA to reproduce a
particular snapshot.

```sh
Rscript data-raw/earnings.R catalogue
Rscript data-raw/earnings.R catalogue 70a6aabce78741b392456d86c2b14d5efd259f78
```

Both routes fetch and validate all six series before copying any files into
`inst/extdata`. Invalid data or a failed download leave the bundled files
untouched. The printed summary reports the source, observation counts and date
ranges. If a later release changes the bundled period or published levels,
update the snapshot descriptions in NEWS, README and help, and the corresponding
reference values in `test_awe_inflator.R`.

For comparisons without changing bundled data, source the script and supply
separate output directories:

```r
source("data-raw/earnings.R")
prepare_earnings("readabs", output_dir = tempfile("abs-"))
prepare_earnings("catalogue", output_dir = tempfile("catalogue-"))
```

Sources: [readabs](https://github.com/MattCowgill/readabs) and
[ABS-Catalogue](https://github.com/HughParsonage/ABS-Catalogue).
