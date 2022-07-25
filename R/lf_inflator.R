#' Labour force inflator
#' @description Uses the Labour Force Index
#'
#' @param from,to Times for which the inflator is desired.
#' @param check \code{integer(1)} If \code{0L}, no checks are performed, and
#' clearly invalid inputs result in \code{NA} in the output. If \code{check = 1L}
#' an error is performed for bad input; \code{check = 2L} is more thorough.
#'
#' @param nThread Number of threads to use.
#'
#' @export
lf_inflator <- function(from, to,
                        check = 1L,
                        x = NULL,
                        nThread = getOption("grattanInflators.nThread", 1L)) {
  Index <- GET_SERIES(lfi2series_id("original"))
  Inflate(from, to, index = Index,
          check = check,
          x = x,
          nThread = nThread)
}

lfi2series_id <- function(adjustment) {
  switch(adjustment,
         "original" = "A84423085A",
         "seasonal" = "A84423043C",
         "trend" = "A84423127L",
         "A84423127L")
}
