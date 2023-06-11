#' Wage inflator
#' @description Uses the Wage Price Index
#'
#' @param from,to Times for which the inflator is desired. If \code{NULL}, a date
#' range close to the previous year is used.
#' @param check \code{integer(1)} If \code{0L}, no checks are performed, and
#' clearly invalid inputs result in \code{NA} in the output. If \code{check = 1L}
#' an error is performed for bad input; \code{check = 2L} is more thorough.
#' @param x (Advanced) A vector that will be inflated in-place. If \code{NULL},
#' the default, the return vector is simply the inflation factor for `from`.
#'
#' @param nThread Number of threads to use.
#'
#' @return
#' If `x` is `NULL`, the default, a numeric vector matching the lengths of `from`
#' and `to` equal to the inflators by which nominal wages dated `from` must be
#' multiplied so that they are in `to` real terms.
#'
#' If `x` is numeric, it is taken to be wages dated `from` and the value returned
#' is `x` in `to` real terms.
#'
#' @export
wage_inflator <- function(from = NULL, to = NULL,
                          check = 1L,
                          x = NULL,
                          nThread = getOption("grattanInflators.nThread", 1L)) {
  Index <- GET_SERIES(wpi2series_id("original"))
  Inflate(from, to, index = Index,
          check = check,
          x = x,
          nThread = nThread)
}

wpi2series_id <- function(adjustment) {
  # nocov start
  switch(adjustment,
         "original" = "A2603609J",
         "seasonal" = "A2713849C",
         "trend" = "A2713851R",
         "A2713851R")
  # nocov end
}
