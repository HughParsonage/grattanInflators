#' Wage inflator
#' @description Uses the Wage Price Index
#'
#' @param from,to Times for which the inflator is desired.
#'
#' @export
wage_inflator <- function(from, to) {
  Index <- GET_SERIES(wpi2series_id("trend"))
  Inflate(from, to, Index)
}

wpi2series_id <- function(adjustment) {
  switch(adjustment,
         "original" = "A2603609J",
         "seasonal" = "A2713849C",
         "trend" = "A2713851R",
         "A2713851R")
}
