

.check_input <- function(x, minDate, maxDate, check = 1L, nThread = 1L, var = NULL) {
  if (is.null(var)) {
    var <- deparse1(eval.parent(substitute(substitute(x))))
  }
  x <- ensure_date(x)
  .Call("C_check_input", x, var, check, supported_classes(class(x)), minDate, maxDate, nThread, PACKAGE = packageName())
}

varname <- function(x, vx, n = 1) {
  sx <- eval.parent(substitute(substitute(x)), n = n)
  if (is.symbol(sx)) {
    return(deparse1(sx))
  }

  # from_vname
  vx
}
