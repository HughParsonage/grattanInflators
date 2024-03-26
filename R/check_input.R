

.check_input <- function(x, minDate, maxDate, check = 1L, nThread = 1L, fy_month = 3L, var = NULL, xclass) {
  if (is.null(var)) {
    var <- deparse1(eval.parent(substitute(substitute(x))))
  }
  .Call("C_check_input", x, var, check, xclass, minDate, maxDate, nThread, fy_month, PACKAGE = packageName())
}

varname <- function(x, vx, n = 1) {
  sx <- eval.parent(substitute(substitute(x)), n = n)
  if (is.symbol(sx)) {
    return(deparse1(sx))
  }

  # from_vname
  vx
}
