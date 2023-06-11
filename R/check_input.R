

.check_input <- function(x, minDate, maxDate, check = 1L, nThread = 1L) {
  var <- deparse1(eval.parent(substitute(substitute(x))))
  x <- ensure_date(x)
  .Call("C_check_input", x, var, check, supported_classes(class(x)), minDate, maxDate, nThread, PACKAGE = packageName())
}
