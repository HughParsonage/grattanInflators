

.check_input <- function(x, minDate, nThread = 1L) {
  var <- as.character(eval.parent(substitute(substitute(x))))
  .Call("C_check_input", x, var, supported_classes(class(x)), minDate, nThread, PACKAGE = packageName())
}
