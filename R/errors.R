condition <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    list(
      message = message,
      call = call
    ),
    class = c(subclass, 'error', 'condition'),
    ...
  )
}

expected <- function(symbol, actual, lineno = 1, ...) {
  msg <- sprintf('found "%s" on line %i, expected "%s"', actual, lineno, symbol)
  condition('expected', msg, symbol = symbol, actual = actual, lineno = lineno,
            call. = FALSE, ...)
}
