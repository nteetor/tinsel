condition <- function(subclass, msg, call = sys.call(-1), ...) {
  structure(
    list(
      message = msg,
      call = call,
      ...
    ),
    class = c(subclass, 'condition')
  )
}

expected <- function(symbol, actual, lineno = 1) {
  msg <- sprintf('unexpected character %s on line %i', actual, lineno)
  condition('expected', msg, symbol = symbol, actual = actual, lineno = lineno)
}
