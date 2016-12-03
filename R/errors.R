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
#  symname <- names(which(.sym == symbol))

    if (.sym$NUMBER == symbol) {
      symbol <- 'a digit'
    } else if (.sym$LETTER == symbol) {
      symbol <- 'a letter'
    } else if (.sym$FILENAME_CHAR == symbol) {
      symbol <- 'a valid file name character'
    } else if (.sym$SYNTACTIC_CHAR == symbol) {
      symbol <- 'a letter, _, or .'
    }

  msg <- sprintf('found "%s" on line %i, expected "%s"', actual, lineno, symbol)
  condition('expected', msg, symbol = symbol, actual = actual, lineno = lineno,
            call. = FALSE, ...)
}
