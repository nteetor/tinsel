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
  if (.sym$NUMBER == symbol) {
    symbolmsg <- 'a numeric character'
  } else if (.sym$LETTER == symbol) {
    symbolmsg <- 'an alphabetic character'
  } else if (.sym$FILENAME_CHAR == symbol) {
    symbolmsg <- 'a character allowed in a file name' # bleh
  } else if (.sym$SYNTACTIC_CHAR == symbol) {
    symbolmsg <- 'an alphanumeric character, "_", or "."'
  } else {
    symbolmsg <- paste0('"', symbol, '"')
  }

  msg <- sprintf('found "%s" on line %i, expected %s', actual, lineno, symbolmsg)
  condition('expected', msg, symbol = symbol, actual = actual, lineno = lineno,
            call. = FALSE, ...)
}
